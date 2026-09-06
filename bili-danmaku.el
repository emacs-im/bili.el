;;; bili-danmaku.el --- In-memory native video danmaku -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Decode Bilibili's segmented protobuf pool and independently lay out normal
;; comments as ASS for video.el's native libass overlay.  No subprocesses or
;; temporary files are involved.  Advanced/code/BAS modes 7+ are ignored.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bili-api)
(require 'appkit-effect)

(declare-function video-player-live-p "video-runtime" (player))
(declare-function video-player-set-subtitles "video-runtime" (player text))

(defcustom bili-danmaku-enabled t
  "Whether newly opened Bilibili videos fetch and display danmaku."
  :type 'boolean :group 'bili)

(defcustom bili-danmaku-font-family "sans-serif"
  "Font family used by the native libass renderer."
  :type 'string :group 'bili)

(defcustom bili-danmaku-font-size 32
  "Normal danmaku font size on the 1280 by 720 ASS canvas.
Provider font sizes are scaled relative to their normal size of 25."
  :type '(integer :tag "Size (12–64)") :group 'bili)

(defcustom bili-danmaku-opacity 0.85
  "Danmaku opacity, from zero (transparent) to one (opaque)."
  :type 'number :group 'bili)

(defcustom bili-danmaku-display-area 0.5
  "Fraction of the screen height available for comments, from 0.1 to 1.
Scrolling and top comments start at the top; bottom comments start at the
bottom.  All modes share occupancy, including when their regions meet."
  :type 'number :group 'bili)

(defconst bili-danmaku--pool-byte-limit (* 32 1024 1024))
(defconst bili-danmaku--comment-limit 100000)

(cl-defstruct (bili-danmaku--cursor (:constructor bili-danmaku--cursor-create))
  "Bounded protobuf reader over a single immutable byte string."
  bytes (position 0) end)

(defun bili-danmaku--varint (cursor)
  "Read one unsigned 64-bit varint from CURSOR or reject malformed input."
  (let ((value 0) (shift 0) byte)
    (catch 'done
      (dotimes (index 10)
        (when (>= (bili-danmaku--cursor-position cursor)
                  (bili-danmaku--cursor-end cursor))
          (error "Truncated danmaku protobuf varint"))
        (setq byte (aref (bili-danmaku--cursor-bytes cursor)
                         (bili-danmaku--cursor-position cursor)))
        (cl-incf (bili-danmaku--cursor-position cursor))
        (when (and (= index 9) (> byte 1))
          (error "Danmaku protobuf varint exceeds 64 bits"))
        (setq value (logior value (ash (logand byte 127) shift)))
        (when (< byte 128) (throw 'done value))
        (setq shift (+ shift 7)))
      (error "Malformed danmaku protobuf varint"))))

(defun bili-danmaku--advance (cursor size)
  "Advance CURSOR by SIZE bytes, rejecting lengths outside its boundary."
  (let ((end (+ (bili-danmaku--cursor-position cursor) size)))
    (when (> end (bili-danmaku--cursor-end cursor))
      (error "Truncated danmaku protobuf field"))
    (setf (bili-danmaku--cursor-position cursor) end)))

(defun bili-danmaku--tag (cursor)
  "Read a valid protobuf field key from CURSOR."
  (let ((tag (bili-danmaku--varint cursor)))
    (unless (<= 1 (ash tag -3) #x1fffffff)
      (error "Invalid danmaku protobuf field number"))
    tag))

(defun bili-danmaku--skip (cursor tag &optional depth)
  "Skip unknown TAG at CURSOR, including matched groups up to DEPTH 16."
  (pcase (logand tag 7)
    (0 (bili-danmaku--varint cursor))
    (1 (bili-danmaku--advance cursor 8))
    (2 (bili-danmaku--advance cursor (bili-danmaku--varint cursor)))
    (3
     (when (>= (or depth 0) 16)
       (error "Danmaku protobuf nesting exceeds limit"))
     (catch 'end-group
       (while (< (bili-danmaku--cursor-position cursor)
                 (bili-danmaku--cursor-end cursor))
         (let ((child (bili-danmaku--tag cursor)))
           (if (= (logand child 7) 4)
               (if (= (ash child -3) (ash tag -3))
                   (throw 'end-group t)
                 (error "Mismatched danmaku protobuf group"))
             (bili-danmaku--skip cursor child (1+ (or depth 0))))))
       (error "Truncated danmaku protobuf group")))
    (5 (bili-danmaku--advance cursor 4))
    (_ (error "Invalid danmaku protobuf wire type"))))

(defun bili-danmaku--element (cursor)
  "Decode a DanmakuElem at CURSOR into [MS MODE SIZE RGB TEXT].
Only normal modes 1 through 6 are retained; advanced/code/BAS modes 7+
are intentionally ignored.  Unknown fields are skipped by their wire type."
  (let ((comment (vector 0 0 0 0 "")))
    (while (< (bili-danmaku--cursor-position cursor)
              (bili-danmaku--cursor-end cursor))
      (let* ((tag (bili-danmaku--tag cursor))
             (field (ash tag -3)) (wire (logand tag 7)))
        (cond
         ((memq field '(2 3 4 5))
          (unless (= wire 0) (error "Invalid danmaku numeric wire type"))
          (let ((value (bili-danmaku--varint cursor)))
            (when (> value #xffffffff)
              ;; Negative int32 values can use a sign-extended ten-byte varint.
              (unless (and (/= field 5) (>= value #xffffffff80000000))
                (error "Danmaku protobuf integer exceeds 32 bits")))
            (when (and (/= field 5) (/= 0 (logand value #x80000000)))
              (setq value (- (logand value #xffffffff) #x100000000)))
            (aset comment (- field 2) value)))
         ((= field 7)
          (unless (= wire 2) (error "Invalid danmaku text wire type"))
          (let* ((size (bili-danmaku--varint cursor))
                 (start (bili-danmaku--cursor-position cursor)))
            (when (> size 4096) (error "Danmaku comment text exceeds limit"))
            (bili-danmaku--advance cursor size)
            (let ((text (decode-coding-string
                         (substring (bili-danmaku--cursor-bytes cursor)
                                    start (+ start size)) 'utf-8)))
              (when (cl-some (lambda (char) (eq (char-charset char) 'eight-bit)) text)
                (error "Invalid UTF-8 in danmaku comment"))
              (aset comment 4 text))))
         (t (bili-danmaku--skip cursor tag)))))
    (when (and (<= 1 (aref comment 1) 6) (>= (aref comment 0) 0)
               (<= 0 (aref comment 3) #xffffff)
               (not (string-empty-p (aref comment 4))))
      comment)))

(defun bili-danmaku-decode-segment (bytes)
  "Decode bounded DmSegMobileReply BYTES into normal comment vectors.
The vector contract is [MILLISECONDS MODE FONT-SIZE RGB TEXT].  Empty
protobuf is an empty pool.  Malformed fields, UTF-8, lengths, and varints
signal errors; unknown fields are skipped, not interpreted as comments.
Advanced/code/BAS modes 7+ are intentionally ignored."
  (unless (and (stringp bytes) (not (multibyte-string-p bytes))
               (<= (length bytes) bili-api--response-limit))
    (error "Invalid or oversized danmaku protobuf bytes"))
  (let ((cursor (bili-danmaku--cursor-create :bytes bytes :end (length bytes)))
        (count 0) comments)
    (while (< (bili-danmaku--cursor-position cursor)
              (bili-danmaku--cursor-end cursor))
      (let ((tag (bili-danmaku--tag cursor)))
        (if (= (ash tag -3) 1)
            (progn
              (unless (= (logand tag 7) 2)
                (error "Invalid danmaku element wire type"))
              (when (> (cl-incf count) bili-danmaku--comment-limit)
                (error "Danmaku comment count exceeds limit"))
              (let* ((size (bili-danmaku--varint cursor))
                     (start (bili-danmaku--cursor-position cursor)))
                (bili-danmaku--advance cursor size)
                (when-let* ((comment (bili-danmaku--element
                                     (bili-danmaku--cursor-create
                                      :bytes bytes :position start :end (+ start size)))))
                  (push comment comments))))
          (bili-danmaku--skip cursor tag))))
    (nreverse comments)))

(defun bili-danmaku--text (text)
  "Return safe single-line ASS TEXT without user-supplied override syntax.
ASS has no portable literal escape for braces or backslashes: display their
fullwidth counterparts.  Newlines and controls become spaces, so comments
cannot inject Dialogue records, override tags, or extra unallocated lanes."
  (mapconcat (lambda (char)
               (cond ((= char ?\\) "＼") ((= char ?{) "｛") ((= char ?}) "｝")
                     ((or (< char 32) (= char 127) (= char #x2028)
                          (= char #x2029)) " ")
                     (t (char-to-string char))))
             text ""))

(defun bili-danmaku--time (seconds)
  "Format nonnegative SECONDS as an ASS centisecond timestamp."
  (let ((ticks (floor (* seconds 100))))
    (format "%d:%02d:%02d.%02d" (/ ticks 360000)
            (% (/ ticks 6000) 60) (% (/ ticks 100) 60) (% ticks 100))))

(defun bili-danmaku--separated-p (old new)
  "Whether moving NEW stays behind OLD for their entire shared lifetime.
Occupancy vectors are [START END WIDTH SPEED DIRECTION].  Check both ends
of the linear gap, so a later wide/fast comment cannot catch an earlier
narrow/slow one.  Opposite directions and fixed comments never share a lane."
  (or (<= (aref old 1) (aref new 0))
      (and (/= (aref new 4) 0) (= (aref old 4) (aref new 4))
           (let* ((start (aref new 0))
                  (end (min (aref old 1) (aref new 1)))
                  (old-speed (aref old 3)) (new-speed (aref new 3))
                  (gap (- (* old-speed (- start (aref old 0)))
                          (aref old 2))))
             (and (>= gap 16)
                  (>= (+ gap (* (- old-speed new-speed) (- end start))) 16))))))

(defun bili-danmaku--duration (duration)
  "Validate selected part DURATION and return its number of 360s segments.
A 24-hour ceiling bounds requests to 240; nonfinite durations are rejected."
  (unless (and (numberp duration) (> duration 0) (<= duration 86400))
    (error "Bilibili danmaku duration must be between 0 and 86400 seconds"))
  (ceiling (/ duration 360.0)))

(defun bili-danmaku-to-ass (comments duration)
  "Lay out normal COMMENTS for part DURATION as an in-memory ASS script.
Modes 1/2/3 roll right-to-left, 4 is bottom, 5 top, 6 left-to-right.
Advanced/code/BAS modes 7+ are intentionally ignored.  All modes share
screen-safe vertical lanes.  Conservative glyph bounds reserve two ems per
character; overflow is dropped rather than stacked or allowed to collide."
  (bili-danmaku--duration duration)
  (unless (and (integerp bili-danmaku-font-size)
               (<= 12 bili-danmaku-font-size 64)
               (numberp bili-danmaku-opacity) (<= 0 bili-danmaku-opacity 1)
               (numberp bili-danmaku-display-area)
               (<= 0.1 bili-danmaku-display-area 1)
               (stringp bili-danmaku-font-family)
               (not (string-empty-p bili-danmaku-font-family))
               (not (cl-some (lambda (char)
                               (or (memq char '(?, ?{ ?} ?\\))
                                   (< char 32) (= char 127)))
                             bili-danmaku-font-family)))
    (error "Invalid Bilibili danmaku display customization"))
  (let* ((unit (+ 4 (ceiling (* bili-danmaku-font-size 1.5))))
         (rows (floor (/ 716.0 unit)))
         (area (floor (/ (* 716 bili-danmaku-display-area) unit)))
         (lanes (make-vector rows nil))
         (alpha (round (* 255 (- 1 bili-danmaku-opacity))))
         lines)
    (dolist (comment (sort (copy-sequence comments)
                          (lambda (a b) (< (aref a 0) (aref b 0)))))
      (let* ((start (/ (aref comment 0) 1000.0))
             (mode (aref comment 1))
             (size (round (* bili-danmaku-font-size
                             (/ (max 12 (min 50 (aref comment 2))) 25.0))))
             (height (+ 4 (ceiling (* size 1.5))))
             (needed (ceiling (/ (float height) unit)))
             (text (bili-danmaku--text (aref comment 4)))
             (width (+ 8 (* 2 size (length text))))
             (moving (memq mode '(1 2 3 6)))
             (end (min duration (+ start (if moving 8 4))))
             (direction (cond ((= mode 6) 1) (moving -1) (t 0)))
             (occupancy (vector start end width (if moving (/ (+ 1280 width) 8.0) 0)
                                direction))
             lane)
        (when (and (<= 1 mode 6) (< start end) (<= width 2560)
                   (or moving (<= width 1248)) (<= needed area))
          (setq lane
                (catch 'found
                  (dotimes (offset (1+ (- area needed)))
                    (let ((candidate (if (= mode 4) (- rows needed offset) offset))
                          (free t))
                      (dotimes (row needed)
                        (let ((index (+ candidate row)))
                          (aset lanes index
                                (cl-delete-if (lambda (old) (<= (aref old 1) start))
                                              (aref lanes index)))
                          (unless (cl-every (lambda (old)
                                             (bili-danmaku--separated-p old occupancy))
                                           (aref lanes index))
                            (setq free nil))))
                      (when free (throw 'found candidate))))))
          (when lane
            (dotimes (row needed) (push occupancy (aref lanes (+ lane row))))
            (let* ((y (+ 2 (* lane unit)))
                   (rgb (aref comment 3))
                   (bgr (logior (ash (logand rgb 255) 16)
                                (logand rgb #xff00) (ash rgb -16)))
                   (position (if moving
                                 (format "\\an7\\move(%d,%d,%d,%d,0,8000)"
                                         (if (= mode 6) (- width) 1280) y
                                         (if (= mode 6) 1280 (- width)) y)
                               (format "\\an8\\pos(640,%d)" y))))
              (push (format "Dialogue: 0,%s,%s,Danmaku,,0,0,0,,{%s\\fs%d\\c&H%06X&}%s\n"
                            (bili-danmaku--time start) (bili-danmaku--time end)
                            position size bgr text)
                    lines))))))
    (concat "[Script Info]\nScriptType: v4.00+\nPlayResX: 1280\nPlayResY: 720\nWrapStyle: 2\nScaledBorderAndShadow: yes\n\n"
            "[V4+ Styles]\nFormat: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n"
            (format "Style: Danmaku,%s,%d,&H%02XFFFFFF,&H%02XFFFFFF,&H%02X000000,&HFF000000,0,0,0,0,100,100,0,0,1,1,0,7,0,0,0,1\n\n"
                    bili-danmaku-font-family bili-danmaku-font-size alpha alpha alpha)
            "[Events]\nFormat: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n"
            (apply #'concat (nreverse lines)))))

(defun bili-danmaku-start (cid duration player)
  "Fetch CID's segmented pool for DURATION and attach once to live PLAYER.
Return an AppKit cancellation capability immediately, including while a
request is pending, or nil when disabled or startup fails.  Fetch sequential
360-second segments in memory.  Failure only reports a message: video keeps
playing.  Cancellation retires the request/timer and rejects stale callbacks."
  (when bili-danmaku-enabled
    (condition-case error-data
        (let ((segments (bili-danmaku--duration duration))
              (index 1) (bytes 0) (count 0) chunks request timer token done)
          (unless (and (integerp cid) (> cid 0))
            (error "Bilibili danmaku CID must be positive"))
          (cl-labels
              ((cancel ()
                 (setq done t token nil chunks nil)
                 (when timer (cancel-timer timer) (setq timer nil))
                 (when request
                   (let ((pending request))
                     (setq request nil)
                     (bili-api-cancel pending))))
               (fail (reason)
                 (unless done
                   (cancel)
                   (message "Bilibili danmaku: %s" reason)))
               (live ()
                 (and (not done)
                      (if (video-player-live-p player) t (cancel) nil)))
               (step ()
                 (setq timer nil)
                 (when (live)
                   (condition-case condition
                       (if (> index segments)
                           (let ((text (bili-danmaku-to-ass
                                        (apply #'nconc (nreverse chunks)) duration)))
                             (setq chunks nil)
                             (when (live)
                               (video-player-set-subtitles player text)
                               (setq done t token nil)))
                         (let ((current (make-symbol "danmaku-step")) delivered pending)
                           (setq token current)
                           (setq pending
                                 (bili-api-danmaku-segment
                                  cid index
                                  (lambda (body)
                                    (setq delivered t)
                                    (when (and (eq token current) (live))
                                      (setq request nil token nil)
                                      (condition-case condition
                                          (progn
                                            (cl-incf bytes (length body))
                                            (when (> bytes bili-danmaku--pool-byte-limit)
                                              (error "Danmaku pool exceeds byte limit"))
                                            (let ((comments (bili-danmaku-decode-segment body)))
                                              (cl-incf count (length comments))
                                              (when (> count bili-danmaku--comment-limit)
                                                (error "Danmaku pool exceeds comment limit"))
                                              (push comments chunks))
                                            (cl-incf index)
                                            (setq timer (run-at-time 0 nil #'step)))
                                        (error (fail (error-message-string condition))))))
                                  :errback (lambda (reason)
                                             (setq delivered t)
                                             (when (and (eq token current) (not done))
                                               (setq request nil)
                                               (fail reason)))
                                  :owner bili-api--effect-owner))
                           (unless delivered
                             (if done (when pending (bili-api-cancel pending))
                               (setq request pending)))))
                     (error (fail (error-message-string condition)))))))
            (let ((capability (appkit-cancellation-create :kind 'transport :cancel #'cancel)))
              (step)
              capability)))
      (error (message "Bilibili danmaku: %s" (error-message-string error-data)) nil))))

(provide 'bili-danmaku)

;;; bili-danmaku.el ends here
