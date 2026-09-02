;;; bili-cover.el --- Bilibili cover acquisition and rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: multimedia
;; Package-Requires: ((emacs "30.1") (appkit "0.3"))

;;; Commentary:

;; Fetch public Bilibili cover art into an Appkit-owned disk cache.  Stable
;; entity keys own acquisition state; transport URLs only select a cache
;; revision.  Account cookies never cross this boundary.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'bili-api)
(require 'bili-core)

(defcustom bili-cover-cache-directory
  (locate-user-emacs-file "bili/covers/")
  "Directory holding public Bilibili cover images."
  :type 'directory
  :group 'bili)

(defcustom bili-cover-catalog-lines 4
  "Number of physical text rows occupied by one catalog cover."
  :type '(choice (const 3) (const 4))
  :group 'bili)

(defcustom bili-cover-catalog-max-width 160
  "Maximum catalog cover width in pixels."
  :type 'integer
  :group 'bili)

(defcustom bili-cover-detail-max-width 480
  "Maximum detail cover width in pixels."
  :type 'integer
  :group 'bili)

(defcustom bili-cover-retry-delay 60
  "Seconds before retrying one failed cover acquisition."
  :type 'number
  :group 'bili)

(defvar bili-cover--image-cache (make-hash-table :test #'equal)
  "Decoded cover descriptors keyed by local file identity and geometry.")

(defun bili-cover-resource-key (entity-key)
  "Return the Appkit cover resource key for stable ENTITY-KEY."
  (cons 'cover entity-key))

(defconst bili-cover-trusted-domain-suffixes
  '("hdslb.com" "biliimg.com")
  "Domain suffixes permitted for public Bilibili cover acquisition.")

(defun bili-cover--trusted-host-p (host)
  "Return non-nil when HOST belongs to a trusted Bilibili image domain."
  (and (stringp host)
       (seq-some
        (lambda (suffix)
          (or (string-equal host suffix)
              (string-suffix-p (concat "." suffix) host)))
        bili-cover-trusted-domain-suffixes)))

(defun bili-cover-normalize-url (value)
  "Return VALUE as a trusted HTTPS Bilibili image URL, or nil."
  (when (stringp value)
    (let* ((trimmed (string-trim value))
           (candidate
            (cond
             ((string-prefix-p "//" trimmed) (concat "https:" trimmed))
             ((string-prefix-p "http://" trimmed)
              (concat "https://" (substring trimmed (length "http://"))))
             (t trimmed))))
      (condition-case nil
          (let* ((url (url-generic-parse-url candidate))
                 (host (downcase (or (url-host url) ""))))
            (when (and (string-equal (url-type url) "https")
                       (bili-cover--trusted-host-p host)
                       (or (null (url-port url)) (= (url-port url) 443))
                       (null (url-user url))
                       (null (url-password url)))
              candidate))
        (error nil)))))

(defun bili-cover--image-capable-frame-p (frame)
  "Return non-nil when live FRAME can render inline images."
  (and (frame-live-p frame)
       (with-selected-frame frame
         (appkit-media-inline-image-rendering-available-p))))

(defun bili-cover--display-frame (&optional view)
  "Return an image-capable frame, preferring VIEW's display window."
  (or (when (and (appkit-view-p view) (appkit-view-live-p view))
        (when-let* ((buffer (appkit-view-buffer view))
                    ((buffer-live-p buffer))
                    (window (appkit-view-display-window buffer))
                    ((window-live-p window))
                    (frame (window-frame window))
                    ((bili-cover--image-capable-frame-p frame)))
          frame))
      (seq-find #'bili-cover--image-capable-frame-p (frame-list))
      (selected-frame)))

(defun bili-cover--image-display-available-p ()
  "Return non-nil when any live frame can render inline images."
  (seq-some #'bili-cover--image-capable-frame-p (frame-list)))

(defun bili-cover--cache-base (entity-key url)
  "Return the cache base for ENTITY-KEY and public source URL."
  (let ((directory
         (expand-file-name
          (secure-hash 'sha256 (prin1-to-string entity-key))
          bili-cover-cache-directory)))
    (expand-file-name
     (secure-hash 'sha256 (car (split-string url "[#?]")))
     directory)))

(defun bili-cover--cached-file (entity-key url)
  "Return a cached cover file for ENTITY-KEY and URL, or nil."
  (appkit-media-image-cache-existing-file
   (bili-cover--cache-base entity-key url)))

(defun bili-cover--state-current-p (app entity-key url token)
  "Return non-nil when APP still expects URL and TOKEN for ENTITY-KEY."
  (let ((state (and (appkit-app-live-p app)
                    (bili-core-cover-state app entity-key))))
    (and (equal (plist-get state :url) url)
         (eq (plist-get state :token) token))))

(defun bili-cover--failed-state (url reason)
  "Return a failed cover state for URL and readable REASON."
  (list :url url :status 'failed
        :reason (format "%s" reason)
        :retry-at (+ (float-time) bili-cover-retry-delay)))

(defun bili-cover--start-fetch (app entity-key url)
  "Start APP's cover transfer for ENTITY-KEY from URL."
  (let ((token (gensym "bili-cover-"))
        transfer lifecycle-handle completed-p)
    (bili-core-store-cover-state
     app entity-key (list :url url :status 'pending :token token))
    (cl-labels
        ((finish
          (new-state)
          (unless completed-p
            (setq completed-p t)
            (when lifecycle-handle
              (appkit-retire-handle lifecycle-handle))
            (when (bili-cover--state-current-p app entity-key url token)
              (bili-core-store-cover-state app entity-key new-state)))))
      (condition-case error-data
          (progn
            (setq transfer
                  (appkit-media-cache-image-resource-async
                   `((url . ,url)
                     (name . ,(or (appkit-media-url-filename url)
                                  "cover.img")))
                   (bili-cover--cache-base entity-key url)
                   (lambda (downloaded)
                     (finish (list :url url :status 'ready
                                   :file downloaded)))
                   (lambda (reason)
                     (finish (bili-cover--failed-state url reason)))
                   :headers
                   `(("Accept" . "image/avif,image/webp,image/*;q=0.8,*/*;q=0.1")
                     ("Referer" . "https://www.bilibili.com/")
                     ("User-Agent" . ,bili-api-user-agent))))
            (when (and (appkit-media-transfer-p transfer)
                       (not completed-p))
              (setq lifecycle-handle
                    (appkit-register-handle
                     app 'function transfer #'appkit-media-cancel-transfer)))
            transfer)
        (error
         (when (appkit-media-transfer-p transfer)
           (appkit-media-cancel-transfer transfer))
         (finish
          (bili-cover--failed-state
           url (error-message-string error-data)))
         nil)))))

(defun bili-cover-prefetch (app entity-key url)
  "Ensure APP is acquiring public cover URL for stable ENTITY-KEY.

Return the Appkit media transfer handle, or nil when no transfer is needed.
The transfer is owned by APP, shared by Appkit when byte-identical, and never
sends account cookies."
  (when-let* ((url (bili-cover-normalize-url url))
              ((appkit-app-live-p app))
              ((bili-cover--image-display-available-p)))
    (let* ((state (bili-core-cover-state app entity-key))
           (same-source (equal (plist-get state :url) url))
           (status (and same-source (plist-get state :status)))
           (file (and same-source (plist-get state :file))))
      (cond
       ((and (eq status 'ready)
             (stringp file)
             (file-readable-p file))
        nil)
       ((eq status 'pending) nil)
       ((and (eq status 'failed)
             (> (or (plist-get state :retry-at) 0) (float-time)))
        nil)
       (t
        (if-let* ((cached (bili-cover--cached-file entity-key url)))
            (progn
              (bili-core-store-cover-state
               app entity-key
               (list :url url :status 'ready :file cached))
              nil)
          (bili-cover--start-fetch app entity-key url)))))))

(defun bili-cover--file (app entity-key url)
  "Return APP's ready cover file for ENTITY-KEY and URL, or nil."
  (let ((state (bili-core-cover-state app entity-key)))
    (when-let* ((file (plist-get state :file)))
      (when (and (equal (plist-get state :url) url)
                 (eq (plist-get state :status) 'ready)
                 (file-readable-p file))
        file))))

(defun bili-cover-image (app entity-key url pixel-width pixel-height)
  "Return APP's cached cover image for ENTITY-KEY and URL.

PIXEL-WIDTH and PIXEL-HEIGHT define a center-cropped display box.  This
function performs no network I/O; `bili-cover-prefetch' owns acquisition."
  (when-let* ((url (bili-cover-normalize-url url))
              (file (bili-cover--file app entity-key url))
              (attributes (file-attributes file 'string)))
    (let* ((identity
            (list file (file-attribute-size attributes)
                  (file-attribute-modification-time attributes)
                  pixel-width pixel-height))
           (cached (gethash identity bili-cover--image-cache)))
      (or cached
          (when-let* ((image
                       (appkit-media-cropped-preview-image-from-file
                        file pixel-width pixel-height)))
            (puthash identity image bili-cover--image-cache)
            image)))))

(defun bili-cover-catalog-slice-rows (view entity-key url)
  "Return aligned cover slice rows for VIEW, ENTITY-KEY, and URL.

The exact row count follows `bili-cover-catalog-lines'.  Each slice retains a
fixed-width textual fallback so right-hand metadata can use ordinary column
layout before and after the image arrives."
  (let* ((app (appkit-view-app view))
         (frame (bili-cover--display-frame view))
         (line-count (min 4 (max 3 bili-cover-catalog-lines)))
         (height (* line-count (max 1 (frame-char-height frame))))
         (width (min (max 1 bili-cover-catalog-max-width)
                     (round (* height (/ 16.0 9.0)))))
         (columns (max 8 (ceiling (/ (float width)
                                    (max 1 (frame-char-width frame))))))
         (image
          (with-selected-frame frame
            (bili-cover-image app entity-key url width height)))
         (source-rows
          (and image
               (with-selected-frame frame
                 (appkit-media-image-slice-rows image))))
         rows)
    (let ((index 0))
      (while (< index line-count)
        (let* ((fallback (make-string columns ?\s))
               (source (nth index source-rows))
               (display (and source (get-text-property 0 'display source)))
               (row (propertize fallback
                                'help-echo (or url "Cover unavailable")
                                'face (and (null display) 'shadow)
                                'rear-nonsticky '(display))))
          (when display
            (put-text-property 0 (length row) 'display display row))
          (push row rows))
        (setq index (1+ index))))
    (nreverse rows)))

(defun bili-cover-detail-image (view entity-key url)
  "Return a responsive 16:9 cover image for VIEW, ENTITY-KEY, and URL."
  (let* ((frame (bili-cover--display-frame view))
         (columns
          (with-current-buffer (appkit-view-buffer view)
            (or (appkit-view-responsive-width 4) 76)))
         (width (max 160
                     (min bili-cover-detail-max-width
                          (* columns (max 1 (frame-char-width frame))))))
         (height (max 90 (round (* width (/ 9.0 16.0))))))
    (with-selected-frame frame
      (bili-cover-image
       (appkit-view-app view) entity-key url width height))))

(provide 'bili-cover)

;;; bili-cover.el ends here
