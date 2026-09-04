;;; bili-detail.el --- Bilibili entity detail views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Present one video or canonical live room through an Appkit-owned, stable-key
;; projection with responsive multiline media.

;;; Code:

(require 'button)
(require 'browse-url)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-command)
(require 'appkit-effect)
(require 'appkit-projection)
(require 'appkit-resource)
(require 'appkit-surface)
(require 'appkit-media-image)
(require 'appkit-ui)
(require 'bili-api)
(require 'bili-cover)
(require 'bili-comment)
(require 'bili-core)
(require 'bili-live)
(require 'bili-model)
(require 'bili-playback)
(require 'bili-render)

(defconst bili-detail--request-key 'detail
  "App Effect key prefix for entity metadata.")

(defconst bili-detail--playback-request-key 'playback
  "Surface Effect key for playback URL resolution.")

(defconst bili-detail--playback-presentation-key 'playback-presentation
  "Surface Effect key for the active video presentation.")

(defconst bili-detail-row-key-property 'bili-detail-row-key
  "Text property carrying a stable Bilibili detail row key.")

(defvar-keymap bili-detail-mode-map
  :doc "Keymap for Bilibili detail views."
  :parent special-mode-map
  "RET" #'bili-detail-activate
  "g" #'bili-detail-refresh
  "P" #'bili-detail-play
  "c" #'bili-detail-open-comments
  "s" #'bili-detail-select-page
  "o" #'bili-detail-open-in-browser
  "n" #'bili-detail-next-action
  "p" #'bili-detail-previous-action
  "b" #'quit-window
  "q" #'quit-window
  "?" #'describe-mode)

(define-derived-mode bili-detail-mode special-mode "Bilibili-Detail"
  "Major mode for one Bilibili video or live-room detail."
  (setq-local truncate-lines nil
              word-wrap t
              switch-to-buffer-preserve-window-point nil
              header-line-format '(:eval (bili-detail--header-line)))
  (when (fboundp 'appkit-ui-buffer-substring-filter)
    (setq-local filter-buffer-substring-function
                #'appkit-ui-buffer-substring-filter)))

(defun bili-detail--state (owner)
  "Return validated detail state from OWNER."
  (let ((state (if (appkit-surface-p owner)
                   (appkit-surface-model owner)
                 owner)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'detail)
                 (memq (plist-get state :kind) '(video live)))
      (error "Invalid Bilibili detail state"))
    state))

(defun bili-detail--current-surface ()
  "Return the current live Bilibili detail Surface, or nil."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface))
              (state (appkit-surface-model surface))
              ((eq (plist-get state :type) 'detail)))
    surface))

(defun bili-detail--model (app-read-view state)
  "Return APP-READ-VIEW's canonical model selected by detail STATE."
  (pcase (plist-get state :kind)
    ('video (bili-core-video app-read-view (plist-get state :id)))
    ('live (bili-core-room app-read-view (plist-get state :id)))))

(defun bili-detail--cover-data (model)
  "Return stable entity key and cover URL for MODEL."
  (cond
   ((bili-video-p model)
    (cons (list 'video (bili-video-bvid model))
          (bili-video-cover model)))
   ((bili-live-room-p model)
    (cons (list 'live (bili-live-room-id model))
          (bili-live-room-cover model)))))


(defun bili-detail--cover-row (model resource)
  "Return MODEL's cover row depending on canonical RESOURCE."
  (let* ((data (bili-detail--cover-data model))
         (entity-key (car data))
         (url (cdr data))
         (demand (bili-cover-demand entity-key url))
         (resource-key
          (and demand (appkit-resource-demand-key demand))))
    (bili-detail--row
     '(cover) 'cover ""
     :entity-key entity-key :url url :resource-key resource-key
     :dependencies (delq nil (list resource resource-key))
     :resource-demands (and demand (list demand)))))

(defun bili-detail--new-state (kind id &optional model)
  "Return fresh detail state for KIND, ID, and optional MODEL."
  (let ((selected-cid
         (and (bili-video-p model) (bili-video-cid model))))
    (list :type 'detail :kind kind :id id
          :phase (if model 'ready 'initial)
          :message nil :loaded-p (and model t)
          :request-token nil :selected-cid selected-cid
          :playback-phase 'idle :playback-message nil)))

(defun bili-detail--header-line ()
  "Return the persistent header line for the current detail Surface."
  (if-let* ((surface (bili-detail--current-surface))
            (state (bili-detail--state surface)))
      (let ((phase (plist-get state :phase))
            (playback (plist-get state :playback-phase)))
        (format " Bilibili · %s · %s%s%s"
                (if (eq (plist-get state :kind) 'video) "Video" "Live room")
                (plist-get state :id)
                (if (memq phase '(initial refresh)) " · refreshing" "")
                (if (eq playback 'resolving) " · resolving playback" "")))
    " Bilibili"))

(defun bili-detail--row (key type text &rest properties)
  "Return one detail row with KEY, TYPE, TEXT, and PROPERTIES."
  (append (list :key key :type type :text (or text "")) properties))

(defun bili-detail--video-page-label (page selected-p)
  "Return display label for PAGE, marking it when SELECTED-P."
  (format "%s P%d  %s%s"
          (if selected-p "*" " ")
          (bili-video-page-number page)
          (bili-video-page-title page)
          (if (> (bili-video-page-duration page) 0)
              (format "  %s"
                      (bili-render-duration (bili-video-page-duration page)))
            "")))

(defun bili-detail--published-date (timestamp)
  "Return a detail publication date for TIMESTAMP."
  (if (and (numberp timestamp) (> timestamp 0))
      (format-time-string "%Y-%m-%d" (seconds-to-time timestamp))
    ""))

(defun bili-detail--video-rows (state video)
  "Return projected detail rows for VIDEO in STATE."
  (let* ((resource (list 'video (bili-video-bvid video)))
         (pages (bili-video-pages video))
         (selected-cid (or (plist-get state :selected-cid)
                           (bili-video-cid video)))
         (selected
          (or (seq-find (lambda (page)
                          (= (bili-video-page-cid page) selected-cid))
                        pages)
              (car pages)))
         (playback-phase (plist-get state :playback-phase))
         (play-label
          (pcase playback-phase
            ('resolving "Resolving playback address...")
            ('error "Retry playback")
            (_ (if (> (length pages) 1)
                   (format "Play P%d · %s"
                           (bili-video-page-number selected)
                           (bili-video-page-title selected))
                 "Play video"))))
         (rows
          (list
           (bili-detail--row '(title) 'title (bili-video-title video)
                             :dependencies (list resource))
           (bili-detail--cover-row video resource)
           (bili-detail--row
            '(owner) 'meta
            (string-join
             (delq nil
                   (list
                    (format "UP: %s" (bili-video-owner video))
                    (bili-video-bvid video)
                    (and (not (string-empty-p (bili-video-category video)))
                         (bili-video-category video))
                    (and (> (bili-video-published-at video) 0)
                         (bili-detail--published-date
                          (bili-video-published-at video)))))
             " · ")
            :dependencies (list resource))
           (bili-detail--row
            '(stats) 'meta
            (format "%s views · %s likes · %s danmaku · %s"
                    (bili-render-count (bili-video-views video))
                    (bili-render-count (bili-video-likes video))
                    (bili-render-count (bili-video-danmaku video))
                    (bili-render-duration (bili-video-duration video)))
            :dependencies (list resource))
           (bili-detail--row
            '(action play) 'action play-label
            :enabled-p (not (eq playback-phase 'resolving))
            :action #'bili-detail-play :dependencies (list resource))
           (bili-detail--row
            '(action comments) 'action "View comments"
            :enabled-p t :action #'bili-detail-open-comments
            :dependencies (list resource)))))
    (when (eq playback-phase 'error)
      (setq rows
            (append rows
                    (list (bili-detail--row
                           '(playback-error) 'error
                           (format "Playback failed: %s"
                                   (or (plist-get state :playback-message)
                                       "Unknown error")))))))
    (unless (string-empty-p (bili-video-description video))
      (setq rows
            (append
             rows
             (list
              (bili-detail--row
               '(description-heading) 'heading "Description")
              (bili-detail--row
               '(description) 'body (bili-video-description video)
               :dependencies (list resource))))))
    (when (> (length pages) 1)
      (setq rows
            (append
             rows
             (list (bili-detail--row
                    '(pages-heading) 'heading
                    (format "Parts · %d" (length pages))))
             (mapcar
              (lambda (page)
                (let ((cid (bili-video-page-cid page)))
                  (bili-detail--row
                   (list 'page cid) 'page
                   (bili-detail--video-page-label page (= cid selected-cid))
                   :object page :action #'bili-detail--play-page-action
                   :dependencies (list resource))))
              pages))))
    rows))

(defun bili-detail--live-status (status)
  "Return display text and face for live STATUS."
  (pcase status
    (1 (cons "LIVE" 'bili-live-face))
    (2 (cons "Round/replay in progress (playback not supported)" 'warning))
    (0 (cons "Offline" 'bili-disabled-face))
    (_ (cons (format "Unknown live status: %s" status) 'warning))))

(defun bili-detail--live-rows (state room)
  "Return projected detail rows for live ROOM in STATE."
  (let* ((resource (list 'live (bili-live-room-id room)))
         (status (bili-live-room-live-status room))
         (status-presentation (bili-detail--live-status status))
         (playback-phase (plist-get state :playback-phase))
         (rows
          (list
           (bili-detail--row '(status) 'status (car status-presentation)
                             :face (cdr status-presentation)
                             :dependencies (list resource))
           (bili-detail--row '(title) 'title (bili-live-room-title room)
                             :dependencies (list resource))
           (bili-detail--cover-row room resource)
           (bili-detail--row
            '(owner) 'meta
            (format "Streamer: %s · room %s"
                    (if (string-empty-p (bili-live-room-owner room))
                        "Unknown"
                      (bili-live-room-owner room))
                    (bili-live-room-id room))
            :dependencies (list resource))
           (bili-detail--row
            '(stats) 'meta
            (string-join
             (delq nil
                   (list
                    (and (not (string-empty-p (bili-live-room-parent-area room)))
                         (bili-live-room-parent-area room))
                    (and (not (string-empty-p (bili-live-room-area room)))
                         (bili-live-room-area room))
                    (format "%s online"
                            (bili-render-count (bili-live-room-online room)))))
             " · ")
            :dependencies (list resource)))))
    (pcase status
      (1
       (setq rows
             (append
              rows
              (list
               (bili-detail--row
                '(action play) 'action
                (pcase playback-phase
                  ('resolving "Resolving live stream...")
                  ('error "Retry live stream")
                  (_ "Watch live"))
                :enabled-p (not (eq playback-phase 'resolving))
                :action #'bili-detail-play :dependencies (list resource))))))
      (0
       (setq rows
             (append rows
                     (list (bili-detail--row
                            '(action refresh) 'action "Refresh live status"
                            :enabled-p t :action #'bili-detail-refresh)))))
      (_ nil))
    (when (eq playback-phase 'error)
      (setq rows
            (append rows
                    (list (bili-detail--row
                           '(playback-error) 'error
                           (format "Playback failed: %s"
                                   (or (plist-get state :playback-message)
                                       "Unknown error")))))))
    (unless (string-empty-p (bili-live-room-description room))
      (setq rows
            (append rows
                    (list
                     (bili-detail--row '(description-heading) 'heading
                                       "Description")
                     (bili-detail--row '(description) 'body
                                       (bili-live-room-description room)
                                       :dependencies (list resource))))))
    rows))

(defun bili-detail--rows (_surface app-read-view state)
  "Return projected rows for detail STATE against APP-READ-VIEW."
  (let ((model (bili-detail--model app-read-view state))
        rows)
    (when (memq (plist-get state :phase) '(initial refresh))
      (push (bili-detail--row
             '(request-status) 'note
             (if model "Refreshing details..." "Loading details...")
             :face 'shadow)
            rows))
    (when (eq (plist-get state :phase) 'error)
      (push (bili-detail--row
             '(request-error) 'error
             (format "Unable to load details: %s"
                     (or (plist-get state :message) "Unknown error")))
            rows)
      (push (bili-detail--row '(action retry) 'action "Retry"
                              :enabled-p t :action #'bili-detail-refresh)
            rows))
    (setq rows
          (append
           (nreverse rows)
           (cond
            ((bili-video-p model) (bili-detail--video-rows state model))
            ((bili-live-room-p model) (bili-detail--live-rows state model))
            (t nil))))
    (mapcar
     (lambda (entry)
       (appkit-projection-row-create
        :key (plist-get entry :key)
        :payload entry
        :dependencies (plist-get entry :dependencies)
        :resource-demands (plist-get entry :resource-demands)))
     rows)))

(defun bili-detail--print-row
    (surface _app-read-view projection-row)
  "Render PROJECTION-ROW for detail SURFACE."
  (let* ((entry (appkit-projection-row-payload projection-row))
         (key (appkit-projection-row-key projection-row))
         (type (plist-get entry :type))
         (text (plist-get entry :text))
         (face (plist-get entry :face))
         (start (point)))
    (pcase type
      ('title (appkit-presentation-insert-heading-line text :face 'bili-title-face))
      ('heading (appkit-presentation-insert-heading-line text :face 'bili-section-face))
      ('meta (appkit-presentation-insert-note-line text :face 'bili-meta-face))
      ('status (appkit-presentation-insert-heading-line text :face face))
      ('note (appkit-presentation-insert-note-line text :face (or face 'shadow)))
      ('error (appkit-presentation-insert-note-line text :face 'bili-error-face))
      ('cover
       (let ((image
              (bili-cover-detail-image
               surface (plist-get entry :entity-key)
               (plist-get entry :url))))
         (if image
             (progn
               (appkit-media-insert-image-slices
                image nil nil "[cover]" (plist-get entry :url))
               (insert "\n"))
           (let ((state
                  (and (plist-get entry :resource-key)
                       (appkit-resource-state
                        surface (plist-get entry :resource-key)))))
             (appkit-presentation-insert-note-line
              (if (and state
                       (eq (appkit-resource-state-status state) 'pending))
                  "Loading cover..."
                "Cover unavailable")
              :face 'shadow)))))
      ('body
       (insert (propertize text 'face (or face 'default)) "\n"))
      ('action
       (let ((row-start (point)))
         (if (plist-get entry :enabled-p)
             (appkit-ui-insert-action-button
              (format " %s " text) (plist-get entry :action)
              :face 'bili-action-face :help-echo text)
           (insert
            (propertize (format " %s " text) 'face 'bili-disabled-face)))
         (insert "\n")
         (appkit-ui-apply-line-prefix row-start (point) "  ")))
      ('page
       (let ((row-start (point)))
         (insert text)
         (let ((row-end (point)))
           (appkit-ui-make-action-row
            row-start row-end (plist-get entry :object)
            (plist-get entry :action) :help-echo "Play this part"))
         (insert "\n")
         (appkit-ui-apply-line-prefix row-start (point) "  ")))
      (_ (error "Unknown Bilibili detail row type: %S" type)))
    (add-text-properties
     start (point)
     (list bili-detail-row-key-property key
           'rear-nonsticky (list bili-detail-row-key-property)))))



(defun bili-detail--render-change (&optional position)
  "Return a full detail render request restoring POSITION."
  (appkit-projection-change-create
   :full-p t :frame-p t :position (or position 'preserve)))

(defun bili-detail--reply-command (route message)
  "Return a report-delivery command sending MESSAGE to ROUTE."
  (appkit-command-post-message
   :target route :message message :delivery 'report))

(defun bili-detail--metadata-effect (context token kind id)
  "Return App Effect resolving KIND and ID for request TOKEN."
  (let ((source (appkit-transition-context-source-address context))
        (route (appkit-transition-context-reply-route context)))
    (unless (and source route)
      (error "Detail request lacks a live Surface reply route"))
    (appkit-effect-create
     :key (list bili-detail--request-key source)
     :input (list token kind id route)
     :start
     (lambda (_effect-context input _observe resolve reject)
       (pcase-let ((`(,_token ,request-kind ,request-id ,_route)
                    input))
         (pcase request-kind
           ('video
            (bili-api-effect-cancellation
             (bili-api-video
              request-id
              (lambda (data)
                (condition-case condition
                    (funcall resolve
                             (bili-model-video-from-json data))
                  (error
                   (funcall reject
                            (error-message-string condition)))))
              :errback reject
              :owner bili-api--effect-owner)))
           ('live
            (bili-live-effect-cancellation
             (bili-live-resolve-room
              request-id resolve :errback reject
              :owner bili-api--effect-owner)))
           (_ (error "Unsupported Bilibili detail kind")))))
     :success
     (lambda (input entity)
       (list 'detail 'transport-succeeded input entity))
     :failure
     (lambda (input reason)
       (list 'detail 'transport-failed input (format "%s" reason)))
     :cancellation-requirement 'transport)))

(defun bili-detail--app-succeeded (model input entity)
  "Commit detail ENTITY into App MODEL and reply using INPUT."
  (pcase-let ((`(,token ,kind ,id ,route) input))
    (condition-case condition
        (let (next canonical-id selected-cid live-status)
          (pcase kind
            ('video
             (unless (and (bili-video-p entity)
                          (equal (bili-video-bvid entity) id))
               (error "Bilibili returned another video"))
             (setq next (bili-core--put-video model entity)
                   canonical-id (bili-video-bvid entity)
                   selected-cid (bili-video-cid entity)))
            ('live
             (unless (bili-live-room-p entity)
               (error "Bilibili returned an invalid live room"))
             (setq next (bili-core--put-room model entity)
                   canonical-id (bili-live-room-id entity)
                   live-status (bili-live-room-live-status entity)))
            (_ (error "Unsupported Bilibili detail kind")))
          (appkit-next
           :model next
           :render appkit-render-none
           :commands
           (list
            (bili-detail--reply-command
             route
             (list 'detail 'succeeded token kind canonical-id
                   selected-cid live-status)))))
      (error
       (appkit-next
        :model model
        :render appkit-render-none
        :commands
        (list
         (bili-detail--reply-command
          route
          (list 'detail 'failed token
                (error-message-string condition)))))))))

(defun bili-detail--app-update (context model message)
  "Advance canonical detail state in MODEL for MESSAGE."
  (pcase message
    (`(detail request ,token ,kind ,id)
     (if (and token (memq kind '(video live)))
         (appkit-next
          :model model
          :render appkit-render-none
          :commands
          (list
           (appkit-command-start-effect
            (bili-detail--metadata-effect
             context token kind id))))
       (appkit-next-reject "Invalid Bilibili detail request")))
    (`(detail transport-succeeded ,input ,entity)
     (bili-detail--app-succeeded model input entity))
    (`(detail transport-failed
       (,token ,_kind ,_id ,route) ,reason)
     (appkit-next
      :model model
      :render appkit-render-none
      :commands
      (list
       (bili-detail--reply-command
        route (list 'detail 'failed token reason)))))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili detail message: %S" message)))))

(defun bili-detail--request-command (context state token)
  "Return command requesting STATE's entity from its App."
  (appkit-command-post-message
   :target (appkit-transition-context-parent-address context)
   :message
   (list 'detail 'request token
         (plist-get state :kind) (plist-get state :id))
   :delivery 'report
   :reply-correlation token))

(defun bili-detail--surface-init (context input)
  "Initialize one detail Surface from INPUT."
  (let ((state (copy-sequence (bili-detail--state input))))
    (if (plist-get state :loaded-p)
        (appkit-next
         :model state :render (bili-detail--render-change 'first))
      (let ((token (make-symbol "bili-detail-request-")))
        (setf (plist-get state :request-token) token)
        (appkit-next
         :model state
         :render (bili-detail--render-change 'first)
         :commands
         (list
          (bili-detail--request-command context state token)))))))

(defun bili-detail--surface-request (context state)
  "Transition detail STATE into a metadata refresh."
  (let ((next (copy-sequence state))
        (token (make-symbol "bili-detail-request-")))
    (setf (plist-get next :phase)
          (if (plist-get state :loaded-p) 'refresh 'initial)
          (plist-get next :message) nil
          (plist-get next :request-token) token)
    (appkit-next
     :model next
     :render (bili-detail--render-change)
     :commands
     (list (bili-detail--request-command context next token)))))

(defun bili-detail--surface-succeeded
    (state token kind id selected-cid live-status)
  "Install accepted detail response in STATE."
  (if (not (eq token (plist-get state :request-token)))
      (appkit-next :model state :render appkit-render-none)
    (let ((next (copy-sequence state)))
      (setf (plist-get next :kind) kind
            (plist-get next :id) id
            (plist-get next :phase) 'ready
            (plist-get next :message) nil
            (plist-get next :loaded-p) t
            (plist-get next :request-token) nil)
      (when (and selected-cid
                 (null (plist-get next :selected-cid)))
        (setf (plist-get next :selected-cid) selected-cid))
      (appkit-next
       :model next
       :render
       (bili-detail--render-change
        (if (and (eq kind 'live) (not (= live-status 1)))
            '(title)
          '(action play)))))))

(defun bili-detail--surface-failed (state token reason)
  "Install detail failure REASON when TOKEN remains current."
  (if (not (eq token (plist-get state :request-token)))
      (appkit-next :model state :render appkit-render-none)
    (let ((next (copy-sequence state)))
      (setf (plist-get next :phase) 'error
            (plist-get next :message) reason
            (plist-get next :request-token) nil)
      (appkit-next
       :model next :render (bili-detail--render-change)))))

(defun bili-detail--playback-resolution-effect (app-read-view state page)
  "Return the transport Effect resolving playback from committed STATE."
  (let* ((entity (bili-detail--model app-read-view state))
         (selected
          (and (bili-video-p entity)
               (or page (bili-detail--selected-page entity state))))
         (input (if (bili-video-p entity)
                    (list entity selected)
                  (list entity))))
    (unless entity
      (user-error "Bilibili detail has not loaded"))
    (when (and (bili-live-room-p entity)
               (/= (bili-live-room-live-status entity) 1))
      (user-error "This live room has no supported live stream"))
    (appkit-effect-create
     :key bili-detail--playback-request-key
     :input input
     :start
     (if (bili-video-p entity)
         #'bili-playback--video-transport-start
       #'bili-playback--live-transport-start)
     :success
     (lambda (owned-input data)
       (list 'playback 'resolved owned-input data))
     :failure
     (lambda (_input reason)
       (list 'playback 'failed (format "%s" reason)))
     :cancellation-requirement 'transport)))

(defun bili-detail--playback-presentation-effect (presentation)
  "Return the Effect owning video PRESENTATION until its viewer closes."
  (appkit-effect-create
   :key bili-detail--playback-presentation-key
   :input presentation
   :start #'appkit-media-video-presentation-start
   :success (lambda (_input _reason) '(playback closed))
   :failure
   (lambda (_input reason)
     (list 'playback 'failed (format "%s" reason)))
   :cancellation-requirement 'logical))

(defun bili-detail--surface-play (context state page)
  "Resolve PAGE playback from detail STATE."
  (let* ((app-read-view
          (appkit-transition-context-app-read-view context))
         (next (copy-sequence state))
         (selected
          (and page (bili-video-page-cid page))))
    (when selected
      (setf (plist-get next :selected-cid) selected))
    (setf (plist-get next :playback-phase) 'resolving
          (plist-get next :playback-message) nil)
    (appkit-next
     :model next
     :render (bili-detail--render-change)
     :commands
     (list
      (appkit-command-cancel-effect
       bili-detail--playback-presentation-key)
      (appkit-command-start-effect
       (bili-detail--playback-resolution-effect
        app-read-view next page))))))

(defun bili-detail--surface-playback-resolved (state input data)
  "Present resolved playback DATA for owned INPUT from STATE."
  (condition-case condition
      (let* ((entity (car input))
             (presentation
              (if (bili-video-p entity)
                  (bili-playback-video-presentation
                   entity data :page (cadr input))
                (bili-playback-live-presentation entity data)))
             (next (copy-sequence state)))
        (setf (plist-get next :playback-phase) 'playing
              (plist-get next :playback-message) nil)
        (appkit-next
         :model next
         :render (bili-detail--render-change)
         :commands
         (list
          (appkit-command-start-effect
           (bili-detail--playback-presentation-effect presentation)))))
    ((error quit)
     (let ((next (copy-sequence state)))
       (setf (plist-get next :playback-phase) 'error
             (plist-get next :playback-message)
             (error-message-string condition))
       (appkit-next
        :model next :render (bili-detail--render-change))))))

(defun bili-detail--surface-update (context state message)
  "Advance detail Surface STATE for MESSAGE."
  (pcase message
    ('refresh (bili-detail--surface-request context state))
    (`(detail succeeded ,token ,kind ,id ,selected-cid ,live-status)
     (bili-detail--surface-succeeded
      state token kind id selected-cid live-status))
    (`(detail failed ,token ,reason)
     (bili-detail--surface-failed state token reason))
    (`(play ,page)
     (bili-detail--surface-play context state page))
    (`(playback resolved ,input ,data)
     (bili-detail--surface-playback-resolved state input data))
    ('(playback closed)
     (let ((next (copy-sequence state)))
       (setf (plist-get next :playback-phase) 'idle
             (plist-get next :playback-message) nil)
       (appkit-next
        :model next :render (bili-detail--render-change))))
    (`(playback failed ,reason)
     (let ((next (copy-sequence state)))
       (setf (plist-get next :playback-phase) 'error
             (plist-get next :playback-message) reason)
       (appkit-next
        :model next :render (bili-detail--render-change))))
    (`(select-page ,cid)
     (let ((next (copy-sequence state)))
       (setf (plist-get next :selected-cid) cid)
       (appkit-next
        :model next
        :render (bili-detail--render-change (list 'page cid)))))
    ('geometry
     (appkit-next
      :model state
      :render (appkit-projection-change-create :geometry-p t)))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili detail Surface message: %S"
                message)))))

(defun bili-detail--setup (surface)
  "Install responsive geometry for detail SURFACE."
  (appkit-surface-enable-responsive-geometry
   surface
   (lambda (owner _width)
     (when (appkit-surface-live-p owner)
       (appkit-surface-post owner 'geometry))))
  (with-current-buffer (appkit-surface-buffer surface)
    (appkit-surface-refresh-responsive-geometry surface)))

(defconst bili-detail--surface-type
  (appkit-surface-type-create
   :name 'bili-detail
   :mode #'bili-detail-mode
   :init #'bili-detail--surface-init
   :update #'bili-detail--surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-projection-renderer-create
      :project-all #'bili-detail--rows
      :printer #'bili-detail--print-row
      :anchor-property bili-detail-row-key-property
      :geometry-mode 'reproject
      :no-separator-p t)))
  "Generated Surface type for Bilibili entity details.")

(defun bili-detail--open (kind id)
  "Open or select the canonical detail Surface for KIND and ID."
  (let* ((app (bili-core-app))
         (surface-id (list 'detail kind id))
         (existing (appkit-app-surface app surface-id)))
    (if (appkit-surface-live-p existing)
        (progn
          (pop-to-buffer (appkit-surface-buffer existing))
          existing)
      (let* ((cached
              (pcase kind
                ('video (bili-core-video app id))
                ('live (bili-core-room app id))))
             (surface
              (appkit-open-generated-surface
               bili-detail--surface-type
               :app app :identity surface-id
               :input (bili-detail--new-state kind id cached)
               :buffer-name (format "*Bilibili %s: %s*" kind id)
               :select t)))
        (bili-detail--setup surface)
        surface))))

(defun bili-detail-open-video (bvid)
  "Open Bilibili video BVID details."
  (bili-detail--open 'video bvid))

(defun bili-detail-open-live-room (room-id)
  "Open Bilibili live-room ROOM-ID details."
  (unless (and (integerp room-id) (> room-id 0))
    (error "Bilibili live room id must be positive"))
  (bili-detail--open 'live room-id))

(defun bili-detail-refresh ()
  "Refresh the current Bilibili detail Surface."
  (interactive)
  (if-let* ((surface (bili-detail--current-surface)))
      (appkit-surface-send surface 'refresh)
    (user-error "Current buffer is not a Bilibili detail view")))

(defun bili-detail--selected-page (video state)
  "Return VIDEO page selected by detail STATE."
  (or (seq-find
       (lambda (page)
         (= (bili-video-page-cid page) (plist-get state :selected-cid)))
       (bili-video-pages video))
      (car (bili-video-pages video))))

(defun bili-detail--start-playback (surface &optional page)
  "Start playback from SURFACE, optionally selecting video PAGE."
  (appkit-surface-send surface (list 'play page)))

(defun bili-detail-open-comments ()
  "Open the read-only comment stream for the current video detail."
  (interactive)
  (let* ((surface
          (or (bili-detail--current-surface)
              (user-error "Current buffer is not a Bilibili detail view")))
         (state (bili-detail--state surface))
         (video
          (bili-detail--model (appkit-surface-app surface) state)))
    (unless (bili-video-p video)
      (user-error "Current detail is not a video"))
    (bili-comment-open video)))

(defun bili-detail-play ()
  "Play the current Bilibili detail or selected video part."
  (interactive)
  (let ((surface
         (or (bili-detail--current-surface)
             (user-error "Current buffer is not a Bilibili detail view"))))
    (bili-detail--start-playback surface)))

(defun bili-detail--play-page-action (page)
  "Play video PAGE from the current detail view."
  (let ((surface
         (or (bili-detail--current-surface)
             (user-error "Current buffer is not a Bilibili detail view"))))
    (bili-detail--start-playback surface page)))

(defun bili-detail-select-page ()
  "Select a video part in the current detail view without playing it."
  (interactive)
  (let* ((surface
          (or (bili-detail--current-surface)
              (user-error "Current buffer is not a Bilibili detail view")))
         (state (bili-detail--state surface))
         (video
          (bili-detail--model (appkit-surface-app surface) state)))
    (unless (bili-video-p video)
      (user-error "Current detail is not a video"))
    (let* ((pages (bili-video-pages video))
           (choices
            (mapcar (lambda (page)
                      (cons (format "P%d · %s"
                                    (bili-video-page-number page)
                                    (bili-video-page-title page))
                            page))
                    pages))
           (page (cdr (assoc (completing-read "Part: " choices nil t)
                             choices))))
      (appkit-surface-send
       surface (list 'select-page (bili-video-page-cid page))))))

(defun bili-detail-activate ()
  "Activate the detail action at point."
  (interactive)
  (let ((button
         (or (button-at (point))
             (and (> (point) (point-min))
                  (button-at (1- (point)))))))
    (unless button
      (user-error "No Bilibili detail action at point"))
    (button-activate button)))

(defun bili-detail-next-action ()
  "Move to the next action in the current detail view."
  (interactive)
  (unless (ignore-errors (forward-button 1 t t))
    (user-error "No later detail action")))

(defun bili-detail-previous-action ()
  "Move to the previous action in the current detail view."
  (interactive)
  (unless (ignore-errors (backward-button 1 t t))
    (user-error "No earlier detail action")))

(defun bili-detail-open-in-browser ()
  "Open the current Bilibili entity in the system browser."
  (interactive)
  (let* ((surface
          (or (bili-detail--current-surface)
              (user-error "Current buffer is not a Bilibili detail view")))
         (state (bili-detail--state surface)))
    (browse-url
     (pcase (plist-get state :kind)
       ('video (format "https://www.bilibili.com/video/%s"
                       (plist-get state :id)))
       ('live (format "https://live.bilibili.com/%s"
                      (plist-get state :id)))))))

(provide 'bili-detail)

;;; bili-detail.el ends here
