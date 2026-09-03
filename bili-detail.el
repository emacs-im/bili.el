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
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-projection)
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
  "View request-table key for entity metadata.")

(defconst bili-detail--playback-request-key 'playback
  "View request-table key for playback URL resolution.")

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

(defun bili-detail--state (view)
  "Return validated detail state owned by VIEW."
  (let ((state (and (appkit-view-p view) (appkit-view-state view))))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'detail)
                 (memq (plist-get state :kind) '(video live)))
      (error "Invalid Bilibili detail state"))
    state))

(defun bili-detail--current-view ()
  "Return the current live Bilibili detail view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'detail)))
    view))

(defun bili-detail--model (view state)
  "Return VIEW's canonical model selected by detail STATE."
  (pcase (plist-get state :kind)
    ('video (bili-core-video (appkit-view-app view) (plist-get state :id)))
    ('live (bili-core-room (appkit-view-app view) (plist-get state :id)))))

(defun bili-detail--cover-data (model)
  "Return stable entity key and cover URL for MODEL."
  (cond
   ((bili-video-p model)
    (cons (list 'video (bili-video-bvid model))
          (bili-video-cover model)))
   ((bili-live-room-p model)
    (cons (list 'live (bili-live-room-id model))
          (bili-live-room-cover model)))))

(defun bili-detail--prefetch-cover (view model)
  "Start MODEL's cover acquisition for VIEW's application."
  (when-let* ((data (bili-detail--cover-data model))
              (url (cdr data))
              ((not (string-empty-p url))))
    (bili-cover-prefetch (appkit-view-app view) (car data) url)))

(defun bili-detail--cover-row (model resource)
  "Return MODEL's cover projection row depending on RESOURCE."
  (let* ((data (bili-detail--cover-data model))
         (entity-key (car data))
         (url (cdr data)))
    (bili-detail--row
     '(cover) 'cover ""
     :entity-key entity-key :url url
     :dependencies
     (list resource (bili-cover-resource-key entity-key)))))

(defun bili-detail--new-state (kind id &optional model)
  "Return fresh detail state for KIND, ID, and optional MODEL."
  (let ((selected-cid
         (and (bili-video-p model) (bili-video-cid model))))
    (list :type 'detail :kind kind :id id
          :phase (if model 'ready 'initial)
          :message nil :loaded-p (and model t) :position-intent nil
          :selected-cid selected-cid
          :playback-phase 'idle :playback-message nil)))

(defun bili-detail--header-line ()
  "Return the persistent header line for the current detail view."
  (if-let* ((view (bili-detail--current-view))
            (state (bili-detail--state view)))
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

(defun bili-detail--rows (view state)
  "Return Appkit projection rows for VIEW and detail STATE."
  (let ((model (bili-detail--model view state))
        rows)
    (when model
      (bili-detail--prefetch-cover view model))
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
        :dependencies (plist-get entry :dependencies)))
     rows)))

(defun bili-detail--print-row (projection-row)
  "Render one Appkit PROJECTION-ROW."
  (let* ((entry (appkit-projection-row-payload projection-row))
         (key (appkit-projection-row-key projection-row))
         (type (plist-get entry :type))
         (text (plist-get entry :text))
         (face (plist-get entry :face))
         (start (point)))
    (pcase type
      ('title (appkit-view-insert-heading-line text :face 'bili-title-face))
      ('heading (appkit-view-insert-heading-line text :face 'bili-section-face))
      ('meta (appkit-view-insert-note-line text :face 'bili-meta-face))
      ('status (appkit-view-insert-heading-line text :face face))
      ('note (appkit-view-insert-note-line text :face (or face 'shadow)))
      ('error (appkit-view-insert-note-line text :face 'bili-error-face))
      ('cover
       (let* ((view (or (bili-detail--current-view)
                        (error "No live Bilibili detail view")))
              (image
               (bili-cover-detail-image
                view (plist-get entry :entity-key) (plist-get entry :url))))
         (if image
             (progn
               (appkit-media-insert-image-slices
                image nil nil "[cover]" (plist-get entry :url))
               (insert "\n"))
           (appkit-view-insert-note-line
            (pcase (plist-get
                    (bili-core-cover-state
                     (appkit-view-app view) (plist-get entry :entity-key))
                    :status)
              ('pending "Loading cover...")
              ('failed "Cover unavailable")
              (_ "Cover unavailable"))
            :face 'shadow))))
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

(defun bili-detail--sync (view invalidations _events)
  "Synchronize detail VIEW from INVALIDATIONS."
  (let* ((state (bili-detail--state view))
         (position (or (plist-get state :position-intent) 'preserve)))
    (setf (plist-get state :position-intent) nil)
    (with-current-buffer (appkit-view-buffer view)
      (appkit-projection-sync-invalidations
          view invalidations (bili-detail--rows view state)
        :reconcile-parts '(details)
        :position position)
      (force-mode-line-update))))


(defun bili-detail--request-failed (view state message)
  "Install metadata failure MESSAGE in VIEW and STATE."
  (setf (plist-get state :phase) 'error
        (plist-get state :message) message)
  (appkit-request-sync view :structure t :part 'details :position t))

(defun bili-detail--request-succeeded (view state model phase)
  "Install metadata MODEL in VIEW and STATE for request PHASE."
  (condition-case error-data
      (let ((app (appkit-view-app view)))
        (pcase (plist-get state :kind)
          ('video
           (unless (and (bili-video-p model)
                        (equal (bili-video-bvid model)
                               (plist-get state :id)))
             (error "Bilibili returned another video"))
           (bili-core-store-video app model)
           (unless (plist-get state :selected-cid)
             (setf (plist-get state :selected-cid) (bili-video-cid model))))
          ('live
           (unless (and (bili-live-room-p model)
                        (= (bili-live-room-id model) (plist-get state :id)))
             (error "Bilibili returned another live room"))
           (bili-core-store-room app model)))
        (bili-detail--prefetch-cover view model)
        (setf (plist-get state :phase) 'ready
              (plist-get state :message) nil
              (plist-get state :loaded-p) t
              (plist-get state :position-intent)
              (and (eq phase 'initial)
                   (if (and (eq (plist-get state :kind) 'live)
                            (not (= (bili-live-room-live-status model) 1)))
                       '(title)
                     '(action play))))
        (appkit-request-sync view :structure t :part 'details :position t))
    (error
     (bili-detail--request-failed
      view state (error-message-string error-data)))))

(defun bili-detail--start-request (view phase)
  "Start metadata request for VIEW in PHASE."
  (unless (memq phase '(initial refresh))
    (error "Invalid Bilibili detail phase: %S" phase))
  (let* ((state (bili-detail--state view))
         (operation
          (appkit-view-operation-begin view bili-detail--request-key)))
    (setf (plist-get state :phase) phase
          (plist-get state :message) nil)
    (appkit-request-sync view :structure t :part 'details :position t)
    (cl-labels
        ((success
          (model)
          (when (appkit-view-operation-finish operation)
            (bili-detail--request-succeeded view state model phase)))
         (failure
          (message)
          (when (appkit-view-operation-finish operation)
            (bili-detail--request-failed view state message))))
      (pcase (plist-get state :kind)
        ('video
         (bili-api-video
          (plist-get state :id)
          (lambda (data)
            (condition-case error-data
                (success (bili-model-video-from-json data))
              (error (failure (error-message-string error-data)))))
          :errback #'failure :owner operation))
        ('live
         (bili-live-resolve-room
          (plist-get state :id) #'success
          :errback #'failure :owner operation))))))

(defun bili-detail--setup (view)
  "Initialize projection for detail VIEW."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-projection-ensure
     view :printer #'bili-detail--print-row
     :anchor-property bili-detail-row-key-property
     :no-separator-p t))
  (appkit-view-enable-responsive-geometry view)
  (when-let* ((model (bili-detail--model view (bili-detail--state view))))
    (bili-detail--prefetch-cover view model))
  (appkit-invalidate view :structure t :part 'details :position t)
  (appkit-sync-invalidations view)
  (unless (plist-get (bili-detail--state view) :loaded-p)
    (bili-detail--start-request view 'initial)))

(defun bili-detail--open (kind id &optional model)
  "Open canonical detail KIND and ID, optionally seeded with MODEL."
  (let* ((app (bili-core-app))
         (view-id (list 'detail kind id))
         (existing (appkit-view-for-id app view-id))
         (state (or (and existing (appkit-view-state existing))
                    (bili-detail--new-state kind id model))))
    (when model
      (pcase kind
        ('video (bili-core-store-video app model))
        ('live (bili-core-store-room app model)))
      (setf (plist-get state :phase) 'ready
            (plist-get state :message) nil
            (plist-get state :loaded-p) t))
    (appkit-open-view
     :app app :id view-id :mode #'bili-detail-mode
     :buffer-name (format "*Bilibili %s: %s*" kind id)
     :state state :sync-function #'bili-detail--sync
     :parts '(details) :position-policy 'semantic
     :setup #'bili-detail--setup :select t)))

(defun bili-detail-open-video (bvid)
  "Open Bilibili video BVID details."
  (bili-detail--open 'video bvid (bili-core-video (bili-core-app) bvid)))

(defun bili-detail-open-live-room (room-id)
  "Resolve ROOM-ID, then open one canonical live-room detail view."
  (unless (and (integerp room-id) (> room-id 0))
    (error "Bilibili live room id must be positive"))
  (let* ((app (bili-core-app))
         (cached (bili-core-room app room-id)))
    (if cached
        (bili-detail--open 'live (bili-live-room-id cached) cached)
      (message "Resolving Bilibili live room %s..." room-id)
      (bili-live-resolve-room
       room-id
       (lambda (room)
         (when (appkit-app-live-p app)
           (bili-detail--open 'live (bili-live-room-id room) room)))
       :errback (lambda (message) (message "%s" message))
       :owner app))))

(defun bili-detail-refresh ()
  "Refresh the current Bilibili detail view."
  (interactive)
  (if-let* ((view (bili-detail--current-view)))
      (bili-detail--start-request view 'refresh)
    (user-error "Current buffer is not a Bilibili detail view")))


(defun bili-detail--selected-page (video state)
  "Return VIDEO page selected by detail STATE."
  (or (seq-find
       (lambda (page)
         (= (bili-video-page-cid page) (plist-get state :selected-cid)))
       (bili-video-pages video))
      (car (bili-video-pages video))))

(defun bili-detail--start-playback (view &optional page)
  "Start playback from VIEW, optionally selecting video PAGE."
  (let* ((state (bili-detail--state view))
         (model (bili-detail--model view state))
         operation)
    (unless model
      (user-error "Bilibili detail has not loaded"))
    (when (and (bili-live-room-p model)
               (/= (bili-live-room-live-status model) 1))
      (user-error "This live room has no supported live stream"))
    (when page
      (setf (plist-get state :selected-cid) (bili-video-page-cid page)))
    (setq operation
          (appkit-view-operation-begin
           view bili-detail--playback-request-key))
    (setf (plist-get state :playback-phase) 'resolving
          (plist-get state :playback-message) nil)
    (appkit-request-sync view :part 'details :entry '(action play) :position t)
    (cl-labels
        ((success
          (_buffer)
          (when (appkit-view-operation-finish operation)
            (setf (plist-get state :playback-phase) 'idle
                  (plist-get state :playback-message) nil)
            (appkit-request-sync view :part 'details :position t)))
         (failure
          (message)
          (when (appkit-view-operation-finish operation)
            (setf (plist-get state :playback-phase) 'error
                  (plist-get state :playback-message) message)
            (appkit-request-sync view :part 'details :position t))))
      (if (bili-video-p model)
          (bili-playback-video
           model operation
           :page (or page (bili-detail--selected-page model state))
           :callback #'success :errback #'failure)
        (bili-playback-live
         model operation :callback #'success :errback #'failure)))))

(defun bili-detail-open-comments ()
  "Open the read-only comment stream for the current video detail."
  (interactive)
  (let* ((view (or (bili-detail--current-view)
                   (user-error "Current buffer is not a Bilibili detail view")))
         (state (bili-detail--state view))
         (video (bili-detail--model view state)))
    (unless (bili-video-p video)
      (user-error "Current detail is not a video"))
    (bili-comment-open video)))

(defun bili-detail-play ()
  "Play the current Bilibili detail or selected video part."
  (interactive)
  (let ((view (or (bili-detail--current-view)
                  (user-error "Current buffer is not a Bilibili detail view"))))
    (bili-detail--start-playback view)))

(defun bili-detail--play-page-action (page)
  "Play video PAGE from the current detail view."
  (let ((view (or (bili-detail--current-view)
                  (user-error "Current buffer is not a Bilibili detail view"))))
    (bili-detail--start-playback view page)))

(defun bili-detail-select-page ()
  "Select a video part in the current detail view without playing it."
  (interactive)
  (let* ((view (or (bili-detail--current-view)
                   (user-error "Current buffer is not a Bilibili detail view")))
         (state (bili-detail--state view))
         (video (bili-detail--model view state)))
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
      (setf (plist-get state :selected-cid) (bili-video-page-cid page)
            (plist-get state :position-intent)
            (list 'page (bili-video-page-cid page)))
      (appkit-request-sync view :part 'details :position t))))

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
  (let* ((view (or (bili-detail--current-view)
                   (user-error "Current buffer is not a Bilibili detail view")))
         (state (bili-detail--state view)))
    (browse-url
     (pcase (plist-get state :kind)
       ('video (format "https://www.bilibili.com/video/%s"
                       (plist-get state :id)))
       ('live (format "https://live.bilibili.com/%s"
                      (plist-get state :id)))))))

(provide 'bili-detail)

;;; bili-detail.el ends here
