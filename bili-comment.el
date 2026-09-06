;;; bili-comment.el --- Bilibili video comments  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read-only, cursor-paginated video comments backed by canonical Appkit state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-command)
(require 'appkit-discussion)
(require 'appkit-effect)
(require 'appkit-projection)
(require 'appkit-resource)
(require 'appkit-scroll)
(require 'appkit-surface)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'bili-cover)
(require 'bili-api)
(require 'bili-core)
(require 'bili-model)
(require 'bili-render)

(defcustom bili-comment-auto-load-threshold 400
  "Character distance from visible comment end that loads the next page.

Set this to nil to disable automatic pagination."
  :type '(choice (const :tag "Disable automatic pagination" nil)
                 integer)
  :group 'bili)

(defconst bili-comment--request-key 'comments
  "App Effect key prefix for comment page requests.")


(defconst bili-comment-row-key-property 'bili-comment-row-key
  "Text property carrying a stable comment projection row key.")

(defvar-local bili-comment--scroll-observer nil
  "Appkit-owned automatic pagination observer for this comment view.")

(defvar-keymap bili-comment-mode-map
  :doc "Keymap for a Bilibili video comment view."
  :parent special-mode-map
  "n" #'appkit-discussion-next-entry
  "p" #'appkit-discussion-previous-entry
  "g" #'bili-comment-refresh
  "b" #'quit-window
  "q" #'quit-window
  "?" #'describe-mode)

(define-derived-mode bili-comment-mode appkit-discussion-mode "Bilibili-Comments"
  "Major mode for a read-only Bilibili video comment stream."
  (setq-local switch-to-buffer-preserve-window-point nil
              header-line-format '(:eval (bili-comment--header-line)))
  (when (fboundp 'appkit-ui-buffer-substring-filter)
    (setq-local filter-buffer-substring-function
                #'appkit-ui-buffer-substring-filter)))

(defun bili-comment--state (owner)
  "Return validated comment state from OWNER."
  (let ((state (if (appkit-surface-p owner)
                   (appkit-surface-model owner)
                 owner)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'comments)
                 (integerp (plist-get state :aid))
                 (> (plist-get state :aid) 0)
                 (stringp (plist-get state :bvid))
                 (listp (plist-get state :items)))
      (error "Invalid Bilibili comment state"))
    state))

(defun bili-comment--current-surface ()
  "Return the current live Bilibili comment Surface, or nil."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface))
              (state (appkit-surface-model surface))
              ((eq (plist-get state :type) 'comments)))
    surface))

(defun bili-comment--header-line ()
  "Return the persistent header line for the current comment Surface."
  (if-let* ((surface (bili-comment--current-surface))
            (state (bili-comment--state surface)))
      (format " Bilibili · Comments · %s · %d%s%s"
              (plist-get state :bvid)
              (length (plist-get state :items))
              (if-let* ((total (plist-get state :total)))
                  (format "/%d" total)
                "")
              (pcase (plist-get state :phase)
                ('initial " · loading")
                ('refresh " · refreshing")
                ('older " · loading")
                ('error " · error")
                (_ "")))
    " Bilibili · Comments"))

(defun bili-comment--projection-row (key type &rest properties)
  "Return one comment projection row with KEY, TYPE, and PROPERTIES."
  (appkit-projection-row-create
   :key key
   :payload (append (list :type type) properties)
   :dependencies (plist-get properties :dependencies)
   :resource-demands (plist-get properties :resource-demands)))

(defun bili-comment--avatar-key (aid comment)
  "Return stable avatar key for video AID and COMMENT."
  (list 'comment-avatar aid (bili-comment-id comment)))

(defun bili-comment--resources (aid root-id comment)
  "Return dependencies and avatar demands for COMMENT under ROOT-ID."
  (let* ((avatar-key (bili-comment--avatar-key aid comment))
         (avatar-url (bili-comment-avatar comment))
         (demand (bili-cover-demand avatar-key avatar-url))
         (resource-key
          (and demand (appkit-resource-demand-key demand))))
    (cons
     (delq nil (list (list 'comment aid root-id) resource-key))
     (and demand (list demand)))))

(defun bili-comment--project (_surface app-read-view state)
  "Project comment STATE against APP-READ-VIEW into stable rows."
  (let* ((aid (plist-get state :aid))
         (items (plist-get state :items))
         (phase (plist-get state :phase))
         rows)
    (when (and (null items) (eq phase 'initial))
      (push (bili-comment--projection-row '(status) 'note :text "Loading...")
            rows))
    (when (eq phase 'error)
      (push
       (bili-comment--projection-row
        '(status) 'error
        :text (format "Unable to load comments: %s"
                      (plist-get state :message)))
       rows)
      (push
       (bili-comment--projection-row
        '(retry) 'action :text "Retry" :action #'bili-comment-retry)
       rows))
    (dolist (id items)
      (when-let* ((comment (bili-core-comment app-read-view aid id)))
        (let* ((root-key (list 'comment aid id))
               (replies (bili-comment-replies comment))
               (resources (bili-comment--resources aid id comment)))
          (push
           (bili-comment--projection-row
            (list 'root id) 'comment
            :comment comment :entry-key root-key
            :depth 0 :connector (and replies 'continue)
            :dependencies (car resources)
            :resource-demands (cdr resources))
           rows)
          (cl-loop for reply in replies
                   for tail on replies
                   for reply-resources =
                   (bili-comment--resources aid id reply)
                   do
                   (push
                    (bili-comment--projection-row
                     (list 'reply id (bili-comment-id reply)) 'comment
                     :comment reply
                     :entry-key
                     (list 'comment aid id (bili-comment-id reply))
                     :parent-key root-key :depth 1
                     :connector (if (cdr tail) 'continue 'end)
                     :dependencies (car reply-resources)
                     :resource-demands (cdr reply-resources))
                    rows)))))
    (unless (or items (memq phase '(initial refresh error)))
      (push
       (bili-comment--projection-row
        '(empty) 'note :text "No comments.")
       rows))
    (nreverse rows)))

(defun bili-comment--timestamp (seconds)
  "Return compact local time for epoch SECONDS, or empty text."
  (if (and (numberp seconds) (> seconds 0))
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time seconds))
    ""))

(defun bili-comment--author-label (comment)
  "Return a factual heading label for COMMENT."
  (let ((author (bili-comment-author comment))
        (author-id (bili-comment-author-id comment)))
    (cond
     ((not (string-empty-p author)) author)
     ((> author-id 0) (format "UID %s" author-id))
     (t ""))))

(defun bili-comment--avatar-fallback (comment)
  "Return one-character avatar fallback for COMMENT."
  (let ((author (bili-comment--author-label comment)))
    (if (string-empty-p author) "@" (substring author 0 1))))

(defun bili-comment--footer-text (comment)
  "Return Appkit discussion footer text for COMMENT."
  (string-join
   (delq nil
         (list
          (format "%s likes"
                  (bili-render-count (bili-comment-likes comment)))
          (when (> (bili-comment-reply-count comment) 0)
            (format "%s replies"
                    (bili-render-count
                     (bili-comment-reply-count comment))))))
   " · "))

(defun bili-comment--insert-comment (surface entry)
  "Insert projected comment ENTRY for SURFACE through `appkit-discussion'."
  (let* ((comment (plist-get entry :comment))
         (message (bili-comment-message comment))
         (pixel-size (appkit-chat-avatar-two-line-pixel-size))
         (avatar
          (bili-cover-avatar-image
           surface
           (bili-comment--avatar-key
            (plist-get (bili-comment--state surface) :aid) comment)
           (bili-comment-avatar comment)
           pixel-size))
         (body-inserter
          (unless (string-empty-p message)
            (lambda (prefix properties)
              (appkit-ui-insert-prefixed-lines
               prefix message :properties properties)))))
    (unless (bili-comment-p comment)
      (error "Bilibili discussion row lost its canonical comment"))
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key (plist-get entry :entry-key)
      :parent-key (plist-get entry :parent-key)
      :depth (plist-get entry :depth)
      :avatar avatar
      :avatar-fallback (bili-comment--avatar-fallback comment)
      :heading (bili-comment--author-label comment)
      :heading-face 'bili-title-face
      :time (bili-comment--timestamp (bili-comment-created-at comment))
      :time-face 'bili-meta-face
      :body-inserter body-inserter
      :footer (bili-comment--footer-text comment)
      :footer-face 'bili-meta-face
      :connector (plist-get entry :connector))
     :width (or (appkit-surface-responsive-width surface) 80)
     :avatar-pixel-size pixel-size)))

(defun bili-comment--insert-action (text action)
  "Insert display-indented action TEXT invoking ACTION."
  (let ((start (point)))
    (appkit-ui-insert-action-button
     (format " %s " text) action
     :face 'bili-action-face :help-echo text)
    (insert "\n")
    (appkit-ui-apply-line-prefix start (point) "    ")))

(defun bili-comment--print-row
    (surface _app-read-view projection-row)
  "Render one comment PROJECTION-ROW for SURFACE."
  (let* ((entry (appkit-projection-row-payload projection-row))
         (type (plist-get entry :type))
         (text (plist-get entry :text)))
    (pcase type
      ('comment
       (bili-comment--insert-comment surface entry))
      ('note
       (appkit-presentation-insert-note-line text :face 'bili-meta-face))
      ('error
       (appkit-presentation-insert-note-line text :face 'bili-error-face))
      ('action
       (bili-comment--insert-action text (plist-get entry :action)))
      (_ (error "Unknown Bilibili comment row type: %S" type)))))


(defun bili-comment--new-ids (current candidates)
  "Return CANDIDATES not already present in CURRENT."
  (let ((seen (make-hash-table :test #'eql))
        result)
    (dolist (id current)
      (puthash id t seen))
    (dolist (id candidates (nreverse result))
      (unless (gethash id seen)
        (puthash id t seen)
        (push id result)))))

(defun bili-comment--response-models (data initial-p)
  "Return normalized root comments from DATA.

When INITIAL-P is non-nil, prepend provider-pinned comments."
  (let* ((top (and initial-p (alist-get 'top_replies data)))
         (top-list
          (cond
           ((null top) nil)
           ((alist-get 'rpid top) (list top))
           ((listp top) top)
           (t nil)))
         (raw (append top-list (alist-get 'replies data)))
         (seen (make-hash-table :test #'eql))
         models)
    (dolist (entry raw (nreverse models))
      (when-let* ((model (bili-model-comment-from-json entry))
                  (id (bili-comment-id model))
                  ((not (gethash id seen))))
        (puthash id t seen)
        (push model models)))))


(defun bili-comment--render-change (&optional position)
  "Return a full comment render request restoring POSITION."
  (appkit-projection-change-create
   :full-p t :frame-p t :position (or position 'preserve)))

(defun bili-comment--reply-command (route message)
  "Return a report-delivery command sending MESSAGE to ROUTE."
  (appkit-command-post-message
   :target route :message message :delivery 'report))

(defun bili-comment--page-effect
    (context token aid phase offset)
  "Return App Effect loading one comment page from CONTEXT."
  (let ((source (appkit-transition-context-source-address context))
        (route (appkit-transition-context-reply-route context)))
    (unless (and source route)
      (error "Comment request lacks a live Surface reply route"))
    (appkit-effect-create
     :key (list bili-comment--request-key source)
     :input (list token aid phase offset route)
     :start
     (lambda (_effect-context input _observe resolve reject)
       (pcase-let ((`(,_token ,request-aid ,_phase
                                ,request-offset ,_route)
                    input))
         (bili-api-effect-cancellation
          (bili-api-video-comments
           request-aid resolve
           :offset request-offset
           :errback reject
           :owner bili-api--effect-owner))))
     :success
     (lambda (input data)
       (list 'comments 'transport-succeeded input data))
     :failure
     (lambda (input reason)
       (list 'comments 'transport-failed input (format "%s" reason)))
     :cancellation-requirement 'transport)))

(defun bili-comment--app-succeeded (model input data)
  "Commit comment DATA into App MODEL and reply using INPUT."
  (pcase-let ((`(,token ,aid ,phase ,_offset ,route) input))
    (condition-case condition
        (let* ((cursor (alist-get 'cursor data))
               (_ (unless (listp cursor)
                    (error "Bilibili comment response has no cursor")))
               (models (bili-comment--response-models
                        data (not (eq phase 'older))))
               (stored (bili-core--put-comments model aid models))
               (next-model (car stored))
               (ids (cdr stored))
               (pagination (alist-get 'pagination_reply cursor))
               (raw-offset (alist-get 'next_offset pagination))
               (next-offset
                (and (stringp raw-offset)
                     (not (string-empty-p raw-offset))
                     raw-offset))
               (metadata
                (list :cursor next-offset
                      :total
                      (bili-model--number (alist-get 'all_count cursor))
                      :provider-exhausted
                      (or (eq (alist-get 'is_end cursor) t)
                          (null models)
                          (null next-offset)))))
          (appkit-next
           :model next-model
           :render appkit-render-none
           :commands
           (list
            (bili-comment--reply-command
             route
             (list 'comments 'succeeded
                   token phase ids metadata)))))
      (error
       (appkit-next
        :model model
        :render appkit-render-none
        :commands
        (list
         (bili-comment--reply-command
          route
          (list 'comments 'failed token phase
                (error-message-string condition)))))))))

(defun bili-comment--app-update (context model message)
  "Advance canonical comment state in MODEL for MESSAGE."
  (pcase message
    (`(comments request ,token ,aid ,phase ,offset)
     (if (and token
              (integerp aid) (> aid 0)
              (memq phase '(initial refresh older)))
         (appkit-next
          :model model
          :render appkit-render-none
          :commands
          (list
           (appkit-command-start-effect
            (bili-comment--page-effect
             context token aid phase offset))))
       (appkit-next-reject "Invalid Bilibili comment request")))
    (`(comments transport-succeeded ,input ,data)
     (bili-comment--app-succeeded model input data))
    (`(comments transport-failed
       (,token ,_aid ,phase ,_offset ,route) ,reason)
     (appkit-next
      :model model
      :render appkit-render-none
      :commands
      (list
       (bili-comment--reply-command
        route (list 'comments 'failed token phase reason)))))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili comment message: %S" message)))))

(defun bili-comment--request-command
    (context state phase token offset)
  "Return command requesting a comment page from STATE's App."
  (appkit-command-post-message
   :target (appkit-transition-context-parent-address context)
   :message
   (list 'comments 'request token
         (plist-get state :aid) phase offset)
   :delivery 'report
   :reply-correlation token))

(defun bili-comment--surface-init (context input)
  "Initialize one comment Surface from INPUT."
  (let* ((state (copy-sequence (bili-comment--state input)))
         (token (make-symbol "bili-comment-request-")))
    (setf (plist-get state :request-token) token)
    (appkit-next
     :model state
     :render (bili-comment--render-change 'first)
     :commands
     (list
      (bili-comment--request-command
       context state 'initial token nil)))))

(defun bili-comment--surface-request (context state phase)
  "Transition comment STATE into request PHASE."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Bilibili comment request phase: %S" phase))
  (if (and (eq phase 'older) (plist-get state :exhausted-p))
      (appkit-next-reject "No more Bilibili comments")
    (let ((next (copy-sequence state))
          (token (make-symbol "bili-comment-request-"))
          (offset (and (eq phase 'older)
                       (plist-get state :cursor))))
      (setf (plist-get next :phase) phase
            (plist-get next :failed-phase) nil
            (plist-get next :message) nil
            (plist-get next :request-token) token)
      (appkit-next
       :model next
       :render (bili-comment--render-change)
       :commands
       (list
        (bili-comment--request-command
         context next phase token offset))))))

(defun bili-comment--surface-succeeded
    (state token phase ids metadata)
  "Install one comment response in STATE when TOKEN remains current."
  (if (not (eq token (plist-get state :request-token)))
      (appkit-next :model state :render appkit-render-none)
    (let* ((next (copy-sequence state))
           (current (plist-get state :items))
           (new (if (eq phase 'older)
                    (bili-comment--new-ids current ids)
                  ids)))
      (setf (plist-get next :items)
            (if (eq phase 'older) (append current new) ids)
            (plist-get next :cursor) (plist-get metadata :cursor)
            (plist-get next :total) (plist-get metadata :total)
            (plist-get next :phase) 'ready
            (plist-get next :failed-phase) nil
            (plist-get next :message) nil
            (plist-get next :loaded-p) t
            (plist-get next :request-token) nil
            (plist-get next :exhausted-p)
            (or (plist-get metadata :provider-exhausted)
                (and (eq phase 'older) (null new))))
      (appkit-next
       :model next
       :render
       (bili-comment--render-change
        (and (eq phase 'initial) 'first))))))

(defun bili-comment--surface-failed
    (state token phase reason)
  "Install comment failure REASON when TOKEN remains current."
  (if (not (eq token (plist-get state :request-token)))
      (appkit-next :model state :render appkit-render-none)
    (let ((next (copy-sequence state)))
      (setf (plist-get next :phase) 'error
            (plist-get next :failed-phase) phase
            (plist-get next :message) reason
            (plist-get next :request-token) nil)
      (appkit-next
       :model next :render (bili-comment--render-change)))))

(defun bili-comment--surface-update (context state message)
  "Advance comment Surface STATE for MESSAGE."
  (pcase message
    (`(request ,phase)
     (bili-comment--surface-request context state phase))
    (`(comments succeeded ,token ,phase ,ids ,metadata)
     (bili-comment--surface-succeeded
      state token phase ids metadata))
    (`(comments failed ,token ,phase ,reason)
     (bili-comment--surface-failed state token phase reason))
    ('geometry
     (appkit-next
      :model state
      :render (appkit-projection-change-create :geometry-p t)))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili comment Surface message: %S"
                message)))))

(defun bili-comment--maybe-auto-load
    (surface _window position end)
  "Request SURFACE's next page when POSITION approaches END."
  (when (and (appkit-surface-live-p surface)
             (numberp bili-comment-auto-load-threshold)
             (appkit-scroll-near-end-p
              position end bili-comment-auto-load-threshold))
    (let ((state (bili-comment--state surface)))
      (when (and (plist-get state :loaded-p)
                 (eq (plist-get state :phase) 'ready)
                 (not (plist-get state :exhausted-p)))
        (appkit-surface-post surface '(request older))))))

(defun bili-comment--setup (surface)
  "Install lifecycle-owned geometry and scroll observers for SURFACE."
  (with-current-buffer (appkit-surface-buffer surface)
    (appkit-surface-enable-responsive-geometry
     surface
     (lambda (owner _width)
       (when (appkit-surface-live-p owner)
         (appkit-surface-post owner 'geometry))))
    (setq-local
     bili-comment--scroll-observer
     (appkit-scroll-observer-install
      surface
      :end-function
      (lambda (window position end)
        (bili-comment--maybe-auto-load
         surface window position end))))
    (appkit-surface-refresh-responsive-geometry surface)))

(defconst bili-comment--surface-type
  (appkit-surface-type-create
   :name 'bili-comments
   :mode #'bili-comment-mode
   :init #'bili-comment--surface-init
   :update #'bili-comment--surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-projection-renderer-create
      :project-all #'bili-comment--project
      :printer #'bili-comment--print-row
      :anchor-property bili-comment-row-key-property
      :geometry-mode 'reproject
      :no-separator-p t)))
  "Generated Surface type for Bilibili comments.")

(defun bili-comment--request (surface phase)
  "Synchronously request PHASE from comment SURFACE."
  (appkit-surface-send surface (list 'request phase)))

(defun bili-comment-refresh ()
  "Refresh the current Bilibili comment stream."
  (interactive)
  (if-let* ((surface (bili-comment--current-surface)))
      (let ((state (bili-comment--state surface)))
        (bili-comment--request
         surface (if (plist-get state :loaded-p) 'refresh 'initial)))
    (user-error "Current buffer is not a Bilibili comment view")))

(defun bili-comment-retry ()
  "Retry the failed request in the current Bilibili comment stream."
  (interactive)
  (if-let* ((surface (bili-comment--current-surface))
            (state (bili-comment--state surface))
            ((eq (plist-get state :phase) 'error))
            (phase (plist-get state :failed-phase)))
      (bili-comment--request surface phase)
    (user-error "Current Bilibili comments have no failed request")))

(defun bili-comment--new-state (video)
  "Return fresh comment Surface state for VIDEO."
  (list :type 'comments
        :aid (bili-video-aid video)
        :bvid (bili-video-bvid video)
        :items nil :cursor nil :total nil
        :phase 'initial :failed-phase nil :message nil
        :loaded-p nil :exhausted-p nil :request-token nil))

(defun bili-comment-open (video)
  "Open or reuse the read-only comment Surface for VIDEO."
  (unless (and (bili-video-p video) (> (bili-video-aid video) 0))
    (error "Invalid Bilibili video for comments"))
  (let* ((app (bili-core-app))
         (aid (bili-video-aid video))
         (id (list 'comments aid))
         (existing (appkit-app-surface app id)))
    (if (appkit-surface-live-p existing)
        (progn
          (pop-to-buffer (appkit-surface-buffer existing))
          existing)
      (let ((surface
             (appkit-open-generated-surface
              bili-comment--surface-type
              :app app :identity id
              :input (bili-comment--new-state video)
              :buffer-name
              (format "*Bilibili Comments: %s*"
                      (bili-video-bvid video))
              :select t)))
        (bili-comment--setup surface)
        surface))))

(provide 'bili-comment)

;;; bili-comment.el ends here
