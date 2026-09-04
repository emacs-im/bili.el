;;; bili-comment.el --- Bilibili video comments  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read-only, cursor-paginated video comments backed by canonical Appkit state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-chat-avatar)
(require 'appkit-discussion)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-scroll)
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
  "View request-table key for a comment page request.")


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

(define-derived-mode bili-comment-mode special-mode "Bilibili-Comments"
  "Major mode for a read-only Bilibili video comment stream."
  (setq-local truncate-lines nil
              word-wrap t
              switch-to-buffer-preserve-window-point nil
              header-line-format '(:eval (bili-comment--header-line)))
  (when (fboundp 'appkit-ui-buffer-substring-filter)
    (setq-local filter-buffer-substring-function
                #'appkit-ui-buffer-substring-filter)))

(defun bili-comment--state (view)
  "Return validated comment state owned by VIEW."
  (let ((state (and (appkit-view-p view) (appkit-view-state view))))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'comments)
                 (integerp (plist-get state :aid))
                 (> (plist-get state :aid) 0)
                 (stringp (plist-get state :bvid))
                 (listp (plist-get state :items)))
      (error "Invalid Bilibili comment state"))
    state))

(defun bili-comment--current-view ()
  "Return the current live Bilibili comment view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'comments)))
    view))

(defun bili-comment--header-line ()
  "Return the persistent header line for the current comment view."
  (if-let* ((view (bili-comment--current-view))
            (state (bili-comment--state view)))
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
   :dependencies (plist-get properties :dependencies)))

(defun bili-comment--avatar-key (aid comment)
  "Return stable avatar key for video AID and COMMENT."
  (list 'comment-avatar aid (bili-comment-id comment)))

(defun bili-comment--dependencies (app aid root-id comment)
  "Prefetch COMMENT's avatar and return its APP dependencies.

AID identifies the video; ROOT-ID identifies COMMENT's canonical root."
  (let ((avatar-key (bili-comment--avatar-key aid comment))
        (avatar-url (bili-comment-avatar comment)))
    (unless (string-empty-p avatar-url)
      (bili-cover-prefetch app avatar-key avatar-url))
    (list (list 'comment aid root-id)
          (bili-cover-resource-key avatar-key))))

(defun bili-comment--project (view state)
  "Project comment STATE for VIEW into stable Appkit discussion rows."
  (let* ((app (appkit-view-app view))
         (aid (plist-get state :aid))
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
      (when-let* ((comment (bili-core-comment app aid id)))
        (let* ((root-key (list 'comment aid id))
               (replies (bili-comment-replies comment))
               (dependency (bili-comment--dependencies app aid id comment)))
          (push
           (bili-comment--projection-row
            (list 'root id) 'comment
            :comment comment :entry-key root-key
            :depth 0 :connector (and replies 'continue)
            :dependencies dependency)
           rows)
          (cl-loop for reply in replies
                   for tail on replies
                   do
                   (push
                    (bili-comment--projection-row
                     (list 'reply id (bili-comment-id reply)) 'comment
                     :comment reply
                     :entry-key
                     (list 'comment aid id (bili-comment-id reply))
                     :parent-key root-key :depth 1
                     :connector (if (cdr tail) 'continue 'end)
                     :dependencies
                     (bili-comment--dependencies app aid id reply))
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

(defun bili-comment--insert-comment (view entry)
  "Insert projected comment ENTRY for VIEW through `appkit-discussion'."
  (let* ((comment (plist-get entry :comment))
         (message (bili-comment-message comment))
         (pixel-size (appkit-chat-avatar-two-line-pixel-size))
         (avatar
          (bili-cover-avatar-image
           view
           (bili-comment--avatar-key
            (plist-get (bili-comment--state view) :aid) comment)
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
     :width (or (appkit-view-responsive-width) 80)
     :avatar-pixel-size pixel-size)))

(defun bili-comment--insert-action (text action)
  "Insert display-indented action TEXT invoking ACTION."
  (let ((start (point)))
    (appkit-ui-insert-action-button
     (format " %s " text) action
     :face 'bili-action-face :help-echo text)
    (insert "\n")
    (appkit-ui-apply-line-prefix start (point) "    ")))

(defun bili-comment--print-row (projection-row)
  "Render one comment PROJECTION-ROW."
  (let* ((view (or (bili-comment--current-view)
                   (error "No live Bilibili comment view")))
         (entry (appkit-projection-row-payload projection-row))
         (type (plist-get entry :type))
         (text (plist-get entry :text)))
    (pcase type
      ('comment
       (bili-comment--insert-comment view entry))
      ('note
       (appkit-presentation-insert-note-line text :face 'bili-meta-face))
      ('error
       (appkit-presentation-insert-note-line text :face 'bili-error-face))
      ('action
       (bili-comment--insert-action text (plist-get entry :action)))
      (_ (error "Unknown Bilibili comment row type: %S" type)))))

(defun bili-comment--sync (view invalidations _events)
  "Synchronize comment VIEW from INVALIDATIONS."
  (let* ((state (bili-comment--state view))
         (position (or (plist-get state :position-intent) 'preserve)))
    (setf (plist-get state :position-intent) nil)
    (with-current-buffer (appkit-view-buffer view)
      (appkit-projection-sync-invalidations
          view invalidations (bili-comment--project view state)
        :reconcile-parts '(comments)
        :position position)
      (force-mode-line-update))))

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


(defun bili-comment--failed (view state phase message &optional quiet)
  "Install request failure MESSAGE for PHASE in VIEW and STATE."
  (setf (plist-get state :phase) 'error
        (plist-get state :failed-phase) phase
        (plist-get state :message) message)
  (appkit-request-sync view :structure t :part 'comments :position t)
  (unless quiet
    (message "%s" message)))

(defun bili-comment--succeeded (view state phase data)
  "Install comment DATA for PHASE in VIEW and STATE."
  (condition-case error-data
      (let* ((cursor (alist-get 'cursor data))
             (models (bili-comment--response-models
                      data (not (eq phase 'older))))
             (app (appkit-view-app view))
             (ids (bili-core-store-comments
                   app (plist-get state :aid) models))
             (current (plist-get state :items))
             (new (if (eq phase 'older)
                      (bili-comment--new-ids current ids)
                    ids))
             (pagination (alist-get 'pagination_reply cursor))
             (next-offset (alist-get 'next_offset pagination)))
        (unless (listp cursor)
          (error "Bilibili comment response has no cursor"))
        (setf (plist-get state :items)
              (if (eq phase 'older) (append current new) ids)
              (plist-get state :cursor)
              (and (stringp next-offset)
                   (not (string-empty-p next-offset))
                   next-offset)
              (plist-get state :total)
              (bili-model--number (alist-get 'all_count cursor))
              (plist-get state :phase) 'ready
              (plist-get state :failed-phase) nil
              (plist-get state :message) nil
              (plist-get state :loaded-p) t
              (plist-get state :position-intent)
              (and (eq phase 'initial) 'first)
              (plist-get state :exhausted-p)
              (or (eq (alist-get 'is_end cursor) t)
                  (null models)
                  (and (eq phase 'older) (null new))
                  (null next-offset)))
        (appkit-request-sync
         view :structure t :part 'comments :position t))
    (error
     (bili-comment--failed
      view state phase (error-message-string error-data) t))))

(defun bili-comment--request (view phase &optional quiet)
  "Start comment VIEW request for PHASE.

QUIET suppresses echo-area messages for automatic pagination."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Bilibili comment request phase: %S" phase))
  (let ((state (bili-comment--state view)))
    (when (and (eq phase 'older) (plist-get state :exhausted-p))
      (user-error "No more Bilibili comments"))
    (let* ((offset (and (eq phase 'older) (plist-get state :cursor)))
           (operation
            (appkit-view-operation-begin view bili-comment--request-key)))
      (setf (plist-get state :phase) phase
            (plist-get state :failed-phase) nil
            (plist-get state :message) nil)
      (appkit-request-sync view :structure t :part 'comments :position t)
      (bili-api-video-comments
       (plist-get state :aid)
       (lambda (data)
         (when (appkit-view-operation-finish operation)
           (bili-comment--succeeded view state phase data)))
       :offset offset
       :errback
       (lambda (message)
         (when (appkit-view-operation-finish operation)
           (bili-comment--failed view state phase message quiet)))
       :owner operation))))

(defun bili-comment--maybe-auto-load (view _window position end)
  "Load VIEW's next comment page when POSITION approaches END."
  (when (and (appkit-view-live-p view)
             (numberp bili-comment-auto-load-threshold)
             (appkit-scroll-near-end-p
              position end bili-comment-auto-load-threshold))
    (let ((state (bili-comment--state view)))
      (when (and (plist-get state :loaded-p)
                 (eq (plist-get state :phase) 'ready)
                 (not (plist-get state :exhausted-p)))
        (bili-comment--request view 'older t)))))

(defun bili-comment--setup (view)
  "Initialize comment VIEW and start its first request."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-projection-ensure
     view :printer #'bili-comment--print-row
     :anchor-property bili-comment-row-key-property
     :no-separator-p t)
    (appkit-view-enable-responsive-geometry view)
    (setq-local
     bili-comment--scroll-observer
     (appkit-scroll-observer-install
      view
      :end-function
      (lambda (window position end)
        (bili-comment--maybe-auto-load view window position end)))))
  (appkit-invalidate view :structure t :part 'comments :position t)
  (appkit-sync-invalidations view)
  (bili-comment--request view 'initial))

(defun bili-comment-refresh ()
  "Refresh the current Bilibili comment stream."
  (interactive)
  (if-let* ((view (bili-comment--current-view)))
      (let ((state (bili-comment--state view)))
        (bili-comment--request
         view (if (plist-get state :loaded-p) 'refresh 'initial)))
    (user-error "Current buffer is not a Bilibili comment view")))

(defun bili-comment-retry ()
  "Retry the failed request in the current Bilibili comment stream."
  (interactive)
  (if-let* ((view (bili-comment--current-view))
            (state (bili-comment--state view))
            ((eq (plist-get state :phase) 'error))
            (phase (plist-get state :failed-phase)))
      (bili-comment--request view phase)
    (user-error "Current Bilibili comments have no failed request")))


(defun bili-comment--new-state (video)
  "Return fresh comment view state for VIDEO."
  (list :type 'comments
        :aid (bili-video-aid video)
        :bvid (bili-video-bvid video)
        :items nil :cursor nil :total nil
        :phase 'initial :failed-phase nil :message nil
        :loaded-p nil :exhausted-p nil :position-intent nil))

(defun bili-comment-open (video)
  "Open or reuse the read-only comment stream for VIDEO."
  (unless (and (bili-video-p video) (> (bili-video-aid video) 0))
    (error "Invalid Bilibili video for comments"))
  (let* ((app (bili-core-app))
         (aid (bili-video-aid video))
         (id (list 'comments aid))
         (existing (appkit-view-for-id app id))
         (state (or (and existing (appkit-view-state existing))
                    (bili-comment--new-state video))))
    (appkit-open-view
     :app app :id id :mode #'bili-comment-mode
     :buffer-name (format "*Bilibili Comments: %s*" (bili-video-bvid video))
     :state state :sync-function #'bili-comment--sync
     :parts '(comments geometry) :position-policy 'semantic
     :setup #'bili-comment--setup :select t)))

(provide 'bili-comment)

;;; bili-comment.el ends here
