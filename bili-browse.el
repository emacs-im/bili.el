;;; bili-browse.el --- Appkit Bilibili browsing views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own catalog Surface state, rich-card projections, endpoint-aware
;; pagination, and declarative App effects and Resource demands.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-command)
(require 'appkit-effect)
(require 'appkit-projection)
(require 'appkit-resource)
(require 'appkit-scroll)
(require 'appkit-surface)
(require 'appkit-ui)
(require 'bili-api)
(require 'bili-auth)
(require 'bili-cover)
(require 'bili-core)
(require 'bili-live)
(require 'bili-model)
(require 'bili-detail)
(require 'bili-render)

(defcustom bili-browse-page-size 20
  "Number of videos requested in one popular or search page."
  :type '(integer 1 50)
  :group 'bili)

(defcustom bili-browse-recommended-page-size 20
  "Number of entries requested in one personalized recommendation page."
  :type '(integer 1 30)
  :group 'bili)

(defcustom bili-browse-auto-load-threshold 2000
  "Character distance from the visible catalog end that loads the next page.

Set this to nil to disable automatic pagination."
  :type '(choice (const :tag "Disable automatic pagination" nil)
          integer)
  :group 'bili)

(defconst bili-browse--catalog-request-key 'catalog
  "App Effect key prefix for catalog page requests.")

(defconst bili-browse-item-key-property 'bili-item-key
  "Text property carrying a canonical Bilibili catalog key.")

(defconst bili-browse-row-key-property 'bili-catalog-row-key
  "Text property carrying a stable catalog projection row key.")

(defvar bili-browse-search-history nil
  "Minibuffer history for Bilibili video searches.")

(defvar-local bili-browse--scroll-observer nil
  "Appkit-owned automatic pagination observer for this catalog.")

(defvar-keymap bili-browse-mode-map
  :doc "Keymap for Bilibili media catalogs."
  :parent special-mode-map
  "RET" #'bili-browse-activate
  "n" #'bili-browse-next-item
  "p" #'bili-browse-previous-item
  "g" #'bili-browse-refresh
  "h" #'bili-browse-home
  "f" #'bili-browse-recommended
  "/" #'bili-browse-search
  "e" #'bili-browse-edit-search
  "l" #'bili-browse-live
  "o" #'bili-browse-open-url-command
  "b" #'quit-window
  "q" #'quit-window
  "?" #'describe-mode)

(define-derived-mode bili-browse-mode special-mode "Bilibili-Browse"
  "Major mode for Bilibili video and live-room card catalogs."
  (setq-local switch-to-buffer-preserve-window-point nil
              truncate-lines t
              header-line-format '(:eval (bili-browse--header-line))))

(defun bili-browse--catalog-state (owner)
  "Return validated catalog state from OWNER."
  (let ((state (if (appkit-surface-p owner)
                   (appkit-surface-model owner)
                 owner)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'catalog)
                 (memq (plist-get state :kind)
                       '(home recommended search live))
                 (listp (plist-get state :items))
                 (integerp (plist-get state :page)))
      (error "Invalid Bilibili catalog state"))
    state))

(defun bili-browse--current-catalog-surface ()
  "Return the current live Bilibili catalog Surface, or nil."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface))
              (state (appkit-surface-model surface))
              ((eq (plist-get state :type) 'catalog)))
    surface))

(defun bili-browse--header-line ()
  "Return the persistent header line for the current catalog."
  (if-let* ((surface (bili-browse--current-catalog-surface))
            (state (bili-browse--catalog-state surface)))
      (format " Bilibili · %s · %d items%s"
              (pcase (plist-get state :kind)
                ('home "Popular")
                ('recommended "For you")
                ('live "Recommended live")
                ('search (format "Search “%s”" (plist-get state :query))))
              (length (plist-get state :items))
              (pcase (plist-get state :phase)
                ('initial " · loading")
                ('refresh " · refreshing")
                ('older " · loading")
                ('error " · error")
                (_ "")))
    " Bilibili"))

(defun bili-browse--catalog-item-list (state data)
  "Normalize catalog response DATA according to STATE."
  (pcase (plist-get state :kind)
    ('home
     (let ((items (alist-get 'list data)))
       (unless (listp items)
         (error "Bilibili popular response has no video list"))
       (delq nil (mapcar #'bili-model-video-catalog-item items))))
    ('recommended
     (unless (> (bili-model--number (alist-get 'mid data)) 0)
       (error "Bilibili personalized recommendations require login"))
     (let ((items (alist-get 'item data)))
       (unless (listp items)
         (error "Bilibili recommendation response has no item list"))
       (delq nil (mapcar #'bili-model-recommended-catalog-item items))))
    ('search
     (let ((items (alist-get 'result data)))
       (unless (listp items)
         (error "Bilibili search response has no result list"))
       (delq nil (mapcar #'bili-model-video-catalog-item items))))
    ('live (bili-live-catalog-items data))
    (_ (error "Unsupported Bilibili catalog kind"))))

(defun bili-browse--new-keys (current candidates)
  "Return CANDIDATES not already present in CURRENT."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (key current)
      (puthash key t seen))
    (dolist (key candidates (nreverse result))
      (unless (gethash key seen)
        (puthash key t seen)
        (push key result)))))

(defun bili-browse--projection-row (key type &rest properties)
  "Return one catalog projection row with KEY, TYPE, and PROPERTIES."
  (appkit-projection-row-create
   :key key
   :payload (append (list :type type) properties)
   :dependencies (plist-get properties :dependencies)
   :resource-demands (plist-get properties :resource-demands)))

(defun bili-browse--search-action ()
  "Prompt for a replacement Bilibili catalog search."
  (call-interactively #'bili-browse-search))

(defun bili-browse--catalog-project (_surface app-read-view state)
  "Project catalog STATE against APP-READ-VIEW into stable rows."
  (let* ((kind (plist-get state :kind))
         (section-key (list 'catalog kind (plist-get state :query)))
         (phase (plist-get state :phase))
         (items (plist-get state :items))
         rows)
    (when (and (null items) (eq phase 'initial))
      (push
       (bili-browse--projection-row
        (list section-key 'status) 'note :text "Loading...")
       rows))
    (when (eq phase 'error)
      (push
       (bili-browse--projection-row
        (list section-key 'status) 'error
        :text
        (format "Unable to load: %s" (plist-get state :message)))
       rows)
      (push
       (bili-browse--projection-row
        (list section-key 'retry) 'action
        :text "Retry" :action #'bili-browse-retry)
       rows))
    (dolist (key items)
      (when-let* ((item (bili-core-catalog-item app-read-view key)))
        (let* ((cover (bili-catalog-item-cover item))
               (demand (bili-cover-demand key cover))
               (resource-key
                (and demand (appkit-resource-demand-key demand))))
          (push
           (bili-browse--projection-row
            key 'item :entity-key key
            :dependencies
            (delq nil (list (list 'catalog key) resource-key))
            :resource-demands (and demand (list demand)))
           rows))))
    (unless (or items (memq phase '(initial refresh error)))
      (push
       (bili-browse--projection-row
        (list section-key 'empty) 'note
        :text
        (if (eq kind 'search)
            (format "No videos match “%s”." (plist-get state :query))
          "No results."))
       rows)
      (push
       (bili-browse--projection-row
        (list section-key 'empty-action) 'action
        :text (if (eq kind 'search) "Change search" "Reload")
        :action (if (eq kind 'search)
                    #'bili-browse--search-action
                  #'bili-browse-refresh))
       rows))
    (nreverse rows)))

(defun bili-browse--open-catalog-key (surface key)
  "Open canonical catalog KEY owned by SURFACE."
  (unless (appkit-surface-live-p surface)
    (user-error "Bilibili catalog Surface is no longer live"))
  (let ((item (bili-core-catalog-item
               (appkit-surface-app surface) key)))
    (unless (bili-catalog-item-p item)
      (error "Bilibili catalog item is unavailable"))
    (pcase (bili-catalog-item-kind item)
      ('video (bili-detail-open-video (bili-catalog-item-id item)))
      ('live (bili-detail-open-live-room (bili-catalog-item-id item)))
      (_ (error "Unsupported Bilibili catalog item")))))

(defun bili-browse--print-catalog-row
    (surface app-read-view projection-row)
  "Render PROJECTION-ROW for SURFACE against APP-READ-VIEW."
  (let* ((entry (appkit-projection-row-payload projection-row))
         (type (plist-get entry :type))
         (text (plist-get entry :text)))
    (pcase type
      ('section
       (appkit-presentation-insert-heading-line text :face 'bili-section-face))
      ('note
       (appkit-presentation-insert-note-line text :face 'bili-meta-face))
      ('error
       (appkit-presentation-insert-note-line text :face 'bili-error-face))
      ('action
       (insert "  ")
       (appkit-ui-insert-action-button
        (format " %s " text) (plist-get entry :action)
        :face 'bili-action-face :help-echo text)
       (insert "\n"))
      ('item
       (bili-render-insert-catalog-card
        surface app-read-view (plist-get entry :entity-key)))
      (_ (error "Unknown Bilibili catalog row type: %S" type)))))

(defun bili-browse-activate ()
  "Open the Bilibili catalog card at point."
  (interactive)
  (let* ((surface
          (or (bili-browse--current-catalog-surface)
              (user-error "Current buffer is not a Bilibili catalog")))
         (key (or (get-text-property (point) bili-browse-item-key-property)
                  (and (> (point) (point-min))
                       (get-text-property
                        (1- (point)) bili-browse-item-key-property)))))
    (unless key
      (user-error "No Bilibili item at point"))
    (bili-browse--open-catalog-key surface key)))

(defun bili-browse--item-starts ()
  "Return ordered buffer positions at the start of every catalog card."
  (let ((position (point-min))
        previous
        starts)
    (while (< position (point-max))
      (let ((key (get-text-property position bili-browse-item-key-property)))
        (when (and key (not (equal key previous)))
          (push position starts))
        (setq previous key
              position
              (or (next-single-property-change
                   position bili-browse-item-key-property nil (point-max))
                  (point-max)))))
    (nreverse starts)))

(defun bili-browse--move-item (delta)
  "Move point by DELTA catalog cards."
  (let* ((starts (bili-browse--item-starts))
         (current (or (seq-position starts (point))
                      (1- (seq-count (lambda (position)
                                       (<= position (point)))
                                     starts))))
         (target (+ (max -1 current) delta)))
    (unless (and (>= target 0) (< target (length starts)))
      (user-error "No %s Bilibili item"
                  (if (> delta 0) "next" "previous")))
    (goto-char (nth target starts))))

(defun bili-browse-next-item (&optional count)
  "Move forward COUNT Bilibili catalog cards."
  (interactive "p")
  (bili-browse--move-item (or count 1)))

(defun bili-browse-previous-item (&optional count)
  "Move backward COUNT Bilibili catalog cards."
  (interactive "p")
  (bili-browse--move-item (- (or count 1))))

(defun bili-browse--render-change (&optional position)
  "Return a full catalog render request restoring POSITION."
  (appkit-projection-change-create
   :full-p t :frame-p t :position (or position 'preserve)))

(defun bili-browse--provider-exhausted-p (kind data models page)
  "Return whether KIND's provider DATA is exhausted at PAGE."
  (or
   (null models)
   (pcase kind
     ('home (eq (alist-get 'no_more data) t))
     ('recommended nil)
     ('search
      (let ((pages (bili-model--number (alist-get 'numPages data))))
        (and (> pages 0) (>= page pages))))
     ('live nil)
     (_ t))))

(defun bili-browse--dispatch-catalog
    (kind query page success failure)
  "Dispatch KIND and QUERY at PAGE through SUCCESS or FAILURE."
  (pcase kind
    ('home
     (bili-api-popular
      page success :page-size bili-browse-page-size
      :errback failure :owner bili-api--effect-owner))
    ('recommended
     (bili-api-recommended-feed
      page success :page-size bili-browse-recommended-page-size
      :errback failure :owner bili-api--effect-owner))
    ('search
     (bili-api-search-videos
      query page success :page-size bili-browse-page-size
      :errback failure :owner bili-api--effect-owner))
    ('live
     (bili-api-live-list
      page success :errback failure :owner bili-api--effect-owner))
    (_ (error "Unsupported Bilibili catalog kind"))))

(defun bili-browse--catalog-effect
    (context token kind query phase page)
  "Return App Effect serving one catalog request from CONTEXT."
  (let ((source (appkit-transition-context-source-address context))
        (route (appkit-transition-context-reply-route context)))
    (unless (and source route)
      (error "Catalog request lacks a live Surface reply route"))
    (appkit-effect-create
     :key (list bili-browse--catalog-request-key source)
     :input (list token kind query phase page route)
     :start
     (lambda (_effect-context input _observe resolve reject)
       (pcase-let ((`(,_token ,request-kind ,request-query
                      ,_phase ,request-page ,_route)
                    input))
         (bili-api-effect-cancellation
          (bili-browse--dispatch-catalog
           request-kind request-query request-page resolve reject))))
     :success
     (lambda (input data)
       (list 'catalog 'transport-succeeded input data))
     :failure
     (lambda (input reason)
       (list 'catalog 'transport-failed input (format "%s" reason)))
     :cancellation-requirement 'transport)))

(defun bili-browse--reply-command (route message)
  "Return a report-delivery command sending MESSAGE to ROUTE."
  (appkit-command-post-message
   :target route :message message :delivery 'report))

(defun bili-browse--app-succeeded (model input data)
  "Commit catalog DATA into App MODEL and reply using INPUT."
  (pcase-let ((`(,token ,kind ,query ,phase ,page ,route) input))
    (condition-case condition
        (let* ((state (list :kind kind :query query))
               (models (bili-browse--catalog-item-list state data))
               (stored (bili-core--put-catalog-items model models))
               (next-model (car stored))
               (keys (cdr stored))
               (metadata
                (list
                 :total-items
                 (and (eq kind 'search)
                      (bili-model--number (alist-get 'numResults data)))
                 :total-pages
                 (and (eq kind 'search)
                      (bili-model--number (alist-get 'numPages data)))
                 :provider-exhausted
                 (bili-browse--provider-exhausted-p
                  kind data models page))))
          (appkit-next
           :model next-model
           :render appkit-render-none
           :commands
           (list
            (bili-browse--reply-command
             route
             (list 'catalog 'succeeded token phase page keys metadata)))))
      (error
       (appkit-next
        :model model
        :render appkit-render-none
        :commands
        (list
         (bili-browse--reply-command
          route
          (list 'catalog 'failed token phase
                (error-message-string condition)))))))))

(defun bili-browse--app-update (context model message)
  "Advance canonical catalog state in MODEL for MESSAGE."
  (pcase message
    (`(catalog request ,token ,kind ,query ,phase ,page)
     (if (and token
              (memq kind '(home recommended search live))
              (memq phase '(initial refresh older))
              (integerp page) (> page 0))
         (appkit-next
          :model model
          :render appkit-render-none
          :commands
          (list
           (appkit-command-start-effect
            (bili-browse--catalog-effect
             context token kind query phase page))))
       (appkit-next-reject "Invalid Bilibili catalog request")))
    (`(catalog transport-succeeded ,input ,data)
     (bili-browse--app-succeeded model input data))
    (`(catalog transport-failed
       (,token ,_kind ,_query ,phase ,_page ,route) ,reason)
     (appkit-next
      :model model
      :render appkit-render-none
      :commands
      (list
       (bili-browse--reply-command
        route (list 'catalog 'failed token phase reason)))))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili catalog message: %S" message)))))

(defun bili-browse--request-command (context state phase token page)
  "Return command requesting STATE's PAGE and PHASE from its App."
  (appkit-command-post-message
   :target (appkit-transition-context-parent-address context)
   :message
   (list 'catalog 'request token
         (plist-get state :kind) (plist-get state :query)
         phase page)
   :delivery 'report
   :reply-correlation token))

(defun bili-browse--surface-init (context input)
  "Initialize a catalog Surface from INPUT."
  (let* ((state (copy-sequence (bili-browse--catalog-state input)))
         (token (make-symbol "bili-catalog-request-")))
    (setf (plist-get state :request-token) token)
    (appkit-next
     :model state
     :render (bili-browse--render-change 'first)
     :commands
     (list
      (bili-browse--request-command context state 'initial token 1)))))

(defun bili-browse--surface-request (context state phase)
  "Transition catalog STATE into request PHASE."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Bilibili catalog request phase: %S" phase))
  (if (and (eq phase 'older) (plist-get state :exhausted-p))
      (appkit-next-reject "No more Bilibili results")
    (let* ((next (copy-sequence state))
           (page (if (eq phase 'older)
                     (1+ (plist-get state :page))
                   1))
           (token (make-symbol "bili-catalog-request-")))
      (setf (plist-get next :phase) phase
            (plist-get next :failed-phase) nil
            (plist-get next :message) nil
            (plist-get next :request-token) token)
      (appkit-next
       :model next
       :render (bili-browse--render-change)
       :commands
       (list
        (bili-browse--request-command
         context next phase token page))))))

(defun bili-browse--surface-succeeded
    (state token phase page keys metadata)
  "Install one catalog response in STATE when TOKEN remains current."
  (if (not (eq token (plist-get state :request-token)))
      (appkit-next :model state :render appkit-render-none)
    (let* ((next (copy-sequence state))
           (current (plist-get state :items))
           (new (if (eq phase 'older)
                    (bili-browse--new-keys current keys)
                  keys)))
      (setf (plist-get next :items)
            (if (eq phase 'older) (append current new) keys)
            (plist-get next :page) page
            (plist-get next :total-items)
            (plist-get metadata :total-items)
            (plist-get next :total-pages)
            (plist-get metadata :total-pages)
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
       (bili-browse--render-change
        (and (eq phase 'initial) 'first))))))

(defun bili-browse--surface-failed (state token phase reason)
  "Install catalog failure REASON when TOKEN remains current."
  (if (not (eq token (plist-get state :request-token)))
      (appkit-next :model state :render appkit-render-none)
    (let ((next (copy-sequence state)))
      (setf (plist-get next :phase) 'error
            (plist-get next :failed-phase) phase
            (plist-get next :message) reason
            (plist-get next :request-token) nil)
      (appkit-next
       :model next :render (bili-browse--render-change)))))

(defun bili-browse--surface-update (context state message)
  "Advance catalog Surface STATE for MESSAGE."
  (pcase message
    (`(request ,phase)
     (bili-browse--surface-request context state phase))
    (`(catalog succeeded ,token ,phase ,page ,keys ,metadata)
     (bili-browse--surface-succeeded
      state token phase page keys metadata))
    (`(catalog failed ,token ,phase ,reason)
     (bili-browse--surface-failed state token phase reason))
    ('geometry
     (appkit-next
      :model state
      :render (appkit-projection-change-create :geometry-p t)))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili catalog Surface message: %S"
                message)))))

(defun bili-browse--maybe-auto-load
    (surface _window position end)
  "Request SURFACE's next page when visible POSITION approaches END."
  (when (and (appkit-surface-live-p surface)
             (numberp bili-browse-auto-load-threshold)
             (appkit-scroll-near-end-p
              position end bili-browse-auto-load-threshold))
    (let ((state (bili-browse--catalog-state surface)))
      (when (and (plist-get state :loaded-p)
                 (eq (plist-get state :phase) 'ready)
                 (not (plist-get state :exhausted-p)))
        (appkit-surface-post surface '(request older))))))

(defun bili-browse--setup-catalog (surface)
  "Install lifecycle-owned geometry and scroll observers for SURFACE."
  (with-current-buffer (appkit-surface-buffer surface)
    (appkit-surface-enable-responsive-geometry
     surface
     (lambda (owner _width)
       (when (appkit-surface-live-p owner)
         (appkit-surface-post owner 'geometry))))
    (setq-local
     bili-browse--scroll-observer
     (appkit-scroll-observer-install
      surface
      :end-function
      (lambda (window position end)
        (bili-browse--maybe-auto-load
         surface window position end))))
    (appkit-surface-refresh-responsive-geometry surface)))

(defconst bili-browse--surface-type
  (appkit-surface-type-create
   :name 'bili-catalog
   :mode #'bili-browse-mode
   :init #'bili-browse--surface-init
   :update #'bili-browse--surface-update
   :renderer-factory
   (lambda (_surface)
     (appkit-projection-renderer-create
      :project-all #'bili-browse--catalog-project
      :printer #'bili-browse--print-catalog-row
      :anchor-property bili-browse-row-key-property
      :geometry-mode 'redraw
      :no-separator-p t)))
  "Generated Surface type for Bilibili catalogs.")

(defun bili-browse--catalog-request (surface phase)
  "Synchronously request PHASE from catalog SURFACE."
  (appkit-surface-send surface (list 'request phase)))

(defun bili-browse-refresh ()
  "Refresh the current Bilibili catalog."
  (interactive)
  (if-let* ((surface (bili-browse--current-catalog-surface)))
      (let ((state (bili-browse--catalog-state surface)))
        (bili-browse--catalog-request
         surface (if (plist-get state :loaded-p) 'refresh 'initial)))
    (user-error "Current buffer is not a Bilibili catalog")))

(defun bili-browse-retry ()
  "Retry the failed operation in the current Bilibili catalog."
  (interactive)
  (if-let* ((surface (bili-browse--current-catalog-surface))
            (state (bili-browse--catalog-state surface))
            ((eq (plist-get state :phase) 'error))
            (phase (plist-get state :failed-phase)))
      (bili-browse--catalog-request surface phase)
    (user-error "Current Bilibili catalog has no failed request")))

(defun bili-browse--make-catalog-state (kind &optional query)
  "Return fresh catalog state for KIND and optional QUERY."
  (list :type 'catalog :kind kind :query query :items nil :page 0
        :total-items nil :total-pages nil
        :phase 'initial :failed-phase nil :message nil
        :loaded-p nil :exhausted-p nil :request-token nil))

(defun bili-browse--open-catalog (id buffer-name state)
  "Open or select catalog Surface ID in BUFFER-NAME with STATE."
  (let* ((app (bili-core-app))
         (existing (appkit-app-surface app id)))
    (if (appkit-surface-live-p existing)
        (progn
          (pop-to-buffer (appkit-surface-buffer existing))
          existing)
      (let ((surface
             (appkit-open-generated-surface
              bili-browse--surface-type
              :app app :identity id :input state
              :buffer-name buffer-name :select t)))
        (bili-browse--setup-catalog surface)
        surface))))

(defun bili-browse-recommended ()
  "Open or reuse the logged-in account's personalized recommendation feed."
  (interactive)
  (bili-auth-credentials)
  (bili-browse--open-catalog
   '(catalog recommended) "*Bilibili For You*"
   (bili-browse--make-catalog-state 'recommended)))

(defun bili-browse-home ()
  "Open or reuse the popular-video catalog."
  (interactive)
  (bili-browse--open-catalog
   '(catalog home) "*Bilibili Popular*"
   (bili-browse--make-catalog-state 'home)))

(defun bili-browse--normalize-query (query)
  "Return QUERY trimmed and collapsed for stable view identity."
  (replace-regexp-in-string
   "[[:space:]\n\r]+" " " (string-trim (or query ""))))

(defun bili-browse-search (query)
  "Open or reuse the Bilibili video search for QUERY."
  (interactive
   (list (read-string "Bilibili search: " nil
                      'bili-browse-search-history)))
  (let ((normalized (bili-browse--normalize-query query)))
    (when (string-empty-p normalized)
      (user-error "Bilibili search query cannot be empty"))
    (let ((display-query
           (truncate-string-to-width normalized 48 nil nil "…")))
      (bili-browse--open-catalog
       (list 'catalog 'search normalized)
       (format "*Bilibili Search: %s*" display-query)
       (bili-browse--make-catalog-state 'search normalized)))))

(defun bili-browse-edit-search ()
  "Prompt for a new search, seeded from the current search when available."
  (interactive)
  (let* ((surface (bili-browse--current-catalog-surface))
         (state (and surface (bili-browse--catalog-state surface)))
         (initial (and (eq (plist-get state :kind) 'search)
                       (plist-get state :query))))
    (bili-browse-search
     (read-string "Bilibili search: " initial
                  'bili-browse-search-history))))

(defun bili-browse-live ()
  "Open or reuse the recommended live-room catalog."
  (interactive)
  (bili-browse--open-catalog
   '(catalog live) "*Bilibili Recommended Live*"
   (bili-browse--make-catalog-state 'live)))

(defun bili-browse-open-url (input)
  "Open Bilibili URL or identifier INPUT in its owning detail view."
  (pcase (bili-model-parse-location input)
    (`(video . ,bvid) (bili-detail-open-video bvid))
    (`(live . ,room-id) (bili-detail-open-live-room room-id))))

(defun bili-browse-open-url-command (input)
  "Prompt for and open Bilibili location INPUT."
  (interactive (list (read-string "Bilibili URL, BV id, or live room: ")))
  (bili-browse-open-url input))

(provide 'bili-browse)

;;; bili-browse.el ends here
