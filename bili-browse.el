;;; bili-browse.el --- Appkit Bilibili browsing views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own catalog view state, request generations, Appkit rich-card projections,
;; endpoint-aware pagination, and resource navigation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-scroll)
(require 'appkit-ui)
(require 'bili-api)
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

(defcustom bili-browse-auto-load-threshold 2000
  "Character distance from the visible catalog end that loads the next page.

Set this to nil to disable automatic pagination."
  :type '(choice (const :tag "Disable automatic pagination" nil)
                 integer)
  :group 'bili)

(defconst bili-browse--catalog-request-key 'catalog
  "View request-table key for a catalog page request.")

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

(defun bili-browse--catalog-state (view)
  "Return validated catalog state owned by VIEW."
  (let ((state (and (appkit-view-p view) (appkit-view-state view))))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'catalog)
                 (memq (plist-get state :kind) '(home search live))
                 (listp (plist-get state :items))
                 (integerp (plist-get state :page))
                 (integerp (plist-get state :generation)))
      (error "Invalid Bilibili catalog state"))
    state))

(defun bili-browse--current-catalog-view ()
  "Return the current live Bilibili catalog view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'catalog)))
    view))

(defun bili-browse--header-line ()
  "Return the persistent header line for the current catalog."
  (if-let* ((view (bili-browse--current-catalog-view))
            (state (bili-browse--catalog-state view)))
      (format " Bilibili · %s · %d items%s"
              (pcase (plist-get state :kind)
                ('home "Popular")
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
  (let ((dependencies (plist-get properties :dependencies)))
    (appkit-projection-row-create
     :key key
     :payload (append (list :type type) properties)
     :dependencies dependencies)))

(defun bili-browse--search-action ()
  "Prompt for a replacement Bilibili catalog search."
  (call-interactively #'bili-browse-search))

(defun bili-browse--catalog-project (view state)
  "Project catalog STATE for VIEW into stable Appkit rows."
  (let* ((app (appkit-view-app view))
         (kind (plist-get state :kind))
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
      (when-let* ((item (bili-core-catalog-item app key)))
        (bili-cover-prefetch app key (bili-catalog-item-cover item))
        (push
         (bili-browse--projection-row
          key 'item :entity-key key
          :dependencies
          (list (list 'catalog key)
                (bili-cover-resource-key key)))
         rows)))
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

(defun bili-browse--open-catalog-key (view key)
  "Open canonical catalog KEY owned by VIEW."
  (unless (appkit-view-live-p view)
    (user-error "Bilibili catalog view is no longer live"))
  (let ((item (bili-core-catalog-item (appkit-view-app view) key)))
    (unless (bili-catalog-item-p item)
      (error "Bilibili catalog item is unavailable"))
    (pcase (bili-catalog-item-kind item)
      ('video (bili-detail-open-video (bili-catalog-item-id item)))
      ('live (bili-detail-open-live-room (bili-catalog-item-id item)))
      (_ (error "Unsupported Bilibili catalog item")))))

(defun bili-browse--print-catalog-row (projection-row)
  "Render one catalog PROJECTION-ROW."
  (let* ((view (or (bili-browse--current-catalog-view)
                   (error "No live Bilibili catalog view")))
         (entry (appkit-projection-row-payload projection-row))
         (type (plist-get entry :type))
         (text (plist-get entry :text)))
    (pcase type
      ('section
       (appkit-view-insert-heading-line text :face 'bili-section-face))
      ('note
       (appkit-view-insert-note-line text :face 'bili-meta-face))
      ('error
       (appkit-view-insert-note-line text :face 'bili-error-face))
      ('action
       (insert "  ")
       (appkit-ui-insert-action-button
        (format " %s " text) (plist-get entry :action)
        :face 'bili-action-face :help-echo text)
       (insert "\n"))
      ('item
       (bili-render-insert-catalog-card
        view (plist-get entry :entity-key)))
      (_ (error "Unknown Bilibili catalog row type: %S" type)))))

(defun bili-browse--catalog-sync (view invalidations)
  "Synchronize catalog VIEW from INVALIDATIONS."
  (let* ((state (bili-browse--catalog-state view))
         (position (or (plist-get state :position-intent) 'preserve))
         (force-keys
          (and (memq 'geometry (appkit-invalidations-parts invalidations))
               (copy-sequence (plist-get state :items)))))
    (setf (plist-get state :position-intent) nil)
    (with-current-buffer (appkit-view-buffer view)
      (appkit-projection-sync
       view (bili-browse--catalog-project view state)
       :force-keys force-keys
       :changed-dependencies (appkit-invalidations-resource-keys invalidations)
       :position position)
      (force-mode-line-update)
      (when (appkit-scroll-observer-p bili-browse--scroll-observer)
        (appkit-scroll-observer-check bili-browse--scroll-observer)))))

(defun bili-browse-activate ()
  "Open the Bilibili catalog card at point."
  (interactive)
  (let* ((view (or (bili-browse--current-catalog-view)
                   (user-error "Current buffer is not a Bilibili catalog")))
         (key (or (get-text-property (point) bili-browse-item-key-property)
                  (and (> (point) (point-min))
                       (get-text-property
                        (1- (point)) bili-browse-item-key-property)))))
    (unless key
      (user-error "No Bilibili item at point"))
    (bili-browse--open-catalog-key view key)))

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

(defun bili-browse--catalog-request-current-p (view state token)
  "Return non-nil when TOKEN may still update catalog STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq token (plist-get state :request-token))))

(defun bili-browse--catalog-retire-request (view state token)
  "Retire VIEW's catalog transport when TOKEN still owns STATE."
  (when (bili-browse--catalog-request-current-p view state token)
    (remhash bili-browse--catalog-request-key
             (appkit-view-request-table view))))

(defun bili-browse--catalog-failed
    (view state token phase message &optional quiet)
  "Install catalog failure MESSAGE for PHASE when TOKEN owns VIEW and STATE.

When QUIET is non-nil, keep the failure in the view without echo-area noise."
  (when (bili-browse--catalog-request-current-p view state token)
    (setf (plist-get state :request-token) nil
          (plist-get state :phase) 'error
          (plist-get state :failed-phase) phase
          (plist-get state :message) message)
    (appkit-request-sync view :structure t :part 'catalog :position t)
    (unless quiet
      (message "%s" message))))

(defun bili-browse--catalog-exhausted-p
    (state data models new-keys phase page)
  "Return whether catalog STATE reached its endpoint-specific end.

DATA is the provider page, MODELS its normalized items, NEW-KEYS the keys not
already visible, PHASE the request phase, and PAGE the accepted page number."
  (or
   (null models)
   (and (eq phase 'older) (null new-keys))
   (pcase (plist-get state :kind)
     ('home (eq (alist-get 'no_more data) t))
     ('search
      (let ((pages (bili-model--number (alist-get 'numPages data))))
        (and (> pages 0) (>= page pages))))
     ('live nil)
     (_ t))))

(defun bili-browse--catalog-record-pagination (state data page)
  "Record endpoint pagination metadata from DATA at PAGE in STATE."
  (setf (plist-get state :total-items)
        (and (eq (plist-get state :kind) 'search)
             (bili-model--number (alist-get 'numResults data)))
        (plist-get state :total-pages)
        (and (eq (plist-get state :kind) 'search)
             (bili-model--number (alist-get 'numPages data)))
        (plist-get state :page) page))

(defun bili-browse--catalog-succeeded
    (view state token phase page data &optional quiet)
  "Install catalog DATA for PAGE and PHASE when TOKEN owns VIEW.

QUIET suppresses completion messages for automatic pagination."
  (when (bili-browse--catalog-request-current-p view state token)
    (condition-case error-data
        (let* ((models (bili-browse--catalog-item-list state data))
               (app (appkit-view-app view))
               (keys (bili-core-store-catalog-items app models))
               (current (plist-get state :items))
               (new (if (eq phase 'older)
                        (bili-browse--new-keys current keys)
                      keys)))
          (dolist (model models)
            (bili-cover-prefetch
             app
             (list (bili-catalog-item-kind model)
                   (bili-catalog-item-id model))
             (bili-catalog-item-cover model)))
          (bili-browse--catalog-record-pagination state data page)
          (setf (plist-get state :items)
                (if (eq phase 'older) (append current new) keys)
                (plist-get state :phase) 'ready
                (plist-get state :failed-phase) nil
                (plist-get state :message) nil
                (plist-get state :request-token) nil
                (plist-get state :loaded-p) t
                (plist-get state :position-intent)
                (and (eq phase 'initial) 'first)
                (plist-get state :exhausted-p)
                (bili-browse--catalog-exhausted-p
                 state data models new phase page))
          (appkit-request-sync
           view :structure t :part 'catalog :position t)
          (unless quiet
            (message (if (eq phase 'older)
                         "Loaded %d more Bilibili items"
                       "Loaded %d Bilibili items")
                     (length new))))
      (error
       (bili-browse--catalog-failed
        view state token phase (error-message-string error-data) quiet)))))

(defun bili-browse--cancel-catalog-request (view state)
  "Cancel VIEW's active catalog transport for STATE."
  (setf (plist-get state :request-token) nil)
  (bili-core-cancel-view-request
   view bili-browse--catalog-request-key #'bili-api-cancel))

(defun bili-browse--dispatch-catalog
    (view state page success failure)
  "Dispatch STATE's PAGE for VIEW using SUCCESS and FAILURE callbacks."
  (pcase (plist-get state :kind)
    ('home
     (bili-api-popular
      page success :page-size bili-browse-page-size
      :errback failure :owner view))
    ('search
     (bili-api-search-videos
      (plist-get state :query) page success
      :page-size bili-browse-page-size :errback failure :owner view))
    ('live
     (bili-api-live-list page success :errback failure :owner view))
    (_ (error "Unsupported Bilibili catalog kind"))))

(defun bili-browse--catalog-request (view phase &optional quiet)
  "Start catalog VIEW request for PHASE.

QUIET suppresses echo-area messages for automatic pagination."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Bilibili catalog request phase: %S" phase))
  (let ((state (bili-browse--catalog-state view)))
    (when (and (eq phase 'older) (plist-get state :exhausted-p))
      (user-error "No more Bilibili results"))
    (let ((page (if (eq phase 'older)
                    (1+ (plist-get state :page))
                  1))
          token
          callback-ran-p
          request)
      (bili-browse--cancel-catalog-request view state)
      (setq token (cl-incf (plist-get state :generation)))
      (setf (plist-get state :request-token) token
            (plist-get state :phase) phase
            (plist-get state :failed-phase) nil
            (plist-get state :message) nil)
      (appkit-request-sync view :structure t :part 'catalog :position t)
      (setq request
            (bili-browse--dispatch-catalog
             view state page
             (lambda (data)
               (setq callback-ran-p t)
               (bili-browse--catalog-retire-request view state token)
               (bili-browse--catalog-succeeded
                view state token phase page data quiet))
             (lambda (message)
               (setq callback-ran-p t)
               (bili-browse--catalog-retire-request view state token)
               (bili-browse--catalog-failed
                view state token phase message quiet))))
      (cond
       ((and request (not callback-ran-p)
             (bili-browse--catalog-request-current-p view state token))
        (puthash bili-browse--catalog-request-key request
                 (appkit-view-request-table view)))
       ((and (null request) (not callback-ran-p)
             (bili-browse--catalog-request-current-p view state token))
        (bili-browse--catalog-failed
         view state token phase
         "Bilibili catalog request did not start" quiet)))
      request)))

(defun bili-browse--maybe-auto-load
    (view _window position end)
  "Load VIEW's next page when visible POSITION approaches END."
  (when (and (appkit-view-live-p view)
             (numberp bili-browse-auto-load-threshold)
             (appkit-scroll-near-end-p
              position end bili-browse-auto-load-threshold))
    (let ((state (bili-browse--catalog-state view)))
      (when (and (plist-get state :loaded-p)
                 (eq (plist-get state :phase) 'ready)
                 (null (plist-get state :request-token))
                 (not (plist-get state :exhausted-p)))
        (bili-browse--catalog-request view 'older t)))))

(defun bili-browse--install-scroll-observer (view)
  "Install VIEW's lifecycle-owned automatic pagination observer."
  (setq-local
   bili-browse--scroll-observer
   (appkit-scroll-observer-install
    view
    :end-function
    (lambda (window position end)
      (bili-browse--maybe-auto-load view window position end)))))

(defun bili-browse--setup-catalog (view)
  "Initialize catalog VIEW and start its first request."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-projection-ensure
     view :printer #'bili-browse--print-catalog-row
     :anchor-property bili-browse-row-key-property
     :no-separator-p t)
    (appkit-view-enable-responsive-geometry view)
    (bili-browse--install-scroll-observer view))
  (appkit-invalidate view :structure t :part 'catalog :position t)
  (appkit-sync-invalidations view)
  (bili-browse--catalog-request view 'initial))

(defun bili-browse-refresh ()
  "Refresh the current Bilibili catalog."
  (interactive)
  (if-let* ((view (bili-browse--current-catalog-view)))
      (let ((state (bili-browse--catalog-state view)))
        (bili-browse--catalog-request
         view (if (plist-get state :loaded-p) 'refresh 'initial)))
    (user-error "Current buffer is not a Bilibili catalog")))

(defun bili-browse-retry ()
  "Retry the failed operation in the current Bilibili catalog."
  (interactive)
  (if-let* ((view (bili-browse--current-catalog-view))
            (state (bili-browse--catalog-state view))
            ((eq (plist-get state :phase) 'error))
            (phase (plist-get state :failed-phase)))
      (bili-browse--catalog-request view phase)
    (user-error "Current Bilibili catalog has no failed request")))

(defun bili-browse--make-catalog-state (kind &optional query)
  "Return fresh catalog state for KIND and optional QUERY."
  (list :type 'catalog :kind kind :query query :items nil :page 0
        :total-items nil :total-pages nil
        :phase 'initial :failed-phase nil :message nil
        :request-token nil :generation 0
        :loaded-p nil :exhausted-p nil :position-intent nil))

(defun bili-browse--open-catalog (id buffer-name state)
  "Open catalog ID in BUFFER-NAME with STATE."
  (let ((view
         (appkit-open-view
          :app (bili-core-app) :id id :mode #'bili-browse-mode
          :buffer-name buffer-name :state state
          :sync-function #'bili-browse--catalog-sync
          :parts '(catalog geometry) :position-policy 'semantic
          :setup #'bili-browse--setup-catalog :select t)))
    (with-current-buffer (appkit-view-buffer view)
      (appkit-view-refresh-responsive-geometry :force t))
    view))

(defun bili-browse-home ()
  "Open or reuse the popular-video catalog."
  (interactive)
  (let* ((app (bili-core-app))
         (id '(catalog home))
         (existing (appkit-view-for-id app id))
         (state (or (and existing (appkit-view-state existing))
                    (bili-browse--make-catalog-state 'home))))
    (bili-browse--open-catalog id "*Bilibili Popular*" state)))

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
    (let* ((app (bili-core-app))
           (id (list 'catalog 'search normalized))
           (existing (appkit-view-for-id app id))
           (state (or (and existing (appkit-view-state existing))
                      (bili-browse--make-catalog-state 'search normalized)))
           (display-query (truncate-string-to-width normalized 48 nil nil "…")))
      (bili-browse--open-catalog
       id (format "*Bilibili Search: %s*" display-query) state))))

(defun bili-browse-edit-search ()
  "Prompt for a new search, seeded from the current search when available."
  (interactive)
  (let* ((view (bili-browse--current-catalog-view))
         (state (and view (bili-browse--catalog-state view)))
         (initial (and (eq (plist-get state :kind) 'search)
                       (plist-get state :query))))
    (bili-browse-search
     (read-string "Bilibili search: " initial
                  'bili-browse-search-history))))

(defun bili-browse-live ()
  "Open or reuse the recommended live-room catalog."
  (interactive)
  (let* ((app (bili-core-app))
         (id '(catalog live))
         (existing (appkit-view-for-id app id))
         (state (or (and existing (appkit-view-state existing))
                    (bili-browse--make-catalog-state 'live))))
    (bili-browse--open-catalog id "*Bilibili Recommended Live*" state)))
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
