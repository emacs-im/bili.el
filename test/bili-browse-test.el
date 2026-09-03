;;; bili-browse-test.el --- Tests for Bilibili Appkit views  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-browse)

(defun bili-browse-test--video-json ()
  "Return one valid Bilibili video detail object."
  '((bvid . "BV1xx411c7mD")
    (aid . 2)
    (title . "Test video")
    (desc . "A useful description")
    (duration . 9)
    (owner . ((name . "Uploader")))
    (stat . ((view . 10) (danmaku . 3) (like . 4)))
    (pages . (((cid . 42) (page . 1) (part . "Part one"))))))

(ert-deftest bili-browse-home-uses-owned-four-line-card-projection ()
  (let (view buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'bili-api-popular)
                   (lambda (page callback &rest _arguments)
                     (should (= page 1))
                     (funcall
                      callback
                      `((no_more . t)
                        (list . (,(bili-browse-test--video-json)))))))
                  ((symbol-function 'message) #'ignore))
          (setq view (bili-browse-home)
                buffer (appkit-view-buffer view))
          (should (appkit-view-live-p view))
          (should (eq (plist-get (appkit-view-state view) :phase) 'ready))
          (appkit-sync-invalidations view)
          (should (= (length (plist-get (appkit-view-state view) :items)) 1))
          (with-current-buffer buffer
            (should (derived-mode-p 'bili-browse-mode))
            (should (appkit-projection-view-p view))
            (should (string-match-p "Test video" (buffer-string)))
            (should-not (string-match-p "RET details" (buffer-string)))
            (should-not (string-match-p "more available" (buffer-string)))
            (let* ((key (car (plist-get (appkit-view-state view) :items)))
                   (start
                    (text-property-any
                     (point-min) (point-max)
                     bili-browse-item-key-property key))
                   (end
                    (and start
                         (next-single-property-change
                          start bili-browse-item-key-property nil
                          (point-max)))))
              (dolist (property '(mouse-face keymap local-map follow-link))
                (should-not (get-text-property start property)))
              (should start)
              (should (eq (char-after start) ?T))
              (save-excursion
                (goto-char start)
                (dotimes (_ 4)
                  (should
                   (stringp (get-text-property (point) 'line-prefix)))
                  (forward-line 1)))
              (save-excursion
                (goto-char start)
                (should (search-forward "00:09" end t))
                (let ((display
                       (get-text-property
                        (1- (match-beginning 0)) 'display)))
                  (should (eq (car-safe display) 'space))
                  (should (eq (nth 1 display) :align-to))))
              (should (= (count-lines start end) 4)))))
      (when (appkit-view-live-p view)
        (appkit-kill-view view t))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-browse-reuses-normalized-search-view ()
  (let (first second buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'bili-api-search-videos)
                   (lambda (query page callback &rest keys)
                     (should (equal query "Emacs tips"))
                     (should (= page 1))
                     (should (= (plist-get keys :page-size) 20))
                     (funcall
                      callback
                      `((numPages . 1)
                        (numResults . 1)
                        (result . (,(bili-browse-test--video-json)))))))
                  ((symbol-function 'message) #'ignore))
          (setq first (bili-browse-search "  Emacs\n tips ")
                buffer (appkit-view-buffer first)
                second (bili-browse-search "Emacs tips"))
          (should (eq first second))
          (should
           (equal (appkit-view-id first)
                  '(catalog search "Emacs tips")))
          (appkit-sync-invalidations first)
          (with-current-buffer buffer
            (should (string-match-p "Search “Emacs tips”"
                                    (bili-browse--header-line)))))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-browse-canonical-catalog-update-invalidates-other-view ()
  (let (home search home-buffer search-buffer)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-api-popular)
              (lambda (_page callback &rest _keys)
                (funcall
                 callback
                 `((no_more . t)
                   (list . (,(bili-browse-test--video-json)))))))
             ((symbol-function 'bili-api-search-videos)
              (lambda (_query _page callback &rest _keys)
                (funcall
                 callback
                 '((numPages . 1)
                   (numResults . 1)
                   (result
                    . (((bvid . "BV1xx411c7mD")
                        (title . "Updated canonical title")
                        (author . "Another uploader")
                        (play . 99)
                        (duration . "00:09"))))))))
             ((symbol-function 'message) #'ignore))
          (setq home (bili-browse-home)
                home-buffer (appkit-view-buffer home))
          (appkit-sync-invalidations home)
          (with-current-buffer home-buffer
            (should (string-match-p "Test video" (buffer-string))))
          (setq search (bili-browse-search "same video")
                search-buffer (appkit-view-buffer search))
          (appkit-sync-invalidations home)
          (with-current-buffer home-buffer
            (should (string-match-p "Updated canonical title"
                                    (buffer-string)))))
      (bili-core-stop)
      (dolist (buffer (list home-buffer search-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest bili-browse-personalized-feed-is-distinct-and-account-gated ()
  (let ((first-page
         '(((goto . "av")
            (bvid . "BV1feed")
            (title . "Personal video")
            (owner . ((name . "Uploader")))
            (stat . ((view . 12) (danmaku . 3)))
            (duration . 61))
           ((goto . "ad") (title . "Advertisement"))
           ((goto . "live")
            (id . 99)
            (title . "Personal live")
            (owner . ((name . "Streamer")))
            (room_info . ((room_id . 99) (live_status . 1))))))
        view buffer credential-checks requested-pages)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-auth-credentials)
              (lambda ()
                (setq credential-checks (1+ (or credential-checks 0)))
                '(:sessdata "present")))
             ((symbol-function 'bili-api-recommended-feed)
              (lambda (page callback &rest options)
                (push page requested-pages)
                (should (= (plist-get options :page-size) 20))
                (funcall
                 callback
                 (list (cons 'mid 42)
                       (cons 'item (and (= page 1) first-page)))))))
          (setq view (bili-browse-recommended)
                buffer (appkit-view-buffer view))
          (should (= credential-checks 1))
          (should (equal (appkit-view-id view) '(catalog recommended)))
          (bili-browse--maybe-auto-load view nil 100 100)
          (should (equal (nreverse requested-pages) '(1 2)))
          (appkit-sync-invalidations view)
          (let ((state (appkit-view-state view)))
            (should (eq (plist-get state :kind) 'recommended))
            (should (= (length (plist-get state :items)) 2))
            (should (plist-get state :exhausted-p)))
          (with-current-buffer buffer
            (should (string-match-p "For you"
                                    (bili-browse--header-line)))
            (should (string-match-p "Personal video" (buffer-string)))
            (should (string-match-p "Personal live" (buffer-string)))
            (should-not (string-match-p "Advertisement" (buffer-string)))))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-browse-live-pagination-follows-window-edge ()
  (let (view buffer observer requested-pages)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-api-live-list)
              (lambda (page callback &rest _keys)
                (push page requested-pages)
                (let ((ids (pcase page
                             (1 (number-sequence 1 12))
                             (2 '(12 13))
                             (3 '(13)))))
                  (funcall
                   callback
                   `((recommend_room_list
                      . ,(mapcar
                          (lambda (id)
                            `((roomid . ,id)
                              (title . ,(format "Room %d" id))
                              (uname . "Streamer")
                              (online . 1)
                              (live_status . 1)))
                          ids)))))))
             ((symbol-function 'message) #'ignore))
          (setq view (bili-browse-live)
                buffer (appkit-view-buffer view)
                observer
                (buffer-local-value
                 'bili-browse--scroll-observer buffer))
          (should (appkit-scroll-observer-p observer))
          (should-not (keymap-lookup bili-browse-mode-map "N"))
          (bili-browse--maybe-auto-load view nil 100 100)
          (should (equal (nreverse requested-pages) '(1 2)))
          (let ((state (appkit-view-state view)))
            (should (= (length (plist-get state :items)) 13))
            (should-not (plist-get state :exhausted-p))
            (bili-browse--maybe-auto-load view nil 100 100)
            (should (plist-get state :exhausted-p))
            (should (= (length (plist-get state :items)) 13))))
      (bili-core-stop)
      (when observer
        (should-not (appkit-scroll-observer-active-p observer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-browse-uses-endpoint-pagination-metadata ()
  (let ((home (bili-browse--make-catalog-state 'home))
        (search (bili-browse--make-catalog-state 'search "q"))
        (models (list (bili-model-video-catalog-item
                       (bili-browse-test--video-json)))))
    (should
     (bili-browse--catalog-exhausted-p
      home '((no_more . t)) models '(one) 'initial 1))
    (should
     (bili-browse--catalog-exhausted-p
      search '((numPages . 3)) models '(one) 'older 3))
    (should-not
     (bili-browse--catalog-exhausted-p
      search '((numPages . 3)) models '(one) 'older 2))))

(provide 'bili-browse-test)

;;; bili-browse-test.el ends here
