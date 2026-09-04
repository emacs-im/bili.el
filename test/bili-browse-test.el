;;; bili-browse-test.el --- Tests for Bilibili generated catalogs  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-browse)
(require 'bili-test-helper)

(defun bili-browse-test--video-json (&optional title)
  "Return one valid Bilibili video object with TITLE."
  `((bvid . "BV1xx411c7mD")
    (aid . 2)
    (title . ,(or title "Test video"))
    (desc . "A useful description")
    (duration . 9)
    (owner . ((name . "Uploader")))
    (stat . ((view . 10) (danmaku . 3) (like . 4)))
    (pages . (((cid . 42) (page . 1) (part . "Part one"))))))

(ert-deftest bili-browse-home-renders-owned-four-line-card ()
  (let (surface buffer)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-popular)
              (lambda (page callback &rest _arguments)
                (should (= page 1))
                (funcall
                 callback
                 `((no_more . t)
                   (list . (,(bili-browse-test--video-json))))))))
          (setq surface (bili-browse-home)
                buffer (appkit-surface-buffer surface))
          (bili-test-drain surface)
          (should (appkit-surface-live-p surface))
          (should (eq (plist-get (appkit-surface-model surface) :phase)
                      'ready))
          (should (= (length
                      (plist-get (appkit-surface-model surface) :items))
                     1))
          (with-current-buffer buffer
            (should (derived-mode-p 'bili-browse-mode))
            (should (string-match-p "Test video" (buffer-string)))
            (let* ((key
                    (car (plist-get
                          (appkit-surface-model surface) :items)))
                   (start
                    (text-property-any
                     (point-min) (point-max)
                     bili-browse-item-key-property key))
                   (end
                    (and start
                         (next-single-property-change
                          start bili-browse-item-key-property nil
                          (point-max)))))
              (should start)
              (should (eq (char-after start) ?T))
              (save-excursion
                (goto-char start)
                (dotimes (_ 4)
                  (should
                   (stringp (get-text-property (point) 'line-prefix)))
                  (forward-line 1)))
              (should (= (count-lines start end) 4)))))
      (bili-test-stop-surface surface)
      (bili-core-stop))))

(ert-deftest bili-browse-reuses-normalized-search-surface ()
  (let (first second)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-search-videos)
              (lambda (query page callback &rest keys)
                (should (equal query "Emacs tips"))
                (should (= page 1))
                (should (= (plist-get keys :page-size) 20))
                (funcall
                 callback
                 `((numPages . 1)
                   (numResults . 1)
                   (result . (,(bili-browse-test--video-json))))))))
          (setq first (bili-browse-search "  Emacs\n tips "))
          (bili-test-drain first)
          (setq second (bili-browse-search "Emacs tips"))
          (should (eq first second))
          (should
           (equal (appkit-surface-identity first)
                  '(catalog search "Emacs tips")))
          (with-current-buffer (appkit-surface-buffer first)
            (should (string-match-p
                     "Search “Emacs tips”"
                     (bili-browse--header-line)))))
      (bili-test-stop-surface first)
      (bili-core-stop))))

(ert-deftest bili-browse-reprojects-current-canonical-catalog-data ()
  (let (home search)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-popular)
              (lambda (_page callback &rest _keys)
                (funcall
                 callback
                 `((no_more . t)
                   (list . (,(bili-browse-test--video-json)))))))
             ((symbol-function 'bili-api-search-videos)
              (lambda (_query _page callback &rest _keys)
                (funcall
                 callback
                 `((numPages . 1)
                   (numResults . 1)
                   (result
                    . (,(bili-browse-test--video-json
                         "Updated canonical title"))))))))
          (setq home (bili-browse-home))
          (bili-test-drain home)
          (with-current-buffer (appkit-surface-buffer home)
            (should (string-match-p "Test video" (buffer-string))))
          (setq search (bili-browse-search "same video"))
          (bili-test-drain search)
          (appkit-surface-send home 'geometry)
          (with-current-buffer (appkit-surface-buffer home)
            (should (string-match-p
                     "Updated canonical title" (buffer-string)))))
      (bili-test-stop-surface home)
      (bili-test-stop-surface search)
      (bili-core-stop))))

(ert-deftest bili-browse-pagination-is-surface-owned-and-bounded ()
  (let (surface observer requested-pages)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-live-list)
              (lambda (page callback &rest _keys)
                (push page requested-pages)
                (let ((ids (pcase page
                             (1 (number-sequence 1 12))
                             (2 '(12 13))
                             (_ '(13)))))
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
                          ids))))))))
          (setq surface (bili-browse-live))
          (bili-test-drain surface)
          (setq observer
                (buffer-local-value
                 'bili-browse--scroll-observer
                 (appkit-surface-buffer surface)))
          (should (appkit-scroll-observer-p observer))
          (bili-browse--maybe-auto-load surface nil 100 100)
          (bili-test-drain surface)
          (should (equal (nreverse requested-pages) '(1 2)))
          (should (= (length
                      (plist-get (appkit-surface-model surface) :items))
                     13))
          (bili-browse--maybe-auto-load surface nil 100 100)
          (bili-test-drain surface)
          (should (plist-get
                   (appkit-surface-model surface) :exhausted-p)))
      (bili-test-stop-surface surface)
      (when observer
        (should-not (appkit-scroll-observer-active-p observer)))
      (bili-core-stop))))

(provide 'bili-browse-test)

;;; bili-browse-test.el ends here
