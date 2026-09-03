;;; bili-detail-test.el --- Tests for Bilibili detail views  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-detail)

(defun bili-detail-test--video-json ()
  "Return one multi-part Bilibili video response."
  '((bvid . "BV1xx411c7mD")
    (aid . 2)
    (title . "A complete video title")
    (desc . "First paragraph\nSecond paragraph")
    (duration . 15)
    (pubdate . 123456)
    (tname . "Knowledge")
    (owner . ((name . "Uploader")))
    (stat . ((view . 10) (danmaku . 3) (like . 4)))
    (pages . (((cid . 42) (page . 1) (part . "Part one") (duration . 9))
              ((cid . 43) (page . 2) (part . "Part two") (duration . 6))))))

(defun bili-detail-test--room (status)
  "Return a canonical live room with STATUS."
  (bili-live-room-create
   :id 42 :short-id 7 :title "Canonical room" :owner "Streamer"
   :cover "https://example.test/live.jpg" :parent-area "Games"
   :area "Action" :online 123 :live-status status
   :description "Room description"))

(ert-deftest bili-detail-video-uses-multiline-projection-and-selects-part ()
  (let (view buffer played)
    (unwind-protect
        (cl-letf (((symbol-function 'bili-api-video)
                   (lambda (_bvid callback &rest _keys)
                     (funcall callback (bili-detail-test--video-json))))
                  ((symbol-function 'bili-playback-video)
                   (lambda (video &optional owner &rest keys)
                     (setq played
                           (list video owner (plist-get keys :page)))
                     (funcall (plist-get keys :callback) (current-buffer))
                     nil))
                  ((symbol-function 'message) #'ignore))
          (setq view (bili-detail-open-video "BV1xx411c7mD")
                buffer (appkit-view-buffer view))
          (appkit-sync-invalidations view)
          (should (appkit-projection-view-p view))
          (with-current-buffer buffer
            (should (derived-mode-p 'bili-detail-mode))
            (should (string-match-p "First paragraph\nSecond paragraph"
                                    (buffer-string)))
            (should (string-match-p "P2  Part two" (buffer-string)))
            (should-not
             (string-match-p "g refresh.*P play" (buffer-string)))
            (bili-detail--play-page-action
             (cadr (bili-video-pages
                    (bili-core-video (appkit-view-app view)
                                     "BV1xx411c7mD")))))
          (should (= (bili-video-page-cid (nth 2 played)) 43))
          (should (appkit-view-operation-p (cadr played)))
          (should (eq (appkit-view-operation-view (cadr played)) view))
          (should-not (appkit-view-operation-current-p (cadr played))))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-detail-resolves-live-identity-before-opening-view ()
  (let (view same buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'bili-live-resolve-room)
                   (lambda (room-id callback &rest _keys)
                     (should (= room-id 7))
                     (funcall callback (bili-detail-test--room 1))
                     nil))
                  ((symbol-function 'message) #'ignore))
          (should-not (bili-detail-open-live-room 7))
          (setq view (appkit-view-for-id
                      (bili-core-app) '(detail live 42))
                buffer (appkit-view-buffer view)
                same (bili-detail-open-live-room 42))
          (should (appkit-view-live-p view))
          (should (eq view same))
          (should (= (hash-table-count
                      (appkit-app-view-registry (bili-core-app)))
                     1))
          (appkit-sync-invalidations view)
          (with-current-buffer buffer
            (should (string-match-p "Streamer" (buffer-string)))
            (should (string-match-p "Watch live" (buffer-string)))))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-detail-distinguishes-round-replay-from-offline ()
  (let (view buffer)
    (unwind-protect
        (progn
          (setq view (bili-detail--open 'live 42
                                        (bili-detail-test--room 2))
                buffer (appkit-view-buffer view))
          (appkit-sync-invalidations view)
          (with-current-buffer buffer
            (should (string-match-p "Round/replay in progress"
                                    (buffer-string)))
            (should-not (string-match-p "Offline" (buffer-string)))
            (should-not (string-match-p "Watch live" (buffer-string)))
            (should-error (bili-detail-play) :type 'user-error)))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-detail-keeps-playback-error-in-the-view ()
  (let (view buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'bili-api-video)
                   (lambda (_bvid callback &rest _keys)
                     (funcall callback (bili-detail-test--video-json))))
                  ((symbol-function 'bili-playback-video)
                   (lambda (_video &optional _owner &rest keys)
                     (funcall (plist-get keys :errback) "Region locked")
                     nil))
                  ((symbol-function 'message) #'ignore))
          (setq view (bili-detail-open-video "BV1xx411c7mD")
                buffer (appkit-view-buffer view))
          (appkit-sync-invalidations view)
          (with-current-buffer buffer (bili-detail-play))
          (appkit-sync-invalidations view)
          (with-current-buffer buffer
            (should (string-match-p "Playback failed: Region locked"
                                    (buffer-string)))
            (should (string-match-p "Retry playback" (buffer-string)))))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest bili-detail-opens-comments-for-canonical-video ()
  (let* ((video (bili-model-video-from-json
                 (bili-detail-test--video-json)))
         (view (bili-detail--open 'video (bili-video-bvid video) video))
         (buffer (appkit-view-buffer view))
         opened)
    (unwind-protect
        (cl-letf (((symbol-function 'bili-comment-open)
                   (lambda (model)
                     (setq opened model)
                     'comment-view)))
          (appkit-sync-invalidations view)
          (with-current-buffer buffer
            (should (string-match-p "View comments" (buffer-string)))
            (should (eq (bili-detail-open-comments) 'comment-view)))
          (should (eq opened video)))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'bili-detail-test)

;;; bili-detail-test.el ends here
