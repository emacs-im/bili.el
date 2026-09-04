;;; bili-detail-test.el --- Tests for Bilibili generated details  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-detail)
(require 'bili-test-helper)

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
   :cover "" :parent-area "Games"
   :area "Action" :online 123 :live-status status
   :description "Room description"))

(ert-deftest bili-detail-video-renders-and-plays-selected-part ()
  (let (surface requested-cid opened-presentation)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-video)
              (lambda (_bvid callback &rest _keys)
                (funcall callback (bili-detail-test--video-json))))
             ((symbol-function 'bili-api-video-playurl)
              (lambda (_bvid cid callback &rest _keys)
                (setq requested-cid cid)
                (funcall
                 callback
                 '((quality . 64)
                   (durl . (((url . "https://cdn.example/video.mp4"))))))))
             ((symbol-function 'appkit-media-video-presentation-start)
              (lambda (_context input _observe resolve _reject)
                (setq opened-presentation input)
                (funcall resolve 'closed)
                nil)))
          (setq surface
                (bili-detail-open-video "BV1xx411c7mD"))
          (bili-test-drain surface)
          (with-current-buffer (appkit-surface-buffer surface)
            (should (derived-mode-p 'bili-detail-mode))
            (should (string-match-p
                     "First paragraph\nSecond paragraph"
                     (buffer-string)))
            (should (string-match-p "P2  Part two" (buffer-string)))
            (bili-detail--play-page-action
             (cadr
              (bili-video-pages
               (bili-core-video
                (appkit-surface-app surface)
                "BV1xx411c7mD")))))
          (bili-test-drain surface)
          (should (= requested-cid 43))
          (should
           (equal
            (appkit-media-video-presentation-cache-key opened-presentation)
            "bili-video:BV1xx411c7mD:43:64"))
          (should (eq (plist-get
                       (appkit-surface-model surface) :playback-phase)
                      'idle)))
      (bili-test-stop-surface surface)
      (bili-core-stop))))

(ert-deftest bili-detail-resolves-live-identity-and-reuses-surface ()
  (let (surface same)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-live-resolve-room)
              (lambda (room-id callback &rest _keys)
                (should (= room-id 7))
                (funcall callback (bili-detail-test--room 1)))))
          (setq surface (bili-detail-open-live-room 7))
          (bili-test-drain surface)
          (setq same (bili-detail-open-live-room 7))
          (should (eq surface same))
          (should (= (plist-get
                      (appkit-surface-model surface) :id)
                     42))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (string-match-p "Streamer" (buffer-string)))
            (should (string-match-p "Watch live" (buffer-string)))))
      (bili-test-stop-surface surface)
      (bili-core-stop))))

(ert-deftest bili-detail-round-replay-refuses-live-playback ()
  (let (surface)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-live-resolve-room)
              (lambda (_room-id callback &rest _keys)
                (funcall callback (bili-detail-test--room 2)))))
          (setq surface (bili-detail-open-live-room 42))
          (bili-test-drain surface)
          (with-current-buffer (appkit-surface-buffer surface)
            (should (string-match-p
                     "Round/replay in progress" (buffer-string)))
            (should-not (string-match-p "Watch live" (buffer-string)))
            (should-error (bili-detail-play) :type 'user-error)))
      (bili-test-stop-surface surface)
      (bili-core-stop))))

(ert-deftest bili-detail-keeps-playback-failure-in-surface-state ()
  (let (surface)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-video)
              (lambda (_bvid callback &rest _keys)
                (funcall callback (bili-detail-test--video-json))))
             ((symbol-function 'bili-api-video-playurl)
              (lambda (_bvid _cid _callback &rest keys)
                (funcall (plist-get keys :errback) "Region locked"))))
          (setq surface
                (bili-detail-open-video "BV1xx411c7mD"))
          (bili-test-drain surface)
          (with-current-buffer (appkit-surface-buffer surface)
            (bili-detail-play))
          (bili-test-drain surface)
          (with-current-buffer (appkit-surface-buffer surface)
            (should (string-match-p
                     "Playback failed: Region locked" (buffer-string)))
            (should (string-match-p "Retry playback" (buffer-string)))))
      (bili-test-stop-surface surface)
      (bili-core-stop))))

(ert-deftest bili-detail-opens-comments-for-canonical-video ()
  (let (surface opened)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-video)
              (lambda (_bvid callback &rest _keys)
                (funcall callback (bili-detail-test--video-json))))
             ((symbol-function 'bili-comment-open)
              (lambda (video)
                (setq opened video)
                'comment-surface)))
          (setq surface
                (bili-detail-open-video "BV1xx411c7mD"))
          (bili-test-drain surface)
          (with-current-buffer (appkit-surface-buffer surface)
            (should (eq (bili-detail-open-comments)
                        'comment-surface)))
          (should (bili-video-p opened)))
      (bili-test-stop-surface surface)
      (bili-core-stop))))

(provide 'bili-detail-test)

;;; bili-detail-test.el ends here
