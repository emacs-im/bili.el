;;; bili-playback-test.el --- Tests for Bilibili playback  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-playback)

(ert-deftest bili-playback-selects-one-safe-progressive-stream ()
  (let ((source
         (bili-playback-video-source
          '((quality . 64)
            (durl . (((url . "https://cdn.example/video.mp4"))))))))
    (should (equal (bili-playback-source-url source)
                   "https://cdn.example/video.mp4"))
    (should (= (bili-playback-source-quality source) 64)))
  (should-error
   (bili-playback-video-source
    '((durl . (((url . "https://cdn.example/one.mp4"))
               ((url . "https://cdn.example/two.mp4")))))))
  (should-error
   (bili-playback-video-source
    '((durl . (((url . "http://cdn.example/video.mp4"))))))))

(ert-deftest bili-playback-prefers-http-flv-avc-live-source ()
  (let* ((data
          '((playurl_info
             . ((playurl
                 . ((stream
                     . (((protocol_name . "http_hls")
                         (format
                          . (((format_name . "fmp4")
                              (codec
                               . (((codec_name . "hevc")
                                   (current_qn . 10000)
                                   (base_url . "hls.m3u8")
                                   (url_info
                                    . (((host . "https://hls.example/")
                                        (extra . "?signed=1")))))))))))
                        ((protocol_name . "http_stream")
                         (format
                          . (((format_name . "flv")
                              (codec
                               . (((codec_name . "avc")
                                   (current_qn . 250)
                                   (base_url . "live.flv")
                                   (url_info
                                    . (((host . "https://flv.example/")
                                        (extra . "?signed=2")))))))))))))))))))
         (source (bili-playback-live-source data)))
    (should (equal (bili-playback-source-url source)
                   "https://flv.example/live.flv?signed=2"))
    (should (equal (bili-playback-source-mime-type source) "video/x-flv"))
    (let* ((unsupported (copy-tree data))
           (playurl
            (alist-get 'playurl (alist-get 'playurl_info unsupported)))
           (streams (alist-get 'stream playurl)))
      (setf (alist-get 'stream playurl) (list (car streams)))
      (should-error (bili-playback-live-source unsupported)))))

(ert-deftest bili-playback-live-cdn-receives-no-account-cookie ()
  (let ((room
         (bili-live-room-create
          :id 6 :title "Live" :owner "Host" :area "Area"
          :online 10 :live-status 1))
        session-call
        presentation-call)
    (cl-letf (((symbol-function 'appkit-media-video-session-create)
               (lambda (resource &optional label &rest arguments)
                 (setq session-call (list resource label arguments))
                 'session))
              ((symbol-function 'appkit-media-present-video-session)
               (lambda (session label &rest arguments)
                 (setq presentation-call (list session label arguments))
                 'viewer))
              ((symbol-function 'appkit-media-video-session-close)
               (lambda (_session) (ert-fail "Live session closed unexpectedly"))))
      (should
       (eq
        (bili-playback-open-live
         room
         '((playurl_info
            . ((playurl
                . ((stream
                    . (((protocol_name . "http_stream")
                        (format
                         . (((format_name . "flv")
                             (codec
                              . (((codec_name . "avc")
                                  (current_qn . 250)
                                  (base_url . "live.flv")
                                  (url_info
                                   . (((host . "https://cdn.example/")
                                       (extra . "?token=temporary"))))))))))))))))))
         'app)
        'viewer)))
    (let* ((arguments (nth 2 session-call))
           (headers (plist-get arguments :request-headers)))
      (should (eq (plist-get arguments :live) t))
      (should (eq (plist-get arguments :cache-policy) 'none))
      (should-not (plist-member arguments :cache-key))
      (should (equal (cdr (assoc "Referer" headers))
                     "https://live.bilibili.com/6"))
      (should-not (assoc-string "Cookie" headers t)))
    (should (equal presentation-call '(session "Live" (:owner app :start t))))))


(ert-deftest bili-playback-video-page-uses-stable-cid-and-public-headers ()
  (let* ((page
          (bili-video-page-create
           :cid 43 :number 2 :title "Part two" :duration 6))
         (video
          (bili-video-create
           :bvid "BV1xx411c7mD" :cid 42 :title "Video"
           :pages (list
                   (bili-video-page-create
                    :cid 42 :number 1 :title "Part one" :duration 9)
                   page)))
         session-call
         presentation-call)
    (cl-letf (((symbol-function 'appkit-media-video-session-create)
               (lambda (resource &optional label &rest arguments)
                 (setq session-call (list resource label arguments))
                 'session))
              ((symbol-function 'appkit-media-present-video-session)
               (lambda (session label &rest arguments)
                 (setq presentation-call (list session label arguments))
                 'viewer))
              ((symbol-function 'appkit-media-video-session-close)
               (lambda (_session)
                 (ert-fail "Video session closed unexpectedly"))))
      (should
       (eq
        (bili-playback-open-video
         video
         '((quality . 64)
           (durl . (((url . "https://cdn.example/video.mp4")))))
         'app :page page)
        'viewer)))
    (let* ((resource (car session-call))
           (arguments (nth 2 session-call))
           (headers (plist-get arguments :request-headers)))
      (should (equal (alist-get 'name resource)
                     "BV1xx411c7mD-43.mp4"))
      (should (equal (plist-get arguments :cache-key)
                     "bili-video:BV1xx411c7mD:43:64"))
      (should (equal (cdr (assoc "Referer" headers))
                     "https://www.bilibili.com/"))
      (should-not (assoc-string "Cookie" headers t)))
    (should
     (equal presentation-call
            '(session "Video · P2 Part two" (:owner app :start t))))))
(provide 'bili-playback-test)

;;; bili-playback-test.el ends here
