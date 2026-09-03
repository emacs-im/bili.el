;;; bili-api-test.el --- Tests for Bilibili API contracts  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bili-api)

(ert-deftest bili-api-derives-known-wbi-mixin-key ()
  (should
   (equal
    (bili-api-wbi-mixin-key
     "https://i0.hdslb.com/bfs/wbi/7cd084941338484aae1ad9425b84077c.png"
     "https://i0.hdslb.com/bfs/wbi/4932caff0ff746eab6f01bf08b70ac45.png")
    "ea1db124af3c7062474693fa704f4ff8")))

(ert-deftest bili-api-produces-known-wbi-signature ()
  (should
   (equal
    (bili-api-wbi-sign
     '((foo . 114) (bar . 514) (zab . 1919810))
     "ea1db124af3c7062474693fa704f4ff8"
     1702204169)
    '(("bar" . "514")
      ("foo" . "114")
      ("wts" . "1702204169")
      ("zab" . "1919810")
      ("w_rid" . "8f6f2b5b3d485fe1886cec6a0be8c5d4")))))

(ert-deftest bili-api-wbi-signing-removes-forbidden-value-characters ()
  (let ((signed
         (bili-api-wbi-sign
          '((keyword . "a!b'c(d)e*f"))
          "ea1db124af3c7062474693fa704f4ff8"
          1702204169)))
    (should (equal (cdr (assoc "keyword" signed)) "abcdef"))))

(ert-deftest bili-api-trusts-only-exact-api-origins ()
  (should (bili-api--trusted-url-p
           "https://api.bilibili.com/x/web-interface/nav"))
  (should (bili-api--trusted-url-p
           "https://api.live.bilibili.com/room/v1/Room/room_init"))
  (should-not (bili-api--trusted-url-p
               "https://api.bilibili.com.evil.test/x"))
  (should-not (bili-api--trusted-url-p
               "https://user@api.bilibili.com/x"))
  (should-not (bili-api--trusted-url-p
               "http://api.bilibili.com/x")))

(ert-deftest bili-api-decodes-bounded-success-and-provider-errors ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n{\"code\":0,\"data\":{\"ok\":true}}")
    (setq-local url-http-response-status 200
                url-http-end-of-headers 20)
    (should (equal (bili-api--response-data nil) '((ok . t)))))
  (with-temp-buffer
    (insert
     "HTTP/1.1 200 OK\r\n\r\n{\"code\":-101,\"message\":\"账号未登录\",\"data\":{\"isLogin\":false}}")
    (setq-local url-http-response-status 200
                url-http-end-of-headers 20)
    (should-error (bili-api--response-data nil))
    (should
     (equal (bili-api--response-data nil '(-101))
            '((isLogin . nil))))))

(ert-deftest bili-api-live-room-uses-base-info-contract ()
  (let (arguments)
    (cl-letf (((symbol-function 'bili-api-get)
               (lambda (&rest values)
                 (setq arguments values)
                 'request)))
      (let ((callback #'ignore)
            (errback #'message)
            (owner 'test-owner))
        (should
         (eq
          (bili-api-live-room
           42 callback :errback errback :owner owner)
          'request))
        (should
         (equal
          (concat (nth 0 arguments) (nth 1 arguments))
          "https://api.live.bilibili.com/xlive/web-room/v1/index/getRoomBaseInfo"))
        (should
         (equal (nth 2 arguments)
                '((req_biz . "web_room_componet") (room_ids . 42))))
        (should (eq (nth 3 arguments) callback))
        (should (eq (plist-get (nthcdr 4 arguments) :errback) errback))
        (should (eq (plist-get (nthcdr 4 arguments) :owner) owner))))))


(ert-deftest bili-api-video-catalog-page-size-is-explicit ()
  (let (popular-params search-params)
    (cl-letf (((symbol-function 'bili-api-get)
               (lambda (_root _path params _callback &rest _keys)
                 (setq popular-params params)
                 'popular-request))
              ((symbol-function 'bili-api-wbi-get)
               (lambda (_root _path params _callback &rest _keys)
                 (setq search-params params)
                 'search-request)))
      (should
       (eq (bili-api-popular 2 #'ignore :page-size 17)
           'popular-request))
      (should
       (eq (bili-api-search-videos "query" 3 #'ignore :page-size 17)
           'search-request))
      (should (= (alist-get 'ps popular-params) 17))
      (should (= (alist-get 'page_size search-params) 17))
      (should-error
       (bili-api-popular 1 #'ignore :page-size 0))
      (should-error
       (bili-api-search-videos "query" 1 #'ignore :page-size 51)))))
(ert-deftest bili-api-recommended-feed-uses-wbi-pagination-contract ()
  (let (root path params options)
    (cl-letf (((symbol-function 'bili-api-wbi-get)
               (lambda (request-root request-path request-params
                        _callback &rest request-options)
                 (setq root request-root
                       path request-path
                       params request-params
                       options request-options)
                 'recommendation-request)))
      (should
       (eq (bili-api-recommended-feed
            3 #'ignore :page-size 20 :owner 'view :errback #'ignore)
           'recommendation-request))
      (should (equal root "https://api.bilibili.com"))
      (should (equal path "/x/web-interface/wbi/index/top/feed/rcmd"))
      (should (= (alist-get 'fresh_type params) 4))
      (should (= (alist-get 'ps params) 20))
      (should (= (alist-get 'fresh_idx params) 3))
      (should (= (alist-get 'fresh_idx_1h params) 3))
      (should (= (alist-get 'brush params) 3))
      (should (= (alist-get 'fetch_row params) 41))
      (should (equal (alist-get 'feed_version params) "V8"))
      (should (= (alist-get 'homepage_ver params) 1))
      (should (= (alist-get 'web_location params) 1430650))
      (should (eq (plist-get options :owner) 'view))
      (should-error
       (bili-api-recommended-feed 0 #'ignore))
      (should-error
       (bili-api-recommended-feed 1 #'ignore :page-size 31)))))

(ert-deftest bili-api-video-comments-wraps-opaque-cursor-for-wbi ()
  (let (params)
    (cl-letf (((symbol-function 'bili-api-wbi-get)
               (lambda (_root _path request-params _callback &rest _options)
                 (setq params request-params)
                 'comment-request)))
      (should
       (eq (bili-api-video-comments 42 #'ignore :offset "CAEiAggC")
           'comment-request))
      (should (= (alist-get 'type params) 1))
      (should (= (alist-get 'oid params) 42))
      (should (= (alist-get 'mode params) 3))
      (should (= (alist-get 'web_location params) 1315875))
      (should
       (equal (json-parse-string
               (alist-get 'pagination_str params)
               :object-type 'alist)
              '((offset . "CAEiAggC"))))
      (should-error (bili-api-video-comments 0 #'ignore))
      (should-error
       (bili-api-video-comments 42 #'ignore :offset "")))))

(provide 'bili-api-test)

;;; bili-api-test.el ends here
