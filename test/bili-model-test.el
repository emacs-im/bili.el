;;; bili-model-test.el --- Tests for Bilibili models  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bili-model)

(ert-deftest bili-model-parses-video-and-live-locations ()
  (should
   (equal (bili-model-parse-location "BV1xx411c7mD")
          '(video . "BV1xx411c7mD")))
  (should
   (equal
    (bili-model-parse-location
     "https://www.bilibili.com/video/BV1xx411c7mD/?spm_id_from=333")
    '(video . "BV1xx411c7mD")))
  (should
   (equal (bili-model-parse-location "https://live.bilibili.com/6")
          '(live . 6)))
  (should-error (bili-model-parse-location "https://example.com/video")
                :type 'user-error))

(ert-deftest bili-model-adapts-video-detail-pages ()
  (let* ((video
          (bili-model-video-from-json
           '((bvid . "BV1xx411c7mD")
             (aid . 2)
             (title . "Test\nvideo")
             (desc . "Description")
             (duration . 9)
             (owner . ((name . "Uploader")))
             (stat . ((view . 10) (danmaku . 3) (like . 4)))
             (pages . (((page . 1) (part . "Missing cid"))
                       ((cid . "42") (page . 2)
                        (part . "<em>Part</em>\none")
                        (duration . 65))
                       ((cid . 0) (page . 3) (part . "Invalid cid")))))))
         (pages (bili-video-pages video))
         (page (car pages)))
    (should (equal (bili-video-bvid video) "BV1xx411c7mD"))
    (should (= (bili-video-cid video) 42))
    (should (equal (bili-video-title video) "Test video"))
    (should (equal (bili-video-owner video) "Uploader"))
    (should (= (bili-video-views video) 10))
    (should (= (length pages) 1))
    (should (bili-video-page-p page))
    (should (= (bili-video-page-cid page) 42))
    (should (= (bili-video-page-number page) 2))
    (should (equal (bili-video-page-title page) "Part one"))
    (should (= (bili-video-page-duration page) 65)))
  (should-error
   (bili-model-video-from-json
    '((bvid . "BV1xx411c7mD")
      (pages . (((page . 1) (part . "Missing cid"))))))))

(ert-deftest bili-model-normalizes-video-catalog-fields ()
  (let ((item
         (bili-model-video-catalog-item
          '((bvid . "BV1xx411c7mD")
            (title . "<em class=\"keyword\">Emacs</em>\nclient")
            (author . "Alice\nExample")
            (pic . "http://i0.hdslb.com/cover.jpg")
            (play . 12)
            (danmaku . "7")
            (duration . "03:05")
            (pubdate . 123456)
            (rcmd_reason . ((content . "Trending")))))))
    (should (equal (bili-catalog-item-title item) "Emacs client"))
    (should (equal (bili-catalog-item-subtitle item) "Alice Example"))
    (should (equal (bili-catalog-item-cover item)
                   "https://i0.hdslb.com/cover.jpg"))
    (should (= (bili-catalog-item-metric item) 12))
    (should (= (bili-catalog-item-secondary-metric item) 7))
    (should (= (bili-catalog-item-duration item) 185))
    (should (= (bili-catalog-item-published-at item) 123456))
    (should (equal (bili-catalog-item-area item) ""))
    (should (= (bili-catalog-item-live-status item) -1))
    (should (equal (bili-catalog-item-reason item) "Trending")))
  (should
   (= (bili-catalog-item-duration
       (bili-model-video-catalog-item
        '((bvid . "BVclock") (duration . "01:02:03"))))
      3723))
  (should
   (= (bili-catalog-item-duration
       (bili-model-video-catalog-item
        '((bvid . "BVseconds") (duration . 90))))
      90)))

(ert-deftest bili-model-normalizes-live-room-and-catalog-fields ()
  (let* ((data
          '((room_id . 6)
            (short_id . 3)
            (title . "<b>Live</b>\nroom")
            (uname . "Alice\nLive")
            (description . "First\nsecond")
            (cover . "//i0.hdslb.com/live.jpg")
            (area_name . "Games\nRetro")
            (parent_area_name . "Entertainment")
            (online . "101")
            (live_status . 2)))
         (room (bili-model-live-room-from-json data))
         (item (bili-model-live-catalog-item data)))
    (should (equal (bili-live-room-title room) "Live room"))
    (should (equal (bili-live-room-description room) "First\nsecond"))
    (should (equal (bili-live-room-area room) "Games Retro"))
    (should (equal (bili-live-room-parent-area room) "Entertainment"))
    (should (= (bili-live-room-live-status room) 2))
    (should (equal (bili-live-room-cover room)
                   "https://i0.hdslb.com/live.jpg"))
    (should (equal (bili-catalog-item-area item)
                   "Entertainment / Games Retro"))
    (should (= (bili-catalog-item-live-status item) 2))
    (should (= (bili-catalog-item-metric item) 101)))
  (let* ((data '((roomid . 9)))
         (room (bili-model-live-room-from-json data))
         (item (bili-model-live-catalog-item data)))
    (should (equal (bili-live-room-description room) ""))
    (should (equal (bili-live-room-area room) ""))
    (should (equal (bili-live-room-parent-area room) ""))
    (should (= (bili-live-room-online room) 0))
    (should (= (bili-live-room-live-status room) -1))
    (should (= (bili-catalog-item-live-status item) -1))))

(ert-deftest bili-model-normalizes-personalized-mixed-feed-items ()
  (let ((video
         (bili-model-recommended-catalog-item
          '((goto . "av")
            (bvid . "BV1feed")
            (title . "Recommended video")
            (owner . ((name . "Uploader")))
            (stat . ((view . 12) (danmaku . 3)))
            (duration . 61))))
        (live
         (bili-model-recommended-catalog-item
          '((goto . "live")
            (id . 99)
            (title . "Recommended live")
            (pic . "http://i0.hdslb.com/live.jpg")
            (owner . ((name . "Streamer")))
            (rcmd_reason . ((content . "Because you watched Emacs")))
            (room_info
             . ((room_id . 99)
                (live_status . 1)
                (show . ((popularity_count . 321)))
                (area . ((parent_area_name . "Knowledge")
                         (area_name . "Technology")))
                (watched_show . ((num . 123)))))))))
    (should (eq (bili-catalog-item-kind video) 'video))
    (should (equal (bili-catalog-item-id video) "BV1feed"))
    (should (eq (bili-catalog-item-kind live) 'live))
    (should (= (bili-catalog-item-id live) 99))
    (should (equal (bili-catalog-item-subtitle live) "Streamer"))
    (should (equal (bili-catalog-item-area live)
                   "Knowledge / Technology"))
    (should (= (bili-catalog-item-metric live) 123))
    (should (= (bili-catalog-item-live-status live) 1))
    (should (equal (bili-catalog-item-reason live)
                   "Because you watched Emacs"))
    (should-not
     (bili-model-recommended-catalog-item
      '((goto . "ad") (title . "Advertisement"))))))

(provide 'bili-model-test)

;;; bili-model-test.el ends here
