;;; bili-playback.el --- Bilibili playback orchestration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Convert transient playurl responses into Appkit media sessions.  Account
;; cookies terminate at bili-api.el; CDN requests receive only public headers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-effect)
(require 'appkit-media-resource)
(require 'appkit-media-effect)
(require 'bili-api)
(require 'bili-model)
(require 'bili-danmaku)

(defvar-keymap bili-playback-mode-map
  :doc "Bilibili-only commands layered over a generic video viewer."
  "d" #'video-toggle-subtitles)

(define-minor-mode bili-playback-mode
  "Expose Bilibili video commands in the current playback buffer.
Enabled by Bilibili video presentations, not by ordinary video/image or
live-room playback.  Generic viewport commands remain in video-mode-map;
modal application bindings are installed by bili-evil.el."
  :lighter " Bili"
  :keymap bili-playback-mode-map
  (when (and bili-playback-mode (not (derived-mode-p 'video-mode)))
    (setq bili-playback-mode nil)
    (user-error "Bilibili playback controls require a video viewer")))

(defun bili-playback--setup-video (cid duration session viewer)
  "Attach CID/DURATION danmaku and local commands to SESSION's VIEWER."
  (with-current-buffer viewer (bili-playback-mode 1))
  (let ((capability (bili-danmaku-start
                     cid duration (appkit-media-video-session-player session))))
    (appkit-cancellation-create
     :kind 'logical
     :cancel
     (lambda ()
       (unwind-protect
           (when-let* ((capability)
                       (cancel (appkit-cancellation-cancel capability)))
             (funcall cancel))
         (when (buffer-live-p viewer)
           (with-current-buffer viewer (bili-playback-mode -1))))))))

(cl-defstruct (bili-playback-source
               (:constructor bili-playback--source-create))
  "One ephemeral CDN transport selected from a playurl response."
  url
  quality
  mime-type)

(defun bili-playback--https-url-p (value)
  "Return non-nil when VALUE is an HTTPS URL without user information."
  (when (stringp value)
    (condition-case nil
        (let ((url (url-generic-parse-url value)))
          (and (string-equal (url-type url) "https")
               (stringp (url-host url))
               (not (string-empty-p (url-host url)))
               (null (url-user url))
               (null (url-password url))))
      (error nil))))

(defun bili-playback-video-source (data)
  "Select one progressive video source from playurl DATA."
  (let ((segments (alist-get 'durl data)))
    (unless (and (listp segments) (= (length segments) 1))
      (error "Bilibili returned a segmented or missing progressive stream"))
    (let* ((segment (car segments))
           (url (alist-get 'url segment)))
      (unless (bili-playback--https-url-p url)
        (error "Bilibili returned an unsafe progressive stream URL"))
      (bili-playback--source-create
       :url url
       :quality (alist-get 'quality data)
       :mime-type "video/mp4"))))

(defun bili-playback-live-source (data)
  "Select a supported HTTP-FLV AVC source from playback DATA."
  (let* ((info (alist-get 'playurl_info data))
         (playurl (alist-get 'playurl info))
         (streams (alist-get 'stream playurl))
         source)
    (catch 'selected
      (dolist (stream streams)
        (when (equal (alist-get 'protocol_name stream) "http_stream")
          (dolist (format (alist-get 'format stream))
            (when (equal (alist-get 'format_name format) "flv")
              (dolist (codec (alist-get 'codec format))
                (when (equal (alist-get 'codec_name codec) "avc")
                  (let ((base (alist-get 'base_url codec)))
                    (dolist (url-info (alist-get 'url_info codec))
                      (let ((url
                             (concat (or (alist-get 'host url-info) "")
                                     (or base "")
                                     (or (alist-get 'extra url-info) ""))))
                        (when (bili-playback--https-url-p url)
                          (setq source
                                (bili-playback--source-create
                                 :url url
                                 :quality (alist-get 'current_qn codec)
                                 :mime-type "video/x-flv"))
                          (throw 'selected source))))))))))))
    (or source
        (error "Bilibili returned no supported HTTP-FLV AVC live stream"))))

(cl-defun bili-playback-video-presentation
    (video playurl-data &key page)
  "Return a managed presentation for VIDEO from PLAYURL-DATA.
PAGE selects the canonical video part and defaults to VIDEO's primary page."
  (let* ((selected (or page (car (bili-video-pages video))))
         (cid (if selected (bili-video-page-cid selected)
                (bili-video-cid video)))
         (duration (if selected (bili-video-page-duration selected)
                     (bili-video-duration video)))
         (source (bili-playback-video-source playurl-data)))
    (appkit-media-video-presentation-create
     (appkit-media-resource-create
      :url (bili-playback-source-url source)
      :name (format "%s-%s.mp4" (bili-video-bvid video) cid)
      :mime-type (bili-playback-source-mime-type source))
     :label
     (if (and selected (> (length (bili-video-pages video)) 1))
         (format "%s · P%d %s"
                 (bili-video-title video)
                 (bili-video-page-number selected)
                 (bili-video-page-title selected))
       (bili-video-title video))
     :cache-key
     (format "bili-video:%s:%s:%s"
             (bili-video-bvid video) cid
             (or (bili-playback-source-quality source) "default"))
     :request-headers
     `(("Referer" . "https://www.bilibili.com/")
       ("User-Agent" . ,bili-api-user-agent))
     :setup-function
     (lambda (session viewer)
       (bili-playback--setup-video cid duration session viewer)))))

(defun bili-playback-live-presentation (room play-info)
  "Return a managed presentation for live ROOM from PLAY-INFO."
  (let* ((source (bili-playback-live-source play-info))
         (room-id (bili-live-room-id room)))
    (appkit-media-video-presentation-create
     (appkit-media-resource-create
      :url (bili-playback-source-url source)
      :name (format "bilibili-live-%s.flv" room-id)
      :mime-type (bili-playback-source-mime-type source))
     :label (bili-live-room-title room)
     :cache-policy 'none
     :live t
     :request-headers
     `(("Referer" . ,(format "https://live.bilibili.com/%s" room-id))
       ("User-Agent" . ,bili-api-user-agent)))))

(defun bili-playback--video-transport-start
    (_context input _observe resolve reject)
  "Resolve video playurl bytes described by Effect INPUT."
  (pcase-let ((`(,video ,page) input))
    (let* ((selected (or page (car (bili-video-pages video))))
           (cid (if selected (bili-video-page-cid selected)
                  (bili-video-cid video))))
      (bili-api-effect-cancellation
       (bili-api-video-playurl
        (bili-video-bvid video) cid resolve
        :errback reject
        :owner bili-api--effect-owner)))))

(defun bili-playback--live-transport-start
    (_context input _observe resolve reject)
  "Resolve live play-info bytes described by Effect INPUT."
  (pcase-let ((`(,room) input))
    (bili-api-effect-cancellation
     (bili-api-live-play-info
      (bili-live-room-id room) resolve
      :errback reject
      :owner bili-api--effect-owner))))

(provide 'bili-playback)

;;; bili-playback.el ends here
