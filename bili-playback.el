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
(require 'appkit-core)
(require 'appkit-media-resource)
(require 'bili-api)
(require 'bili-core)
(require 'bili-model)

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

(defun bili-playback--present-session (session label owner)
  "Present Appkit media SESSION under LABEL and OWNER without leaking it."
  (let (opened-p)
    (unwind-protect
        (prog1
            (appkit-media-present-video-session
             session label :owner owner :start t)
          (setq opened-p t))
      (unless opened-p
        (appkit-media-video-session-close session)))))

(cl-defun bili-playback-open-video
    (video playurl-data &optional owner &key page)
  "Open VIDEO using progressive PLAYURL-DATA under Appkit OWNER.

PAGE selects the canonical video part and defaults to VIDEO's primary page."
  (unless (bili-video-p video)
    (error "Invalid Bilibili video playback model"))
  (unless (or (null page) (bili-video-page-p page))
    (error "Invalid Bilibili video page"))
  (let* ((selected (or page (car (bili-video-pages video))))
         (cid (if selected (bili-video-page-cid selected)
                (bili-video-cid video)))
         (source (bili-playback-video-source playurl-data))
         (url (bili-playback-source-url source))
         (app-owner (or owner (bili-core-app)))
         (session
          (appkit-media-video-session-create
           (appkit-media-resource-create
            :url url :name (format "%s-%s.mp4" (bili-video-bvid video) cid)
            :mime-type (bili-playback-source-mime-type source))
           "Bilibili"
           :owner app-owner
           :cache-key
           (format "bili-video:%s:%s:%s"
                   (bili-video-bvid video) cid
                   (or (bili-playback-source-quality source) "default"))
           :request-headers
           `(("Referer" . "https://www.bilibili.com/")
             ("User-Agent" . ,bili-api-user-agent)))))
    (bili-playback--present-session
     session
     (if (and selected (> (length (bili-video-pages video)) 1))
         (format "%s · P%d %s"
                 (bili-video-title video)
                 (bili-video-page-number selected)
                 (bili-video-page-title selected))
       (bili-video-title video))
     app-owner)))

(defun bili-playback-open-live (room play-info &optional owner)
  "Open live ROOM using transient PLAY-INFO under Appkit OWNER."
  (unless (bili-live-room-p room)
    (error "Invalid Bilibili live-room playback model"))
  (let* ((source (bili-playback-live-source play-info))
         (app-owner (or owner (bili-core-app)))
         (room-id (bili-live-room-id room))
         (session
          (appkit-media-video-session-create
           (appkit-media-resource-create
            :url (bili-playback-source-url source)
            :name (format "bilibili-live-%s.flv" room-id)
            :mime-type (bili-playback-source-mime-type source))
           "Bilibili Live"
           :owner app-owner
           :cache-policy 'none
           :live t
           :request-headers
           `(("Referer" . ,(format "https://live.bilibili.com/%s" room-id))
             ("User-Agent" . ,bili-api-user-agent)))))
    (bili-playback--present-session
     session (bili-live-room-title room) app-owner)))

(cl-defun bili-playback-video
    (video &optional request-owner &key page callback errback)
  "Resolve and play VIDEO under REQUEST-OWNER.

PAGE selects a normalized video part.  CALLBACK receives the opened Canvas
buffer.  ERRBACK receives a readable failure string."
  (unless (bili-video-p video)
    (error "Invalid Bilibili video"))
  (unless (or (null page) (bili-video-page-p page))
    (error "Invalid Bilibili video page"))
  (let* ((selected (or page (car (bili-video-pages video))))
         (cid (if selected (bili-video-page-cid selected)
                (bili-video-cid video)))
         (transport-owner (or request-owner (bili-core-app)))
         (session-owner
          (or (appkit-owner-app transport-owner)
              (error "Bilibili playback has no Appkit application")))
         (failure (or errback (lambda (message) (message "%s" message)))))
    (bili-api-video-playurl
     (bili-video-bvid video) cid
     (lambda (data)
       (condition-case error-data
           (let ((buffer
                  (bili-playback-open-video
                   video data session-owner :page selected)))
             (when callback
               (funcall callback buffer)))
         (error (funcall failure (error-message-string error-data)))))
     :errback failure
     :owner transport-owner)))

(cl-defun bili-playback-live
    (room &optional request-owner &key callback errback)
  "Resolve and play live ROOM under REQUEST-OWNER.

CALLBACK receives the opened Canvas buffer.  ERRBACK receives a readable
failure string."
  (unless (bili-live-room-p room)
    (error "Invalid Bilibili live room"))
  (let* ((transport-owner (or request-owner (bili-core-app)))
         (session-owner
          (or (appkit-owner-app transport-owner)
              (error "Bilibili playback has no Appkit application")))
         (failure (or errback (lambda (message) (message "%s" message)))))
    (bili-api-live-play-info
     (bili-live-room-id room)
     (lambda (data)
       (condition-case error-data
           (let ((buffer (bili-playback-open-live room data session-owner)))
             (when callback
               (funcall callback buffer)))
         (error (funcall failure (error-message-string error-data)))))
     :errback failure
     :owner transport-owner)))

(provide 'bili-playback)

;;; bili-playback.el ends here
