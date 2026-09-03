;;; bili-model.el --- Bilibili protocol models  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Strict, presentation-free adaptation of Bilibili JSON objects.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

(cl-defstruct (bili-video-page (:constructor bili-video-page-create))
  "One normalized playback page belonging to a Bilibili video."
  cid number title duration)

(cl-defstruct (bili-video (:constructor bili-video-create))
  "One Bilibili video and its primary playback page."
  bvid aid cid title owner description cover duration views danmaku likes pages
  published-at category)

(cl-defstruct (bili-live-room (:constructor bili-live-room-create))
  "One Bilibili live room."
  id short-id title owner cover area online live-status description parent-area)

(cl-defstruct (bili-catalog-item (:constructor bili-catalog-item-create))
  "One normalized browse result."
  kind id title subtitle cover metric duration secondary-metric area live-status
  published-at reason)

(cl-defstruct (bili-comment (:constructor bili-comment-create))
  "One normalized Bilibili root comment or reply preview."
  id author author-id message created-at likes reply-count replies)

(defun bili-model--text (value)
  "Return VALUE as text, defaulting null-like values to empty text."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   (t "")))

(defun bili-model--number (value)
  "Return VALUE as a number, accepting numeric strings."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" (string-trim value)))
    (string-to-number value))
   (t 0)))

(defun bili-model--one-line (value)
  "Return API VALUE without markup or physical line-breaking whitespace."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]]+" " "
    (replace-regexp-in-string "<[^>]+>" "" (bili-model--text value)))))

(defun bili-model--body-text (value)
  "Return API VALUE as readable, potentially multiline body text."
  (let ((text (bili-model--text value))
        case-fold-search)
    (setq text (replace-regexp-in-string "<br\\(?:[[:space:]]*/\\)?>" "\n" text)
          text (replace-regexp-in-string "</p\\(?:[[:space:]]*\\)>" "\n" text)
          text (replace-regexp-in-string "<[^>]+>" "" text)
          text (replace-regexp-in-string "\r\n?" "\n" text))
    (string-trim text)))

(defun bili-model--cover-url (value)
  "Return API cover VALUE as a normalized HTTPS URL."
  (let ((url (bili-model--one-line value)))
    (cond
     ((string-prefix-p "//" url) (concat "https:" url))
     ((string-prefix-p "http://" url)
      (concat "https://" (substring url (length "http://"))))
     (t url))))

(defun bili-model--duration (value)
  "Return VALUE as a non-negative duration in seconds.

VALUE may be numeric seconds or a MM:SS or HH:MM:SS clock string."
  (cond
   ((numberp value) (max 0 (floor value)))
   ((not (stringp value)) 0)
   (t
    (let ((text (string-trim value)))
      (cond
       ((string-match-p "\\`[0-9]+\\'" text)
        (string-to-number text))
       ((string-match
         "\\`\\([0-9]+\\):\\([0-9][0-9]\\)\\(?::\\([0-9][0-9]\\)\\)?\\'"
         text)
        (let* ((first (string-to-number (match-string 1 text)))
               (second (string-to-number (match-string 2 text)))
               (third-text (match-string 3 text))
               (third (and third-text (string-to-number third-text))))
          (if (or (>= second 60) (and third (>= third 60)))
              0
            (if third
                (+ (* first 3600) (* second 60) third)
              (+ (* first 60) second)))))
       (t 0))))))

(defun bili-model--live-status (value)
  "Return normalized live status for VALUE, or -1 when it is unknown."
  (let ((status
         (cond
          ((numberp value) value)
          ((and (stringp value)
                (string-match-p "\\`[0-9]+\\'" (string-trim value)))
           (string-to-number value))
          (t -1))))
    (if (memq status '(0 1 2)) status -1)))

(defun bili-model--video-page-from-json (data fallback-number)
  "Adapt page DATA into a `bili-video-page', or nil without a cid.

FALLBACK-NUMBER is used when the provider omitted a positive page number."
  (when (listp data)
    (let ((cid (bili-model--number (alist-get 'cid data)))
          (number (bili-model--number (alist-get 'page data)))
          (title (bili-model--one-line (alist-get 'part data))))
      (when (> cid 0)
        (setq number (if (> number 0) number fallback-number))
        (bili-video-page-create
         :cid cid
         :number number
         :title (if (string-empty-p title)
                    (format "Part %d" number)
                  title)
         :duration (bili-model--duration (alist-get 'duration data)))))))

(defun bili-model-video-from-json (data)
  "Adapt video-detail DATA into a `bili-video'."
  (unless (listp data)
    (error "Bilibili video response has no data object"))
  (let* ((pages
          (delq nil
                (cl-loop for item in (alist-get 'pages data)
                         for number from 1
                         collect
                         (bili-model--video-page-from-json item number))))
         (page (car pages))
         (owner (alist-get 'owner data))
         (stat (alist-get 'stat data))
         (bvid (bili-model--one-line (alist-get 'bvid data)))
         (cid (and page (bili-video-page-cid page))))
    (when (or (string-empty-p bvid) (not cid))
      (error "Bilibili video response lacks bvid or cid"))
    (bili-video-create
     :bvid bvid
     :aid (bili-model--number (alist-get 'aid data))
     :cid cid
     :title (bili-model--one-line (alist-get 'title data))
     :owner (bili-model--one-line (alist-get 'name owner))
     :description (bili-model--body-text (alist-get 'desc data))
     :cover (bili-model--cover-url (alist-get 'pic data))
     :duration (bili-model--duration (alist-get 'duration data))
     :views (bili-model--number (alist-get 'view stat))
     :danmaku (bili-model--number (alist-get 'danmaku stat))
     :likes (bili-model--number (alist-get 'like stat))
     :pages pages
     :published-at (bili-model--number (alist-get 'pubdate data))
     :category (bili-model--one-line (alist-get 'tname data)))))

(defun bili-model-video-catalog-item (data)
  "Adapt popular or search video DATA into a catalog item."
  (let* ((owner (or (alist-get 'owner data) (alist-get 'author data)))
         (stat (alist-get 'stat data))
         (bvid (bili-model--one-line (alist-get 'bvid data)))
         (author (if (listp owner)
                     (bili-model--one-line (alist-get 'name owner))
                   (bili-model--one-line owner)))
         (views (bili-model--number
                 (or (alist-get 'view stat) (alist-get 'play data))))
         (danmaku (bili-model--number
                   (or (alist-get 'danmaku stat)
                       (alist-get 'danmaku data)))))
    (unless (string-empty-p bvid)
      (let ((reason (alist-get 'rcmd_reason data)))
        (bili-catalog-item-create
         :kind 'video
         :id bvid
         :title (bili-model--one-line (alist-get 'title data))
         :subtitle author
         :cover (bili-model--cover-url (alist-get 'pic data))
         :metric views
         :duration (bili-model--duration (alist-get 'duration data))
         :secondary-metric danmaku
         :area ""
         :live-status -1
         :published-at (bili-model--number (alist-get 'pubdate data))
         :reason
         (bili-model--one-line
          (if (listp reason) (alist-get 'content reason) reason)))))))

(defun bili-model-live-room-from-json (data)
  "Adapt live-room DATA into a `bili-live-room'."
  (unless (listp data)
    (error "Bilibili live response has no data object"))
  (let ((room-id (bili-model--number
                  (or (alist-get 'room_id data) (alist-get 'roomid data)))))
    (when (zerop room-id)
      (error "Bilibili live response lacks room id"))
    (bili-live-room-create
     :id room-id
     :short-id (bili-model--number (alist-get 'short_id data))
     :title (bili-model--one-line (alist-get 'title data))
     :owner (bili-model--one-line
             (or (alist-get 'uname data) (alist-get 'username data)))
     :description (bili-model--body-text (alist-get 'description data))
     :cover (bili-model--cover-url
             (or (alist-get 'cover data) (alist-get 'user_cover data)))
     :area (bili-model--one-line
            (or (alist-get 'area_name data)
                (alist-get 'area_v2_name data)))
     :parent-area
     (bili-model--one-line
      (or (alist-get 'parent_area_name data)
          (alist-get 'area_v2_parent_name data)))
     :online (bili-model--number (alist-get 'online data))
     :live-status (bili-model--live-status (alist-get 'live_status data)))))

(defun bili-model-live-catalog-item (data)
  "Adapt live-directory DATA into a catalog item."
  (condition-case nil
      (let* ((room (bili-model-live-room-from-json data))
             (area
              (string-join
               (delete-dups
                (delq nil
                      (mapcar
                       (lambda (value)
                         (and (not (string-empty-p value)) value))
                       (list (bili-live-room-parent-area room)
                             (bili-live-room-area room)))))
               " / "))
             (status (bili-live-room-live-status room)))
        (bili-catalog-item-create
         :kind 'live
         :id (bili-live-room-id room)
         :title (bili-live-room-title room)
         :subtitle (bili-live-room-owner room)
         :cover (bili-live-room-cover room)
         :metric (bili-live-room-online room)
         :duration 0
         :secondary-metric 0
         :area area
         :live-status status
         :published-at 0
         :reason (bili-model--one-line
                  (or (alist-get 'reason data)
                      (alist-get 'content (alist-get 'rcmd_reason data))))))
    (error nil)))

(defun bili-model--recommended-live-catalog-item (data)
  "Adapt personalized recommendation live DATA into a catalog item."
  (let* ((room (alist-get 'room_info data))
         (show (alist-get 'show room))
         (area (alist-get 'area room))
         (watched (alist-get 'watched_show room))
         (owner (alist-get 'owner data)))
    (bili-model-live-catalog-item
     `((room_id . ,(or (alist-get 'room_id room) (alist-get 'id data)))
       (title . ,(or (alist-get 'title data) (alist-get 'title show)))
       (uname . ,(alist-get 'name owner))
       (cover . ,(or (alist-get 'pic data) (alist-get 'cover show)))
       (area_name . ,(alist-get 'area_name area))
       (parent_area_name . ,(alist-get 'parent_area_name area))
       (online . ,(or (alist-get 'num watched)
                      (alist-get 'popularity_count show)))
       (rcmd_reason . ,(alist-get 'rcmd_reason data))
       (live_status . ,(alist-get 'live_status room))))))

(defun bili-model-recommended-catalog-item (data)
  "Adapt one personalized recommendation DATA object into a catalog item.

Unsupported advertisements and non-video destinations return nil."
  (when (listp data)
    (pcase (downcase (bili-model--one-line (alist-get 'goto data)))
      ("av" (bili-model-video-catalog-item data))
      ("live" (bili-model--recommended-live-catalog-item data))
      (_ nil))))

(defun bili-model-comment-from-json (data &optional nested-p)
  "Adapt comment DATA into a normalized comment.

When NESTED-P is non-nil, do not recurse beyond this reply preview.  Return nil
for malformed entries."
  (condition-case nil
      (let* ((id (bili-model--number
                  (or (alist-get 'rpid data) (alist-get 'rpid_str data))))
             (member (alist-get 'member data))
             (content (alist-get 'content data))
             (replies
              (unless nested-p
                (delq nil
                      (mapcar
                       (lambda (reply)
                         (bili-model-comment-from-json reply t))
                       (alist-get 'replies data))))))
        (unless (> id 0)
          (error "Bilibili comment has no valid id"))
        (bili-comment-create
         :id id
         :author (bili-model--one-line (alist-get 'uname member))
         :author-id (bili-model--number
                     (or (alist-get 'mid member) (alist-get 'mid data)))
         :message (bili-model--body-text (alist-get 'message content))
         :created-at (bili-model--number (alist-get 'ctime data))
         :likes (bili-model--number (alist-get 'like data))
         :reply-count
         (max (length replies)
              (bili-model--number
               (or (alist-get 'rcount data) (alist-get 'count data))))
         :replies replies))
    (error nil)))

(defun bili-model-parse-location (input)
  "Parse Bilibili URL or identifier INPUT into a `(KIND . ID)' pair."
  (let ((text (string-trim (or input "")))
        case-fold-search)
    (cond
     ((string-match "\\`\\(BV[0-9A-Za-z]\\{10\\}\\)\\'" text)
      (cons 'video (match-string 1 text)))
     ((string-match
       "\\`https?://\\(?:www\\.\\)?bilibili\\.com/video/\\(BV[0-9A-Za-z]\\{10\\}\\)\\(?:[/?#].*\\)?\\'"
       text)
      (cons 'video (match-string 1 text)))
     ((string-match "\\`[0-9]+\\'" text)
      (cons 'live (string-to-number text)))
     ((string-match
       "\\`https?://live\\.bilibili\\.com/\\([0-9]+\\)\\(?:[/?#].*\\)?\\'"
       text)
      (cons 'live (string-to-number (match-string 1 text))))
     (t
      (user-error "Not a supported Bilibili video or live-room location")))))

(provide 'bili-model)

;;; bili-model.el ends here
