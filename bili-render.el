;;; bili-render.el --- Bilibili view rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Render normalized Bilibili models carried by Appkit directory entries.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-view)
(require 'bili-cover)
(require 'bili-core)
(require 'bili-model)

(defface bili-title-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for Bilibili entity titles."
  :group 'bili)

(defface bili-section-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for Bilibili section headings."
  :group 'bili)

(defface bili-meta-face
  '((t :inherit shadow))
  "Face for Bilibili metadata."
  :group 'bili)

(defface bili-live-face
  '((t :inherit success :weight bold))
  "Face for an active Bilibili live status."
  :group 'bili)

(defface bili-action-face
  '((t :inherit link))
  "Face for Bilibili actions."
  :group 'bili)

(defface bili-disabled-face
  '((t :inherit shadow))
  "Face for unavailable Bilibili states and actions."
  :group 'bili)

(defface bili-error-face
  '((t :inherit error))
  "Face for Bilibili errors."
  :group 'bili)

(defun bili-render-count (value)
  "Return compact display text for numeric VALUE."
  (let ((number (if (numberp value) value 0)))
    (cond
     ((>= number 100000000) (format "%.1f亿" (/ number 100000000.0)))
     ((>= number 10000) (format "%.1f万" (/ number 10000.0)))
     (t (number-to-string number)))))

(defun bili-render-duration (seconds)
  "Return clock text for duration SECONDS."
  (let* ((total (max 0 (if (numberp seconds) (floor seconds) 0)))
         (hours (/ total 3600))
         (minutes (% (/ total 60) 60))
         (remaining (% total 60)))
    (if (> hours 0)
        (format "%d:%02d:%02d" hours minutes remaining)
      (format "%02d:%02d" minutes remaining))))

(defun bili-render--published-date (timestamp)
  "Return a compact publication date for TIMESTAMP."
  (if (and (numberp timestamp) (> timestamp 0))
      (format-time-string "%Y-%m-%d" (seconds-to-time timestamp))
    ""))

(defun bili-render--live-status (status)
  "Return visible text and face for numeric live STATUS."
  (pcase status
    (1 (cons "LIVE" 'bili-live-face))
    (2 (cons "ROUND/REPLAY" 'warning))
    (0 (cons "OFFLINE" 'bili-disabled-face))
    (_ (cons "STATUS UNKNOWN" 'warning))))

(defun bili-render--catalog-width ()
  "Return the current catalog row width, including hidden-buffer fallback."
  (or (appkit-view-responsive-width)
      (and (boundp 'fill-column)
           (integerp fill-column)
           (> fill-column 0)
           fill-column)
      80))
(defun bili-render--catalog-content-line (left right width)
  "Return LEFT and RIGHT arranged within WIDTH text columns."
  (let* ((right (or right ""))
         (right-width (min (string-width right) (max 0 (/ width 3))))
         (right (if (> (string-width right) right-width)
                    (appkit-view-elide-string-for-columns
                     right right-width 'default)
                  right))
         (gap (if (string-empty-p right) 0 1))
         (left-width (max 1 (- width (string-width right) gap)))
         (left (appkit-view-elide-string-for-columns
                (or left "") left-width 'default))
         (padding (max gap (- width (string-width left)
                              (string-width right)))))
    (concat left (make-string padding ?\s) right)))

(defun bili-render--video-card-lines (item width)
  "Return responsive content lines for video catalog ITEM."
  (list
   (bili-render--catalog-content-line
    (propertize (bili-catalog-item-title item) 'face 'bili-title-face)
    (propertize
     (bili-render-duration (bili-catalog-item-duration item))
     'face 'bili-meta-face)
    width)
   (bili-render--catalog-content-line
    (propertize (format "UP  %s" (bili-catalog-item-subtitle item))
                'face 'bili-meta-face)
    (propertize
     (bili-render--published-date (bili-catalog-item-published-at item))
     'face 'bili-meta-face)
    width)
   (bili-render--catalog-content-line
    (propertize
     (format "%s views · %s danmaku"
             (bili-render-count (bili-catalog-item-metric item))
             (bili-render-count
              (bili-catalog-item-secondary-metric item)))
     'face 'bili-meta-face)
    (propertize (bili-catalog-item-id item) 'face 'bili-meta-face)
    width)
   (bili-render--catalog-content-line
    (unless (string-empty-p (bili-catalog-item-reason item))
      (propertize (bili-catalog-item-reason item) 'face 'bili-meta-face))
    nil
    width)))

(defun bili-render--live-card-lines (item width)
  "Return responsive content lines for live catalog ITEM."
  (let ((status
         (bili-render--live-status (bili-catalog-item-live-status item))))
    (list
     (bili-render--catalog-content-line
      (propertize (bili-catalog-item-title item) 'face 'bili-title-face)
      (propertize (car status) 'face (cdr status))
      width)
     (bili-render--catalog-content-line
      (propertize
       (format "Streamer  %s" (bili-catalog-item-subtitle item))
       'face 'bili-meta-face)
      (propertize (format "room %s" (bili-catalog-item-id item))
                  'face 'bili-meta-face)
      width)
     (bili-render--catalog-content-line
      (propertize
       (if (string-empty-p (bili-catalog-item-area item))
           "Area unavailable"
         (bili-catalog-item-area item))
       'face 'bili-meta-face)
      (propertize
       (format "%s online"
               (bili-render-count (bili-catalog-item-metric item)))
       'face 'bili-meta-face)
      width)
     (bili-render--catalog-content-line
      (unless (string-empty-p (bili-catalog-item-reason item))
        (propertize (bili-catalog-item-reason item) 'face 'bili-meta-face))
      nil
      width))))

(defun bili-render-insert-catalog-card (view key)
  "Insert canonical catalog KEY from VIEW as a sliced-cover card."
  (let* ((app (appkit-view-app view))
         (item (bili-core-catalog-item app key)))
    (unless (bili-catalog-item-p item)
      (error "Bilibili catalog row lost its canonical item"))
    (let* ((cover-rows
            (bili-cover-catalog-slice-rows
             view key (bili-catalog-item-cover item)))
           (cover-columns (string-width (car cover-rows)))
           (width (bili-render--catalog-width))
           (content-width (max 16 (- width cover-columns 2)))
           (content-lines
            (if (eq (bili-catalog-item-kind item) 'live)
                (bili-render--live-card-lines item content-width)
              (bili-render--video-card-lines item content-width)))
           (start (point)))
      (cl-loop for cover in cover-rows
               for content in content-lines
               do (insert cover "  " content
                          (propertize "\n" 'line-height t)))
      ;; The stable entity property supports keyboard activation and semantic
      ;; position restoration.  Catalog cards intentionally have no mouse
      ;; action or hover presentation.
      (add-text-properties
       start (1- (point))
       (list 'bili-item-key key
             'rear-nonsticky '(bili-item-key)))
      t)))

(provide 'bili-render)

;;; bili-render.el ends here
