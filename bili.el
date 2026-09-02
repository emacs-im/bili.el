;;; bili.el --- Browse and play Bilibili in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;; Author: 0WD0 <me@0wd0.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0") (appkit "0.3.0") (browser-session "0.1.0") (video "0.1.0"))
;; Keywords: multimedia, convenience
;; URL: https://github.com/0WD0/bili.el

;;; Commentary:

;; Read-only Bilibili browsing, authenticated API access, and in-Emacs video
;; and live-stream playback through Appkit and video.el.

;;; Code:

(require 'bili-auth)
(require 'bili-browse)
(require 'bili-core)
(require 'bili-evil)
(require 'bili-playback)

;;;###autoload
(defun bili ()
  "Open the Bilibili home view."
  (interactive)
  (bili-browse-home))

;;;###autoload
(defun bili-open (url-or-id)
  "Open the Bilibili URL-OR-ID detail view."
  (interactive (list (read-string "Bilibili URL, BV id, or live room: ")))
  (bili-browse-open-url url-or-id))

;;;###autoload
(defun bili-search (query)
  "Search Bilibili videos for QUERY."
  (interactive (list (read-string "Bilibili search: ")))
  (bili-browse-search query))

;;;###autoload
(defun bili-live ()
  "Open the Bilibili live-room directory."
  (interactive)
  (bili-browse-live))

;;;###autoload
(defun bili-login ()
  "Capture and import Bilibili browser credentials."
  (interactive)
  (bili-auth-capture))

;;;###autoload
(defun bili-logout ()
  "Remove imported Bilibili credentials."
  (interactive)
  (bili-auth-clear))

(provide 'bili)

;;; bili.el ends here
