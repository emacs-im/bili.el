;;; bili-cover.el --- Bilibili declarative cover resources  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Describe public Bilibili cover acquisition as Appkit Resource demand and
;; render ready files from Surface-scoped presentation state.  Account cookies
;; never cross this boundary.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-resource)
(require 'appkit-surface)
(require 'bili-api)

(defcustom bili-cover-cache-directory
  (locate-user-emacs-file "bili/covers/")
  "Directory holding public Bilibili cover images."
  :type 'directory
  :group 'bili)

(defcustom bili-cover-catalog-lines 4
  "Number of physical text rows occupied by one catalog cover."
  :type '(choice (const 3) (const 4))
  :group 'bili)

(defcustom bili-cover-catalog-max-width 160
  "Maximum catalog cover width in pixels."
  :type 'integer
  :group 'bili)

(defcustom bili-cover-detail-max-width 480
  "Maximum detail cover width in pixels."
  :type 'integer
  :group 'bili)

(defvar bili-cover--image-cache (make-hash-table :test #'equal)
  "Decoded cover descriptors keyed by local file identity and geometry.")

(defconst bili-cover-trusted-domain-suffixes
  '("hdslb.com" "biliimg.com")
  "Domain suffixes permitted for public Bilibili cover acquisition.")

(defun bili-cover--trusted-host-p (host)
  "Return non-nil when HOST belongs to a trusted Bilibili image domain."
  (and (stringp host)
       (seq-some
        (lambda (suffix)
          (or (string-equal host suffix)
              (string-suffix-p (concat "." suffix) host)))
        bili-cover-trusted-domain-suffixes)))

(defun bili-cover-normalize-url (value)
  "Return VALUE as a trusted HTTPS Bilibili image URL, or nil."
  (when (stringp value)
    (let* ((trimmed (string-trim value))
           (candidate
            (cond
             ((string-prefix-p "//" trimmed) (concat "https:" trimmed))
             ((string-prefix-p "http://" trimmed)
              (concat "https://" (substring trimmed (length "http://"))))
             (t trimmed))))
      (condition-case nil
          (let* ((url (url-generic-parse-url candidate))
                 (host (downcase (or (url-host url) ""))))
            (when (and (string-equal (url-type url) "https")
                       (bili-cover--trusted-host-p host)
                       (or (null (url-port url)) (= (url-port url) 443))
                       (null (url-user url))
                       (null (url-password url)))
              candidate))
        (error nil)))))

(defun bili-cover-resource-key (entity-key url)
  "Return the Resource key for ENTITY-KEY's normalized URL revision."
  (list 'cover entity-key (bili-cover-normalize-url url)))

(defun bili-cover--image-capable-frame-p (frame)
  "Return non-nil when live FRAME can render inline images."
  (and (frame-live-p frame)
       (with-selected-frame frame
         (appkit-media-inline-image-rendering-available-p))))

(defun bili-cover--display-frame (&optional surface)
  "Return an image-capable frame, preferring SURFACE's display window."
  (or (when (appkit-surface-live-p surface)
        (when-let* ((buffer (appkit-surface-buffer surface))
                    ((buffer-live-p buffer))
                    (window (appkit-geometry-display-window buffer))
                    ((window-live-p window))
                    (frame (window-frame window))
                    ((bili-cover--image-capable-frame-p frame)))
          frame))
      (seq-find #'bili-cover--image-capable-frame-p (frame-list))
      (selected-frame)))

(defun bili-cover--image-display-available-p ()
  "Return non-nil when any live frame can render inline images."
  (seq-some #'bili-cover--image-capable-frame-p (frame-list)))

(defun bili-cover--cache-base (entity-key url)
  "Return the cache base for ENTITY-KEY and public source URL."
  (let ((directory
         (expand-file-name
          (secure-hash 'sha256 (prin1-to-string entity-key))
          bili-cover-cache-directory)))
    (expand-file-name
     (secure-hash 'sha256 (car (split-string url "[#?]")))
     directory)))

(defun bili-cover--cached-file (entity-key url)
  "Return a cached cover file for ENTITY-KEY and URL, or nil."
  (appkit-media-image-cache-existing-file
   (bili-cover--cache-base entity-key url)))

(defun bili-cover--load (_context input success failure)
  "Acquire cover INPUT, resolving SUCCESS or FAILURE."
  (pcase-let* ((`(,entity-key ,url) input)
               (cached (bili-cover--cached-file entity-key url)))
    (if cached
        (progn (funcall success cached) nil)
      (let ((transfer
             (appkit-media-cache-image-resource-async
              `((url . ,url)
                (name . ,(or (appkit-media-url-filename url) "cover.img")))
              (bili-cover--cache-base entity-key url)
              success failure
              :headers
              `(("Accept" . "image/avif,image/webp,image/*;q=0.8,*/*;q=0.1")
                ("Referer" . "https://www.bilibili.com/")
                ("User-Agent" . ,bili-api-user-agent)))))
        (when (appkit-media-transfer-p transfer)
          (appkit-cancellation-create
           :kind 'transport
           :cancel (lambda ()
                     (appkit-media-cancel-transfer transfer))))))))

(defun bili-cover-demand (entity-key value)
  "Return declarative cover demand for ENTITY-KEY and URL VALUE, or nil."
  (when-let* ((url (bili-cover-normalize-url value))
              ((bili-cover--image-display-available-p)))
    (appkit-resource-demand-create
     :key (bili-cover-resource-key entity-key url)
     :input (list entity-key url)
     :loader #'bili-cover--load
     :acquisition-identity (list 'bili-cover url)
     :sharing-policy 'shared
     :cache-policy 'while-interested)))

(defun bili-cover--file (surface entity-key url)
  "Return SURFACE's ready cover file for ENTITY-KEY and URL, or nil."
  (when-let* ((state
               (appkit-resource-state
                surface (bili-cover-resource-key entity-key url)))
              ((eq (appkit-resource-state-status state) 'ready))
              (file (appkit-resource-state-value state))
              ((stringp file))
              ((file-readable-p file)))
    file))

(defun bili-cover-image (surface entity-key url pixel-width pixel-height)
  "Return SURFACE's cached cover image for ENTITY-KEY and URL."
  (when-let* ((url (bili-cover-normalize-url url))
              (file (bili-cover--file surface entity-key url))
              (attributes (file-attributes file 'string)))
    (let* ((identity
            (list file (file-attribute-size attributes)
                  (file-attribute-modification-time attributes)
                  pixel-width pixel-height))
           (cached (gethash identity bili-cover--image-cache)))
      (or cached
          (when-let* ((image
                       (appkit-media-cropped-preview-image-from-file
                        file pixel-width pixel-height)))
            (puthash identity image bili-cover--image-cache)
            image)))))

(defun bili-cover-catalog-slices (surface entity-key url)
  "Return `(COLUMNS . ROWS)' for ENTITY-KEY's catalog cover in SURFACE."
  (let* ((frame (bili-cover--display-frame surface))
         (line-count (min 4 (max 3 bili-cover-catalog-lines)))
         (height (* line-count (max 1 (frame-char-height frame))))
         (width (min (max 1 bili-cover-catalog-max-width)
                     (round (* height (/ 16.0 9.0)))))
         (columns (max 8 (ceiling (/ (float width)
                                    (max 1 (frame-char-width frame))))))
         (image
          (with-selected-frame frame
            (bili-cover-image surface entity-key url width height)))
         (source-rows
          (and image
               (with-selected-frame frame
                 (appkit-media-image-slice-rows image))))
         rows)
    (dotimes (index line-count)
      (let ((row
             (or (copy-sequence (nth index source-rows))
                 (propertize
                  " " 'display
                  `(space :width
                          ,(if (display-graphic-p frame)
                               (list width)
                             columns))))))
        (add-text-properties
         0 (length row) (list 'help-echo (or url "Cover unavailable")
                              'rear-nonsticky '(display))
         row)
        (push row rows)))
    (cons columns (nreverse rows))))

(defun bili-cover-avatar-image (surface entity-key url pixel-size)
  "Return SURFACE's cached circular avatar for ENTITY-KEY and URL."
  (let ((frame (bili-cover--display-frame surface)))
    (when-let* ((url (bili-cover-normalize-url url))
                (file (bili-cover--file surface entity-key url))
                (attributes (file-attributes file 'string)))
      (let* ((identity
              (list 'avatar file (file-attribute-size attributes)
                    (file-attribute-modification-time attributes)
                    pixel-size))
             (cached (gethash identity bili-cover--image-cache)))
        (or cached
            (with-selected-frame frame
              (when-let* ((image
                           (appkit-media-circular-image-from-file
                            file pixel-size)))
                (puthash identity image bili-cover--image-cache)
                image)))))))

(defun bili-cover-detail-image (surface entity-key url)
  "Return a responsive 16:9 cover image for SURFACE and ENTITY-KEY."
  (let* ((frame (bili-cover--display-frame surface))
         (columns
          (with-current-buffer (appkit-surface-buffer surface)
            (or (appkit-surface-responsive-width surface 4) 76)))
         (width (max 160
                     (min bili-cover-detail-max-width
                          (* columns (max 1 (frame-char-width frame))))))
         (height (max 90 (round (* width (/ 9.0 16.0))))))
    (with-selected-frame frame
      (bili-cover-image surface entity-key url width height))))

(provide 'bili-cover)

;;; bili-cover.el ends here
