;;; bili-auth.el --- Bilibili browser credentials  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capture, validate, and persist the minimum Bilibili web-session cookies.
;; Only bili-api.el consumes these credentials; playback CDN requests do not.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'browser-session)
(require 'bili-core)

(defcustom bili-auth-file
  (expand-file-name "bili/session.json" user-emacs-directory)
  "Private file containing imported Bilibili session cookies."
  :type 'file
  :group 'bili)

(defcustom bili-auth-browser-profile-root
  (expand-file-name "bili/browser-session" user-emacs-directory)
  "Automatic browser profile root used for Bilibili login capture."
  :type 'directory
  :group 'bili)

(defconst bili-auth--schema 1
  "Current private Bilibili credential schema.")

(defconst bili-auth--capture-url "https://www.bilibili.com/"
  "Exact browser origin used for Bilibili credential capture.")

(defconst bili-auth--cookie-names '("SESSDATA" "bili_jct")
  "Minimum Bilibili cookies imported from the browser.")

(cl-defstruct (bili-auth-credential
               (:constructor bili-auth--credential-create))
  "Validated Bilibili web-session credentials."
  sessdata
  sessdata-expires
  csrf
  csrf-expires)

(defvar bili-auth--capture-request nil
  "Current browser-session capture request, or nil.")

(defvar bili-auth--capture-handle nil
  "Appkit handle owning the current browser-session request.")

(defvar bili-auth--capture-file nil
  "Private temporary file for the current browser-session request.")

(defun bili-auth--valid-value-p (value)
  "Return non-nil when cookie VALUE is safe for an HTTP Cookie header."
  (and (stringp value)
       (not (string-blank-p value))
       (not (string-match-p "[;\r\n]" value))))

(defun bili-auth--unexpired-p (expires)
  "Return non-nil when browser cookie EXPIRES is current or session-scoped."
  (and (numberp expires)
       (or (<= expires 0) (> expires (float-time)))))

(defun bili-auth--bilibili-domain-p (domain)
  "Return non-nil when DOMAIN is Bilibili's registrable cookie domain."
  (and (stringp domain)
       (string-equal (downcase (string-remove-prefix "." domain))
                     "bilibili.com")))

(defun bili-auth--capture-source-valid-p (source)
  "Return non-nil when browser-session SOURCE matches the login origin."
  (when-let* ((source-url (and (listp source) (alist-get 'url source)))
              ((stringp source-url)))
    (condition-case nil
        (let ((url (url-generic-parse-url source-url)))
          (and (string-equal (url-type url) "https")
               (string-equal (downcase (or (url-host url) ""))
                             "www.bilibili.com")
               (null (url-user url))
               (null (url-password url))))
      (error nil))))

(defun bili-auth--captured-cookie (capture name)
  "Return the unique validated NAME cookie from browser CAPTURE."
  (let ((matches
         (cl-remove-if-not
          (lambda (cookie)
            (and (listp cookie)
                 (equal (alist-get 'name cookie) name)
                 (bili-auth--bilibili-domain-p
                  (alist-get 'domain cookie))))
          (browser-session-cookies capture))))
    (unless (= (length matches) 1)
      (error "Bilibili capture must contain exactly one %s cookie" name))
    (let* ((cookie (car matches))
           (value (alist-get 'value cookie))
           (expires (alist-get 'expires cookie)))
      (unless (and (bili-auth--valid-value-p value)
                   (bili-auth--unexpired-p expires)
                   (stringp (alist-get 'path cookie))
                   (eq (alist-get 'secure cookie) t))
        (error "Bilibili capture contains an invalid or expired %s cookie"
               name))
      cookie)))

(defun bili-auth--credential-from-capture (file)
  "Read and validate browser-session capture FILE."
  (let* ((capture (browser-session-read file))
         (source (alist-get 'source capture)))
    (unless (bili-auth--capture-source-valid-p source)
      (error "Browser capture is not scoped to www.bilibili.com"))
    (let ((sessdata (bili-auth--captured-cookie capture "SESSDATA"))
          (csrf (bili-auth--captured-cookie capture "bili_jct")))
      (bili-auth--credential-create
       :sessdata (alist-get 'value sessdata)
       :sessdata-expires (alist-get 'expires sessdata)
       :csrf (alist-get 'value csrf)
       :csrf-expires (alist-get 'expires csrf)))))

(defun bili-auth--validate-credential (credential)
  "Return CREDENTIAL after validating all persisted fields."
  (unless (and (bili-auth-credential-p credential)
               (bili-auth--valid-value-p
                (bili-auth-credential-sessdata credential))
               (bili-auth--unexpired-p
                (bili-auth-credential-sessdata-expires credential))
               (bili-auth--valid-value-p
                (bili-auth-credential-csrf credential))
               (bili-auth--unexpired-p
                (bili-auth-credential-csrf-expires credential)))
    (error "Bilibili credentials are invalid or expired; run M-x bili-login"))
  credential)

(defun bili-auth--write (credential)
  "Atomically persist validated CREDENTIAL with private permissions."
  (bili-auth--validate-credential credential)
  (let* ((file (expand-file-name bili-auth-file))
         (directory (file-name-directory file))
         temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary
                (make-temp-file (expand-file-name ".bili-session-" directory)
                                nil ".json"))
          (unless (eq system-type 'windows-nt)
            (set-file-modes temporary #o600))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temporary
              (insert
               (json-encode
                `((schema . ,bili-auth--schema)
                  (sessdata . ,(bili-auth-credential-sessdata credential))
                  (sessdata_expires
                   . ,(bili-auth-credential-sessdata-expires credential))
                  (csrf . ,(bili-auth-credential-csrf credential))
                  (csrf_expires
                   . ,(bili-auth-credential-csrf-expires credential)))))
              (insert "\n")))
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary
        (ignore-errors (delete-file temporary))))))

(defun bili-auth-credentials ()
  "Return validated imported Bilibili credentials."
  (unless (file-readable-p bili-auth-file)
    (user-error "Bilibili login is not configured; run M-x bili-login"))
  (let ((payload
         (condition-case nil
             (with-temp-buffer
               (insert-file-contents-literally bili-auth-file)
               (json-parse-buffer
                :object-type 'alist :array-type 'list
                :null-object nil :false-object nil))
           (error
            (error "Bilibili credential file is invalid; run M-x bili-login")))))
    (unless (equal (alist-get 'schema payload) bili-auth--schema)
      (error "Bilibili credential file has an unsupported schema"))
    (bili-auth--validate-credential
     (bili-auth--credential-create
      :sessdata (alist-get 'sessdata payload)
      :sessdata-expires (alist-get 'sessdata_expires payload)
      :csrf (alist-get 'csrf payload)
      :csrf-expires (alist-get 'csrf_expires payload)))))

(defun bili-auth-credentials-if-available ()
  "Return imported Bilibili credentials, or nil when none are configured."
  (when (file-readable-p bili-auth-file)
    (bili-auth-credentials)))

(defun bili-auth-cookie-header (credential)
  "Return the API Cookie header for validated CREDENTIAL."
  (bili-auth--validate-credential credential)
  (format "SESSDATA=%s; bili_jct=%s"
          (bili-auth-credential-sessdata credential)
          (bili-auth-credential-csrf credential)))

(defun bili-auth--delete-capture (file)
  "Delete private browser capture FILE when it exists."
  (when (and (stringp file) (file-exists-p file))
    (ignore-errors (delete-file file))))

(defun bili-auth--cancel-owned-capture (capture)
  "Cancel owned browser CAPTURE and remove its temporary file."
  (let ((request (car-safe capture))
        (file (cdr-safe capture)))
    (when request
      (browser-session-cancel request))
    (bili-auth--delete-capture file)
    (when (eq request bili-auth--capture-request)
      (setq bili-auth--capture-request nil
            bili-auth--capture-handle nil
            bili-auth--capture-file nil))))

(defun bili-auth--retire-capture ()
  "Retire the current browser-session request and its Appkit handle."
  (when (appkit-handle-p bili-auth--capture-handle)
    (appkit-retire-handle bili-auth--capture-handle))
  (setq bili-auth--capture-handle nil
        bili-auth--capture-request nil
        bili-auth--capture-file nil))

(defun bili-auth--capture-finished (file)
  "Import browser-session capture FILE."
  (unwind-protect
      (condition-case error-data
          (let ((credential (bili-auth--credential-from-capture file)))
            (bili-auth--write credential)
            (bili-auth--retire-capture)
            (bili-core-stop)
            (message "Imported Bilibili browser credentials"))
        (error
         (message "Bilibili credential import failed: %s"
                  (error-message-string error-data))))
    (bili-auth--retire-capture)
    (bili-auth--delete-capture file)))

(defun bili-auth--capture-failed (file restart-running failure)
  "Handle browser capture FAILURE for FILE and RESTART-RUNNING policy."
  (bili-auth--delete-capture file)
  (bili-auth--retire-capture)
  (if (and (not restart-running)
           (equal (browser-session-error-code failure)
                  "browser-restart-required")
           (yes-or-no-p
            "Restart the Bilibili login browser once to enable capture? "))
      (bili-auth--start-capture t)
    (message "Bilibili browser capture failed: %s"
             (browser-session-error-message failure))))

(defun bili-auth--start-capture (&optional restart-running)
  "Start browser capture, permitting a restart when RESTART-RUNNING is non-nil."
  (when (browser-session-request-live-p bili-auth--capture-request)
    (user-error "A Bilibili browser capture is already running"))
  (let ((file (make-temp-file "bili-browser-session-" nil ".json"))
        settled)
    (message "Opening Bilibili login window...")
    (condition-case error-data
        (let ((request
                (browser-session-capture
                 :url bili-auth--capture-url
                 :cookies bili-auth--cookie-names
                 :output-file file
                 :profile-root bili-auth-browser-profile-root
                 :restart-running restart-running
                 :callback
                 (lambda (_metadata)
                   (setq settled t)
                   (bili-auth--capture-finished file))
                 :errorback
                 (lambda (failure)
                   (setq settled t)
                   (bili-auth--capture-failed file restart-running failure)))))
          (unless settled
            (setq bili-auth--capture-request request
                  bili-auth--capture-file file
                  bili-auth--capture-handle
                  (appkit-register-handle
                   (bili-core-app) 'function (cons request file)
                   #'bili-auth--cancel-owned-capture)))
          request)
      (error
       (bili-auth--delete-capture file)
       (bili-auth--retire-capture)
       (signal (car error-data) (cdr error-data))))))

(defun bili-auth-capture ()
  "Capture and persist the minimum Bilibili browser session."
  (interactive)
  (bili-auth--start-capture))

(defun bili-auth-clear ()
  "Delete imported Bilibili credentials without changing the browser."
  (interactive)
  (cond
   ((and (appkit-handle-p bili-auth--capture-handle)
         (appkit-handle-alive-p bili-auth--capture-handle))
    (appkit-cancel-handle bili-auth--capture-handle))
   (bili-auth--capture-request
    (bili-auth--cancel-owned-capture
     (cons bili-auth--capture-request bili-auth--capture-file))))
  (bili-auth--retire-capture)
  (let ((file (expand-file-name bili-auth-file)))
    (when (file-exists-p file)
      (delete-file file)))
  (bili-core-stop)
  (message "Removed Bilibili browser credentials"))

(provide 'bili-auth)

;;; bili-auth.el ends here
