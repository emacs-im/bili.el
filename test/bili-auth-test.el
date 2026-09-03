;;; bili-auth-test.el --- Tests for Bilibili credentials  -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'bili-auth)

(defun bili-auth-test--capture (expires)
  "Return a browser-session capture object expiring at EXPIRES."
  `((schema . 1)
    (source . ((browser . "chrome")
               (url . "https://www.bilibili.com/")))
    (cookies
     . (((name . "SESSDATA") (value . "session-value")
         (domain . ".bilibili.com") (path . "/")
         (expires . ,expires) (secure . t) (httpOnly . t))
        ((name . "bili_jct") (value . "csrf-value")
         (domain . ".bilibili.com") (path . "/")
         (expires . 0.0) (secure . t) (httpOnly . nil))))))

(ert-deftest bili-auth-validates-capture-origin-domain-and-expiry ()
  (let ((file (make-temp-file "bili-auth-capture-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (json-encode
                     (bili-auth-test--capture (+ (float-time) 3600)))))
          (let ((credential (bili-auth--credential-from-capture file)))
            (should (equal (bili-auth-credential-sessdata credential)
                           "session-value"))
            (should (equal (bili-auth-cookie-header credential)
                           "SESSDATA=session-value; bili_jct=csrf-value")))
          (with-temp-file file
            (insert (json-encode
                     (bili-auth-test--capture (- (float-time) 1)))))
          (should-error (bili-auth--credential-from-capture file)))
      (delete-file file))))

(ert-deftest bili-auth-round-trips-private-credential-file ()
  (let* ((directory (make-temp-file "bili-auth-test-" t))
         (bili-auth-file (expand-file-name "session.json" directory))
         (credential
          (bili-auth--credential-create
           :sessdata "session-value"
           :sessdata-expires (+ (float-time) 3600)
           :csrf "csrf-value"
           :csrf-expires 0.0)))
    (unwind-protect
        (progn
          (bili-auth--write credential)
          (should (file-readable-p bili-auth-file))
          (unless (eq system-type 'windows-nt)
            (should (= (file-modes bili-auth-file) #o600)))
          (let ((read-back (bili-auth-credentials)))
            (should (equal (bili-auth-credential-sessdata read-back)
                           "session-value"))
            (should (equal (bili-auth-credential-csrf read-back)
                           "csrf-value"))))
      (delete-directory directory t))))


(ert-deftest bili-auth-capture-is-minimal-and-appkit-owned ()
  (let ((bili-auth--capture-request nil)
        (bili-auth--capture-handle nil)
        (bili-auth--capture-file nil)
        arguments
        canceled
        capture-file)
    (unwind-protect
        (cl-letf (((symbol-function 'browser-session-request-live-p)
                   (lambda (_request) nil))
                  ((symbol-function 'browser-session-capture)
                   (lambda (&rest keys)
                     (setq arguments keys
                           capture-file (plist-get keys :output-file))
                     'browser-request))
                  ((symbol-function 'browser-session-cancel)
                   (lambda (request)
                     (setq canceled request)))
                  ((symbol-function 'message) #'ignore))
          (should (eq (bili-auth-capture) 'browser-request))
          (should (equal (plist-get arguments :cookies)
                         '("SESSDATA" "bili_jct")))
          (should-not (plist-member arguments :all-origin-cookies))
          (should (appkit-handle-alive-p bili-auth--capture-handle))
          (bili-core-stop)
          (should (eq canceled 'browser-request))
          (should-not (file-exists-p capture-file))
          (should-not bili-auth--capture-request)
          (should-not bili-auth--capture-handle)
          (should-not bili-auth--capture-file))
      (setq bili-auth--capture-request nil
            bili-auth--capture-handle nil
            bili-auth--capture-file nil)
      (when (and capture-file (file-exists-p capture-file))
        (delete-file capture-file))
      (bili-core-stop))))
(provide 'bili-auth-test)

;;; bili-auth-test.el ends here
