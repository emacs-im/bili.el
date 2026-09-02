;;; bili-cover-test.el --- Tests for Bilibili covers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-cover)

(ert-deftest bili-cover-fetch-is-public-cookie-free-and-app-owned ()
  (let ((app (bili-core-app))
        captured
        canceled)
    (unwind-protect
        (cl-letf (((symbol-function
                    'appkit-media-inline-image-rendering-available-p)
                   (lambda () t))
                  ((symbol-function 'bili-cover--cached-file)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'appkit-media-cache-image-resource-async)
                   (lambda (resource cache-base _success _error &rest options)
                     (setq captured (list resource cache-base options))
                     'cover-transfer))
                  ((symbol-function 'appkit-media-transfer-p)
                   (lambda (object) (eq object 'cover-transfer)))
                  ((symbol-function 'appkit-media-cancel-transfer)
                   (lambda (object) (setq canceled object))))
          (should
           (eq (bili-cover-prefetch
                app '(video "BV1") "https://i0.hdslb.com/bfs/a.jpg")
               'cover-transfer))
          (should
           (equal (alist-get 'url (car captured))
                  "https://i0.hdslb.com/bfs/a.jpg"))
          (let ((headers (plist-get (nth 2 captured) :headers)))
            (should (equal (cdr (assoc "Referer" headers))
                           "https://www.bilibili.com/"))
            (should (assoc "User-Agent" headers))
            (should-not (assoc-string "Cookie" headers t)))
          (should (eq (plist-get
                       (bili-core-cover-state app '(video "BV1")) :status)
                      'pending))
          (bili-core-stop)
          (should (eq canceled 'cover-transfer)))
      (bili-core-stop))))

(ert-deftest bili-cover-stale-completion-cannot-replace-new-source ()
  (let ((app (bili-core-app)) callbacks)
    (unwind-protect
        (cl-letf (((symbol-function
                    'appkit-media-inline-image-rendering-available-p)
                   (lambda () t))
                  ((symbol-function 'bili-cover--cached-file)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'appkit-media-cache-image-resource-async)
                   (lambda (resource _cache-base success _error &rest _options)
                     (push (cons (alist-get 'url resource) success) callbacks)
                     nil)))
          (bili-cover-prefetch
           app '(video "BV1") "https://i0.hdslb.com/bfs/old.jpg")
          (bili-cover-prefetch
           app '(video "BV1") "https://i0.hdslb.com/bfs/new.jpg")
          (funcall (cdr (assoc "https://i0.hdslb.com/bfs/old.jpg" callbacks))
                   "/tmp/old.jpg")
          (let ((state (bili-core-cover-state app '(video "BV1"))))
            (should (equal (plist-get state :url)
                           "https://i0.hdslb.com/bfs/new.jpg"))
            (should (eq (plist-get state :status) 'pending)))
          (funcall (cdr (assoc "https://i0.hdslb.com/bfs/new.jpg" callbacks))
                   "/tmp/new.jpg")
          (let ((state (bili-core-cover-state app '(video "BV1"))))
            (should (equal (plist-get state :file) "/tmp/new.jpg"))
            (should (eq (plist-get state :status) 'ready))))
      (bili-core-stop))))


(ert-deftest bili-cover-upgrades-only-trusted-image-origins ()
  (should
   (equal
    (bili-cover-normalize-url "http://i0.hdslb.com/bfs/archive/a.jpg")
    "https://i0.hdslb.com/bfs/archive/a.jpg"))
  (should
   (equal
    (bili-cover-normalize-url "//archive.biliimg.com/a.webp")
    "https://archive.biliimg.com/a.webp"))
  (should-not
   (bili-cover-normalize-url "https://hdslb.com.evil.test/a.jpg"))
  (should-not
   (bili-cover-normalize-url "https://user@i0.hdslb.com/a.jpg")))
(ert-deftest bili-cover-catalog-produces-four-fixed-width-slices ()
  (let* ((app (bili-core-app))
         (view
          (appkit-open-view
           :app app :id 'cover-test :mode #'special-mode
           :buffer-name " *bili-cover-test*" :state nil
           :sync-function #'ignore :parts nil :select nil))
         (buffer (appkit-view-buffer view))
         (descriptor '(image :type png :width 128 :height 72))
         (bili-cover-catalog-lines 4))
    (unwind-protect
        (cl-letf (((symbol-function 'bili-cover--display-frame)
                   (lambda (&optional _view) (selected-frame)))
                  ((symbol-function 'bili-cover-image)
                   (lambda (&rest _arguments) descriptor))
                  ((symbol-function 'appkit-media-image-slice-rows)
                   (lambda (_image)
                     (cl-loop for index below 4
                              collect
                              (propertize
                               " " 'display (list 'slice index))))))
          (with-current-buffer buffer
            (let ((rows
                   (bili-cover-catalog-slice-rows
                    view '(video "BV1") "https://i0.hdslb.com/a.jpg")))
              (should (= (length rows) 4))
              (should (apply #'= (mapcar #'string-width rows)))
              (cl-loop for row in rows
                       for index from 0
                       do (should-not (string-match-p "\n" row))
                       do (should
                           (equal (get-text-property 0 'display row)
                                  (list 'slice index)))))))
      (bili-core-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'bili-cover-test)

;;; bili-cover-test.el ends here
