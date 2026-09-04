;;; bili-cover-test.el --- Tests for Bilibili cover Resources  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bili-cover)

(ert-deftest bili-cover-demand-is-shared-public-and-cancellable ()
  (let (captured canceled)
    (cl-letf
        (((symbol-function 'bili-cover--image-display-available-p)
          (lambda () t))
         ((symbol-function 'bili-cover--cached-file)
          (lambda (&rest _arguments) nil))
         ((symbol-function 'appkit-media-cache-image-resource-async)
          (lambda (resource cache-base _success _failure &rest options)
            (setq captured (list resource cache-base options))
            'cover-transfer))
         ((symbol-function 'appkit-media-transfer-p)
          (lambda (object) (eq object 'cover-transfer)))
         ((symbol-function 'appkit-media-cancel-transfer)
          (lambda (object) (setq canceled object))))
      (let* ((demand
              (bili-cover-demand
               '(video "BV1")
               "https://i0.hdslb.com/bfs/a.jpg"))
             (cancellation
              (funcall
               (appkit-resource-demand-loader demand)
               'context
               (appkit-resource-demand-input demand)
               #'ignore #'ignore)))
        (should (eq (appkit-resource-demand-sharing-policy demand)
                    'shared))
        (should (eq (appkit-resource-demand-cache-policy demand)
                    'while-interested))
        (should
         (equal (alist-get 'url (car captured))
                "https://i0.hdslb.com/bfs/a.jpg"))
        (let ((headers (plist-get (nth 2 captured) :headers)))
          (should (equal (cdr (assoc "Referer" headers))
                         "https://www.bilibili.com/"))
          (should (assoc "User-Agent" headers))
          (should-not (assoc-string "Cookie" headers t)))
        (funcall (appkit-cancellation-cancel cancellation))
        (should (eq canceled 'cover-transfer))))))

(ert-deftest bili-cover-resource-key-includes-source-revision ()
  (let ((old
         (bili-cover-resource-key
          '(video "BV1") "https://i0.hdslb.com/bfs/old.jpg"))
        (new
         (bili-cover-resource-key
          '(video "BV1") "https://i0.hdslb.com/bfs/new.jpg")))
    (should-not (equal old new))
    (should
     (equal old
            (bili-cover-resource-key
             '(video "BV1")
             "http://i0.hdslb.com/bfs/old.jpg")))))

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
   (bili-cover-normalize-url "https://user@i0.hdslb.com/a.jpg"))
  (should-not
   (bili-cover-normalize-url "https://i0.hdslb.com:444/a.jpg")))

(ert-deftest bili-cover-catalog-produces-display-only-slices ()
  (let ((descriptor '(image :type png :width 128 :height 72)))
    (cl-letf (((symbol-function 'bili-cover--display-frame)
               (lambda (&optional _surface) (selected-frame)))
              ((symbol-function 'bili-cover-image)
               (lambda (&rest _arguments) descriptor))
              ((symbol-function 'appkit-media-image-slice-rows)
               (lambda (_image)
                 (cl-loop for index below 4
                          collect
                          (propertize
                           " " 'display (list 'slice index))))))
      (dolist (line-count '(3 4))
        (let ((bili-cover-catalog-lines line-count))
          (pcase-let
              ((`(,columns . ,rows)
                (bili-cover-catalog-slices
                 nil '(video "BV1")
                 "https://i0.hdslb.com/a.jpg")))
            (should (>= columns 8))
            (should (= (length rows) line-count))
            (cl-loop for row in rows
                     for index from 0
                     do (should (= (length row) 1))
                     do (should-not (string-match-p "\n" row))
                     do (should
                         (equal (get-text-property 0 'display row)
                                (list 'slice index))))))))))

(provide 'bili-cover-test)

;;; bili-cover-test.el ends here
