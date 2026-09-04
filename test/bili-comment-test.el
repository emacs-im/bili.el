;;; bili-comment-test.el --- Tests for Bilibili generated comments  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-discussion)
(require 'bili-comment)
(require 'bili-test-helper)

(defun bili-comment-test--json (id author message &optional replies)
  "Return one comment JSON object with ID, AUTHOR, MESSAGE, and REPLIES."
  `((rpid . ,id)
    (mid . ,(+ id 1000))
    (ctime . 1700000000)
    (like . ,id)
    (rcount . ,(length replies))
    (member . ((mid . ,(number-to-string (+ id 1000)))
               (uname . ,author)))
    (content . ((message . ,message)))
    (replies . ,replies)))

(ert-deftest bili-comment-renders-discussion-and-cursor-pagination ()
  (let* ((video (bili-video-create :bvid "BV1comments" :aid 42))
         (nested (bili-comment-test--json 11 "Nested" "Reply body"))
         (root (bili-comment-test--json 1 "Alice" "Root body" (list nested)))
         (pinned (bili-comment-test--json 9 "Pinned" "Pinned body"))
         (older (bili-comment-test--json 2 "Bob" "Older body"))
         (appkit-discussion-connector-style 'text)
         surface observer requests)
    (unwind-protect
        (cl-letf
            (((symbol-function 'bili-cover--image-display-available-p)
              (lambda () nil))
             ((symbol-function 'bili-api-video-comments)
              (lambda (aid callback &rest options)
                (should (= aid 42))
                (let ((offset (plist-get options :offset)))
                  (push offset requests)
                  (funcall
                   callback
                   (if offset
                       `((cursor
                          . ((all_count . 3) (is_end . t)
                             (pagination_reply . ((next_offset . nil)))))
                         (replies . (,older)))
                     `((cursor
                        . ((all_count . 3) (is_end . nil)
                           (pagination_reply . ((next_offset . "next")))))
                       (top_replies . (,pinned))
                       (replies . (,root)))))))))
          (setq surface (bili-comment-open video))
          (bili-test-drain surface)
          (setq observer
                (buffer-local-value
                 'bili-comment--scroll-observer
                 (appkit-surface-buffer surface)))
          (bili-comment--maybe-auto-load surface nil 100 100)
          (bili-test-drain surface)
          (should (equal (nreverse requests) '(nil "next")))
          (should (appkit-scroll-observer-p observer))
          (let ((state (appkit-surface-model surface)))
            (should (equal (plist-get state :items) '(9 1 2)))
            (should (= (plist-get state :total) 3))
            (should (plist-get state :exhausted-p)))
          (with-current-buffer (appkit-surface-buffer surface)
            (let* ((root-key '(comment 42 1))
                   (reply-key '(comment 42 1 11))
                   (root-start
                    (save-excursion
                      (goto-char (point-min))
                      (when-let* ((match
                                   (text-property-search-forward
                                    appkit-discussion-key-property
                                    root-key #'equal)))
                        (prop-match-beginning match))))
                   (reply-start
                    (save-excursion
                      (goto-char (point-min))
                      (when-let* ((match
                                   (text-property-search-forward
                                    appkit-discussion-key-property
                                    reply-key #'equal)))
                        (prop-match-beginning match)))))
              (should root-start)
              (should reply-start)
              (should (= (get-text-property
                          reply-start appkit-discussion-depth-property)
                         1))
              (should
               (equal
                (get-text-property
                 reply-start appkit-discussion-parent-key-property)
                root-key))
              (should (stringp
                       (get-text-property root-start 'line-prefix)))
              (should (eq (char-after root-start) ?A)))))
      (bili-test-stop-surface surface)
      (when observer
        (should-not (appkit-scroll-observer-active-p observer)))
      (bili-core-stop))))

(provide 'bili-comment-test)

;;; bili-comment-test.el ends here
