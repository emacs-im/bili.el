;;; bili-live-test.el --- Tests for Bilibili live workflows  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bili-live)

(ert-deftest bili-live-cancel-after-synchronous-init-cancels-detail-child ()
  (let (canceled)
    (cl-letf (((symbol-function 'bili-api-live-room-init)
               (lambda (_room-id callback &rest _keys)
                 (funcall callback '((room_id . 42)))
                 nil))
              ((symbol-function 'bili-api-live-room)
               (lambda (_room-id _callback &rest _keys)
                 'detail-child))
              ((symbol-function 'bili-api-cancel)
               (lambda (child)
                 (push child canceled))))
      (let ((request (bili-live-resolve-room 7 #'ignore)))
        (should (bili-live-request-p request))
        (should (bili-live-cancel request))
        (should (equal canceled '(detail-child)))))))

(ert-deftest bili-live-resolve-room-selects-canonical-nested-detail ()
  (let (room failure owners)
    (cl-letf (((symbol-function 'bili-api-live-room-init)
               (lambda (_room-id callback &rest keys)
                 (push (plist-get keys :owner) owners)
                 (funcall callback '((room_id . 42)))
                 nil))
              ((symbol-function 'bili-api-live-room)
               (lambda (room-id callback &rest keys)
                 (should (= room-id 42))
                 (push (plist-get keys :owner) owners)
                 (funcall
                  callback
                  '((by_room_ids
                     (wrong
                      (room_id . 9)
                      (uname . "Wrong owner"))
                     (canonical
                      (room_id . 42)
                      (short_id . 7)
                      (title . "Canonical room")
                      (uname . "Canonical owner")
                      (cover . "https://example.test/cover.jpg")
                      (area_name . "Games")
                      (online . 123)
                      (live_status . 1)))))
                 nil)))
      (should-not
       (bili-live-resolve-room
        7
        (lambda (value) (setq room value))
        :errback (lambda (message) (setq failure message))
        :owner 'test-owner))
      (should-not failure)
      (should (bili-live-room-p room))
      (should (= (bili-live-room-id room) 42))
      (should (equal (bili-live-room-owner room) "Canonical owner"))
      (should (equal owners '(test-owner test-owner))))))

(ert-deftest bili-live-resolve-room-errors-when-canonical-detail-is-missing ()
  (let (room failure)
    (cl-letf (((symbol-function 'bili-api-live-room-init)
               (lambda (_room-id callback &rest _keys)
                 (funcall callback '((room_id . 42)))
                 nil))
              ((symbol-function 'bili-api-live-room)
               (lambda (_room-id callback &rest _keys)
                 (funcall
                  callback
                  '((by_room_ids
                     (other
                      (room_id . 9)
                      (uname . "Other owner")))))
                 nil)))
      (should-not
       (bili-live-resolve-room
        7
        (lambda (value) (setq room value))
        :errback (lambda (message) (setq failure message))))
      (should-not room)
      (should (stringp failure))
      (should (string-match-p "canonical room 42" failure)))))

(provide 'bili-live-test)

;;; bili-live-test.el ends here
