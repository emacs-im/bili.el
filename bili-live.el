;;; bili-live.el --- Bilibili live-room workflows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Resolve short live-room identifiers and normalize live directory responses.

;;; Code:

(require 'cl-lib)
(require 'appkit-effect)
(require 'bili-api)
(require 'bili-model)

(cl-defstruct (bili-live-request
               (:constructor bili-live--request-create))
  "One two-step live-room resolution request."
  child
  callback
  errback
  owner
  settled-p)

(defun bili-live--fail (request message)
  "Settle live REQUEST through its error callback with MESSAGE."
  (unless (bili-live-request-settled-p request)
    (setf (bili-live-request-settled-p request) t
          (bili-live-request-child request) nil)
    (funcall (bili-live-request-errback request) message)))

(defun bili-live--succeed (request room)
  "Settle live REQUEST through its callback with ROOM."
  (unless (bili-live-request-settled-p request)
    (setf (bili-live-request-settled-p request) t
          (bili-live-request-child request) nil)
    (funcall (bili-live-request-callback request) room)))

(defun bili-live-cancel (request)
  "Cancel opaque live-room resolution REQUEST."
  (unless (bili-live-request-p request)
    (error "Invalid Bilibili live-room request"))
  (unless (bili-live-request-settled-p request)
    (setf (bili-live-request-settled-p request) t)
    (when-let* ((child (bili-live-request-child request)))
      (bili-api-cancel child))
    (setf (bili-live-request-child request) nil))
  t)

(defun bili-live--room-detail (data canonical-id)
  "Return CANONICAL-ID's room object from base-info DATA."
  (let ((rooms (and (listp data) (alist-get 'by_room_ids data))))
    (unless (listp rooms)
      (error "Bilibili live response has invalid room details"))
    (or
     (cl-loop for entry in rooms
              for room = (and (consp entry) (cdr entry))
              when (and (listp room)
                        (= (bili-model--number (alist-get 'room_id room))
                           canonical-id))
              return room)
     (error "Bilibili live response lacks canonical room %d" canonical-id))))

(cl-defun bili-live-resolve-room
    (room-id callback &key errback owner)
  "Resolve ROOM-ID and call CALLBACK with a normalized live room.

ERRBACK receives a readable failure string.  OWNER owns both API steps."
  (unless (and (integerp room-id) (> room-id 0))
    (error "Bilibili live room id must be positive"))
  (unless (functionp callback)
    (error "Bilibili live-room callback is not callable"))
  (let* ((error-fn (or errback (lambda (message) (message "%s" message))))
         (request
           (bili-live--request-create
            :callback callback :errback error-fn :owner owner))
         (current 'init)
         (init-callback-ran nil))
    (unless (functionp error-fn)
      (error "Bilibili live-room error callback is not callable"))
    (cl-labels
        ((fail-current
           (stage message)
           (when (and (eq current stage)
                      (not (bili-live-request-settled-p request)))
             (setq current nil)
             (bili-live--fail request message)))
         (start-detail
           (canonical-id)
           (setq current 'detail)
           (setf (bili-live-request-child request) nil)
           (let ((detail-callback-ran nil)
                 detail-child)
             (setq
              detail-child
              (bili-api-live-room
               canonical-id
               (lambda (data)
                 (setq detail-callback-ran t)
                 (when (and (eq current 'detail)
                            (not (bili-live-request-settled-p request)))
                   (setq current nil)
                   (condition-case error-data
                       (bili-live--succeed
                        request
                        (bili-model-live-room-from-json
                         (bili-live--room-detail data canonical-id)))
                     (error
                      (bili-live--fail
                       request (error-message-string error-data))))))
               :errback
               (lambda (message)
                 (setq detail-callback-ran t)
                 (fail-current 'detail message))
               :owner owner))
             (when (and (not detail-callback-ran)
                        (eq current 'detail)
                        (not (bili-live-request-settled-p request)))
               (setf (bili-live-request-child request) detail-child)))))
      (let ((init-child
             (bili-api-live-room-init
              room-id
              (lambda (initial)
                (setq init-callback-ran t)
                (when (and (eq current 'init)
                           (not (bili-live-request-settled-p request)))
                  (setf (bili-live-request-child request) nil)
                  (let ((canonical-id
                         (bili-model--number (alist-get 'room_id initial))))
                    (if (zerop canonical-id)
                        (fail-current
                         'init "Bilibili could not resolve the live room")
                      (start-detail canonical-id)))))
              :errback
              (lambda (message)
                (setq init-callback-ran t)
                (fail-current 'init message))
              :owner owner)))
        (when (and (not init-callback-ran)
                   (eq current 'init)
                   (not (bili-live-request-settled-p request)))
          (setf (bili-live-request-child request) init-child))))
    (unless (bili-live-request-settled-p request)
      request)))

(defun bili-live-effect-cancellation (request)
  "Return transport cancellation for effect-owned live REQUEST, or nil."
  (when (bili-live-request-p request)
    (appkit-cancellation-create
     :kind 'transport
     :cancel (lambda () (bili-live-cancel request)))))

(defun bili-live-catalog-items (data)
  "Return normalized live catalog items from directory DATA."
  (let ((items (or (alist-get 'recommend_room_list data)
                   (alist-get 'list data))))
    (unless (listp items)
      (error "Bilibili live directory response has no room list"))
    (delq nil (mapcar #'bili-model-live-catalog-item items))))

(provide 'bili-live)

;;; bili-live.el ends here
