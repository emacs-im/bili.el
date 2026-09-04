;;; bili-core.el --- Canonical Bilibili App runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the default UI-free Bilibili App and its immutable canonical entity
;; store.  Surface-local membership, pagination, and presentation state live in
;; generated Surfaces.  Finite transport work is declared by App updates.

;;; Code:

(require 'cl-lib)
(require 'appkit-app)
(require 'appkit-command)
(require 'appkit-context)
(require 'bili-model)

(declare-function bili-browse--app-update "bili-browse")
(declare-function bili-comment--app-update "bili-comment")
(declare-function bili-detail--app-update "bili-detail")

(defgroup bili nil
  "Browse and play Bilibili in Emacs."
  :group 'multimedia)

(cl-defstruct (bili-core-session
               (:constructor bili-core--session-create))
  "Canonical state committed only by the Bilibili App update."
  revision
  catalog
  comments
  videos
  rooms)

(defun bili-core--make-session ()
  "Return initialized canonical Bilibili App state."
  (bili-core--session-create
   :revision 0
   :catalog (make-hash-table :test #'equal)
   :comments (make-hash-table :test #'equal)
   :videos (make-hash-table :test #'equal)
   :rooms (make-hash-table :test #'equal)))

(defvar bili-core--app nil
  "Default live Bilibili App runtime.")

(defun bili-core--shutdown (app)
  "Forget APP after AppKit finishes its shutdown."
  (when (eq app bili-core--app)
    (setq bili-core--app nil)))

(defun bili-core--app-init (_context _input)
  "Initialize one canonical Bilibili App model."
  (appkit-next :model (bili-core--make-session)
               :render appkit-render-none))

(defun bili-core--app-update (context model message)
  "Advance canonical Bilibili MODEL for MESSAGE."
  (pcase message
    (`(catalog . ,_)
     (bili-browse--app-update context model message))
    (`(comments . ,_)
     (bili-comment--app-update context model message))
    (`(detail . ,_)
     (bili-detail--app-update context model message))
    (_ (appkit-next-reject
        (format "Unsupported Bilibili App message: %S" message)))))

(defconst bili-core--app-type
  (appkit-app-type-create
   :name 'bili
   :init #'bili-core--app-init
   :update #'bili-core--app-update
   :shutdown #'bili-core--shutdown)
  "Canonical Bilibili App type.")

(defun bili-core-app ()
  "Return Bilibili's live default App runtime."
  (unless (appkit-app-live-p bili-core--app)
    (setq bili-core--app
          (appkit-app-start bili-core--app-type :identity 'default)))
  bili-core--app)

(defun bili-core-session (&optional owner)
  "Return validated canonical state for OWNER.

OWNER may be a Bilibili App, an App read view, a session value, or nil for the
default App."
  (let ((state
         (cond
          ((bili-core-session-p owner) owner)
          ((appkit-app-read-view-p owner)
           (appkit-app-read-view-model owner))
          ((appkit-app-p owner) (appkit-app-model owner))
          ((null owner) (appkit-app-model (bili-core-app)))
          (t nil))))
    (unless (bili-core-session-p state)
      (error "Invalid Bilibili canonical session"))
    state))

(defun bili-core-stop ()
  "Stop Bilibili and every owned Surface and Effect."
  (interactive)
  (when (appkit-app-p bili-core--app)
    (appkit-app-close bili-core--app))
  (setq bili-core--app nil))

(defun bili-core--upsert (table values key-function predicate)
  "Return a possibly copied TABLE and stable keys for VALUES."
  (let ((next table) copied-p keys)
    (dolist (value values)
      (unless (funcall predicate value)
        (error "Invalid canonical Bilibili entity"))
      (let ((key (funcall key-function value)))
        (push key keys)
        (unless (equal value (gethash key table))
          (unless copied-p
            (setq next (copy-hash-table table)
                  copied-p t))
          (puthash key value next))))
    (cons next (nreverse keys))))

(defun bili-core--commit-table (session slot table)
  "Return SESSION with changed SLOT replaced by TABLE."
  (let ((next (copy-bili-core-session session)))
    (setf (bili-core-session-revision next)
          (1+ (bili-core-session-revision session)))
    (pcase slot
      ('catalog (setf (bili-core-session-catalog next) table))
      ('comments (setf (bili-core-session-comments next) table))
      ('videos (setf (bili-core-session-videos next) table))
      ('rooms (setf (bili-core-session-rooms next) table))
      (_ (error "Unknown Bilibili canonical table: %S" slot)))
    next))

(defun bili-core--put-catalog-items (session items)
  "Return `(SESSION . KEYS)' after committing normalized catalog ITEMS."
  (let* ((session (bili-core-session session))
         (current (bili-core-session-catalog session))
         (result
          (bili-core--upsert
           current items
           (lambda (item)
             (list (bili-catalog-item-kind item)
                   (bili-catalog-item-id item)))
           #'bili-catalog-item-p)))
    (cons (if (eq current (car result))
              session
            (bili-core--commit-table session 'catalog (car result)))
          (cdr result))))

(defun bili-core-catalog-item (owner key)
  "Return OWNER's canonical catalog item at KEY, or nil."
  (gethash key (bili-core-session-catalog (bili-core-session owner))))

(defun bili-core--put-comments (session aid comments)
  "Return `(SESSION . IDS)' after committing COMMENTS for video AID."
  (unless (and (integerp aid) (> aid 0))
    (error "Invalid Bilibili comment AID"))
  (let* ((session (bili-core-session session))
         (current (bili-core-session-comments session))
         (result
          (bili-core--upsert
           current comments
           (lambda (comment) (cons aid (bili-comment-id comment)))
           #'bili-comment-p)))
    (cons (if (eq current (car result))
              session
            (bili-core--commit-table session 'comments (car result)))
          (mapcar #'cdr (cdr result)))))

(defun bili-core-comment (owner aid comment-id)
  "Return OWNER's canonical COMMENT-ID for video AID, or nil."
  (gethash (cons aid comment-id)
           (bili-core-session-comments (bili-core-session owner))))

(defun bili-core--put-video (session video)
  "Return SESSION with normalized VIDEO committed."
  (let* ((session (bili-core-session session))
         (current (bili-core-session-videos session))
         (result
          (bili-core--upsert current (list video) #'bili-video-bvid
                             #'bili-video-p)))
    (if (eq current (car result))
        session
      (bili-core--commit-table session 'videos (car result)))))

(defun bili-core-video (owner bvid)
  "Return OWNER's canonical video BVID, or nil."
  (gethash bvid (bili-core-session-videos (bili-core-session owner))))

(defun bili-core--put-room (session room)
  "Return SESSION with normalized live ROOM committed."
  (let* ((session (bili-core-session session))
         (current (bili-core-session-rooms session))
         (result
          (bili-core--upsert current (list room) #'bili-live-room-id
                             #'bili-live-room-p)))
    (if (eq current (car result))
        session
      (bili-core--commit-table session 'rooms (car result)))))

(defun bili-core-room (owner room-id)
  "Return OWNER's canonical live ROOM-ID, or nil."
  (gethash room-id (bili-core-session-rooms (bili-core-session owner))))

(provide 'bili-core)

;;; bili-core.el ends here
