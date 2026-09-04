;;; bili-core.el --- Canonical Bilibili App runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the default UI-free Bilibili App and its canonical entity store.
;; Surface-local membership, pagination, and presentation state live in the
;; catalog, detail, and comment Surface models.  Finite transport work is
;; declared by the App update as keyed Effects.

;;; Code:

(require 'cl-lib)
(require 'appkit-app)
(require 'appkit-command)
(require 'appkit-context)
(require 'bili-model)

(declare-function bili-browse--app-update "bili-browse")
(declare-function bili-comment--app-update "bili-comment")
(declare-function bili-detail--app-update "bili-detail")
(declare-function bili-cover--app-update "bili-cover")

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
  rooms
  covers)

(defun bili-core--make-session ()
  "Return initialized canonical Bilibili App state."
  (bili-core--session-create
   :revision 0
   :catalog (make-hash-table :test #'equal)
   :comments (make-hash-table :test #'equal)
   :videos (make-hash-table :test #'equal)
   :rooms (make-hash-table :test #'equal)
   :covers (make-hash-table :test #'equal)))

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
    (`(cover . ,_)
     (bili-cover--app-update context model message))
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

(defun bili-core-observe (session)
  "Advance and return canonical observation revision in SESSION."
  (cl-incf (bili-core-session-revision (bili-core-session session))))

(defun bili-core-store-catalog-items (session items)
  "Commit normalized catalog ITEMS in SESSION and return stable keys."
  (dolist (item items)
    (unless (bili-catalog-item-p item)
      (error "Invalid Bilibili catalog item")))
  (let ((session (bili-core-session session)) keys changed-p)
    (dolist (item items)
      (let ((key (list (bili-catalog-item-kind item)
                       (bili-catalog-item-id item))))
        (push key keys)
        (unless (equal item (gethash key (bili-core-session-catalog session)))
          (puthash key item (bili-core-session-catalog session))
          (setq changed-p t))))
    (when changed-p (bili-core-observe session))
    (nreverse keys)))

(defun bili-core-catalog-item (owner key)
  "Return OWNER's canonical catalog item at KEY, or nil."
  (gethash key (bili-core-session-catalog (bili-core-session owner))))

(defun bili-core-store-comments (session aid comments)
  "Commit normalized COMMENTS for video AID in SESSION and return ids."
  (unless (and (integerp aid) (> aid 0))
    (error "Invalid Bilibili comment AID"))
  (dolist (comment comments)
    (unless (bili-comment-p comment)
      (error "Invalid Bilibili comment")))
  (let ((session (bili-core-session session)) ids changed-p)
    (dolist (comment comments)
      (let* ((id (bili-comment-id comment))
             (key (cons aid id)))
        (push id ids)
        (unless (equal comment
                       (gethash key (bili-core-session-comments session)))
          (puthash key comment (bili-core-session-comments session))
          (setq changed-p t))))
    (when changed-p (bili-core-observe session))
    (nreverse ids)))

(defun bili-core-comment (owner aid comment-id)
  "Return OWNER's canonical COMMENT-ID for video AID, or nil."
  (gethash (cons aid comment-id)
           (bili-core-session-comments (bili-core-session owner))))

(defun bili-core-cover-state (owner key)
  "Return OWNER's canonical cover state at stable entity KEY."
  (gethash key (bili-core-session-covers (bili-core-session owner))))

(defun bili-core-store-cover-state (session key state)
  "Commit cover STATE for stable entity KEY in SESSION."
  (let* ((session (bili-core-session session))
         (covers (bili-core-session-covers session)))
    (unless (equal state (gethash key covers))
      (puthash key state covers)
      (bili-core-observe session))
    state))

(defun bili-core-store-video (session video)
  "Commit normalized VIDEO in SESSION and return its BVID."
  (unless (bili-video-p video)
    (error "Invalid Bilibili video"))
  (let* ((session (bili-core-session session))
         (bvid (bili-video-bvid video)))
    (unless (equal video (gethash bvid (bili-core-session-videos session)))
      (puthash bvid video (bili-core-session-videos session))
      (bili-core-observe session))
    bvid))

(defun bili-core-video (owner bvid)
  "Return OWNER's canonical video BVID, or nil."
  (gethash bvid (bili-core-session-videos (bili-core-session owner))))

(defun bili-core-store-room (session room)
  "Commit normalized live ROOM in SESSION and return its room id."
  (unless (bili-live-room-p room)
    (error "Invalid Bilibili live room"))
  (let* ((session (bili-core-session session))
         (room-id (bili-live-room-id room)))
    (unless (equal room (gethash room-id (bili-core-session-rooms session)))
      (puthash room-id room (bili-core-session-rooms session))
      (bili-core-observe session))
    room-id))

(defun bili-core-room (owner room-id)
  "Return OWNER's canonical live ROOM-ID, or nil."
  (gethash room-id (bili-core-session-rooms (bili-core-session owner))))

(provide 'bili-core)

;;; bili-core.el ends here
