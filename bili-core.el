;;; bili-core.el --- Appkit session state for bili.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the default Appkit application and canonical Bilibili observations.
;; Views own query and pagination state; transport remains in bili-api.el.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'bili-model)

(defgroup bili nil
  "Browse and play Bilibili in Emacs."
  :group 'multimedia)

(cl-defstruct (bili-core-session
               (:constructor bili-core--session-create))
  "Canonical state owned by one bili.el Appkit application."
  revision
  catalog
  comments
  videos
  rooms
  covers
  wbi-key
  wbi-expires-at)

(defun bili-core--make-session ()
  "Return initialized canonical Bilibili application state."
  (bili-core--session-create
   :revision 0
   :catalog (make-hash-table :test #'equal)
   :comments (make-hash-table :test #'equal)
   :videos (make-hash-table :test #'equal)
   :rooms (make-hash-table :test #'equal)
   :covers (make-hash-table :test #'equal)))

(defvar bili-core--app nil
  "Default live Appkit application for bili.el.")

(defun bili-core--shutdown (app)
  "Forget APP after Appkit finishes its shutdown."
  (when (eq app bili-core--app)
    (setq bili-core--app nil)))

(appkit-define-app-kind bili
  :shutdown #'bili-core--shutdown)

(defun bili-core-app ()
  "Return bili.el's live default Appkit application."
  (unless (appkit-app-live-p bili-core--app)
    (setq bili-core--app
          (appkit-start-app
           'bili :id 'default :state (bili-core--make-session)
           :shutdown #'bili-core--shutdown)))
  bili-core--app)

(defun bili-core-session (&optional app)
  "Return validated canonical state for APP or the default application."
  (let* ((target (or app (bili-core-app)))
         (state (and (appkit-app-p target) (appkit-app-state target))))
    (unless (bili-core-session-p state)
      (error "Invalid Bilibili application state"))
    state))

(defun bili-core-stop ()
  "Stop bili.el and cancel every owned view, request, and media session."
  (interactive)
  (when (appkit-app-live-p bili-core--app)
    (appkit-stop-app bili-core--app))
  nil)

(defun bili-core-observe (&optional app)
  "Return a new canonical observation revision for APP."
  (let ((session (bili-core-session app)))
    (cl-incf (bili-core-session-revision session))))

(defun bili-core-invalidate-resource (app resource)
  "Invalidate RESOURCE in every live view owned by APP."
  (unless (appkit-app-live-p app)
    (error "Cannot invalidate a dead Bilibili application"))
  (maphash
   (lambda (_id view)
     (when (appkit-view-live-p view)
       (appkit-request-sync view :resource resource :position t)))
   (appkit-app-view-registry app)))

(defun bili-core-store-catalog-items (app items)
  "Store normalized catalog ITEMS in APP and return their stable keys.

Observe the batch once and invalidate only resources whose canonical item
changed."
  (dolist (item items)
    (unless (bili-catalog-item-p item)
      (error "Invalid Bilibili catalog item")))
  (let ((session (bili-core-session app))
        keys
        changed)
    (dolist (item items)
      (let ((key (list (bili-catalog-item-kind item)
                       (bili-catalog-item-id item))))
        (push key keys)
        (unless (equal item (gethash key (bili-core-session-catalog session)))
          (puthash key item (bili-core-session-catalog session))
          (push key changed))))
    (when changed
      (bili-core-observe app)
      (dolist (key changed)
        (bili-core-invalidate-resource app (list 'catalog key))))
    (nreverse keys)))

(defun bili-core-catalog-item (app key)
  "Return APP's canonical catalog item at KEY, or nil."
  (gethash key (bili-core-session-catalog (bili-core-session app))))

(defun bili-core-store-comments (app aid comments)
  "Store normalized COMMENTS for video AID in APP and return their ids.

Observe the batch once and invalidate only changed comment resources."
  (unless (and (integerp aid) (> aid 0))
    (error "Invalid Bilibili comment AID"))
  (dolist (comment comments)
    (unless (bili-comment-p comment)
      (error "Invalid Bilibili comment")))
  (let ((session (bili-core-session app))
        ids
        changed)
    (dolist (comment comments)
      (let* ((id (bili-comment-id comment))
             (key (cons aid id)))
        (push id ids)
        (unless (equal comment
                       (gethash key (bili-core-session-comments session)))
          (puthash key comment (bili-core-session-comments session))
          (push id changed))))
    (when changed
      (bili-core-observe app)
      (dolist (id changed)
        (bili-core-invalidate-resource app (list 'comment aid id))))
    (nreverse ids)))

(defun bili-core-comment (app aid comment-id)
  "Return APP's canonical COMMENT-ID for video AID, or nil."
  (gethash (cons aid comment-id)
           (bili-core-session-comments (bili-core-session app))))

(defun bili-core-cover-state (app key)
  "Return APP's canonical cover acquisition state at stable entity KEY."
  (gethash key (bili-core-session-covers (bili-core-session app))))

(defun bili-core-store-cover-state (app key state)
  "Store cover STATE for stable entity KEY in APP.

Observe and invalidate the corresponding cover resource only when STATE
changes."
  (let* ((session (bili-core-session app))
         (covers (bili-core-session-covers session)))
    (unless (equal state (gethash key covers))
      (puthash key state covers)
      (bili-core-observe app)
      (bili-core-invalidate-resource app (cons 'cover key)))
    state))

(defun bili-core-store-video (app video)
  "Store normalized VIDEO in APP and return its BVID."
  (unless (bili-video-p video)
    (error "Invalid Bilibili video"))
  (let* ((session (bili-core-session app))
         (bvid (bili-video-bvid video)))
    (unless (equal video (gethash bvid (bili-core-session-videos session)))
      (puthash bvid video (bili-core-session-videos session))
      (bili-core-observe app)
      (bili-core-invalidate-resource app (list 'video bvid)))
    bvid))

(defun bili-core-video (app bvid)
  "Return APP's canonical video BVID, or nil."
  (gethash bvid (bili-core-session-videos (bili-core-session app))))

(defun bili-core-store-room (app room)
  "Store normalized live ROOM in APP and return its room id."
  (unless (bili-live-room-p room)
    (error "Invalid Bilibili live room"))
  (let* ((session (bili-core-session app))
         (room-id (bili-live-room-id room)))
    (unless (equal room (gethash room-id (bili-core-session-rooms session)))
      (puthash room-id room (bili-core-session-rooms session))
      (bili-core-observe app)
      (bili-core-invalidate-resource app (list 'live room-id)))
    room-id))

(defun bili-core-room (app room-id)
  "Return APP's canonical live ROOM-ID, or nil."
  (gethash room-id (bili-core-session-rooms (bili-core-session app))))


(provide 'bili-core)

;;; bili-core.el ends here
