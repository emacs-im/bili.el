;;; bili-evil.el --- Optional Evil integration for bili.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Define modal bindings through Appkit's Evil integration without raising an
;; application keymap above Evil's native normal-state grammar.

;;; Code:

(require 'appkit-evil)
(require 'bili-browse)
(require 'bili-detail)

(defcustom bili-evil-initial-state 'normal
  "Initial Evil state for read-only bili.el views.

When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
                 (const :tag "Normal" normal)
                 (const :tag "Motion" motion)
                 (const :tag "Emacs" emacs)
                 (symbol :tag "Custom state"))
  :group 'bili)

(defconst bili-evil--application-modes
  '(bili-browse-mode bili-detail-mode)
  "Read-only bili.el modes participating in Evil integration.")

(defun bili-evil-setup ()
  "Install bili.el's optional Evil integration.
Safe to call repeatedly and before or after Evil loads."
  (when appkit-evil-enable-integration
    (appkit-evil-set-initial-states
     bili-evil--application-modes bili-evil-initial-state)
    (appkit-evil-define-readonly-keys 'bili-browse-mode-map)
    (appkit-evil-define-readonly-keys 'bili-detail-mode-map)
    (appkit-evil-map
      (:map bili-browse-mode-map
       :nm
       "RET" #'bili-browse-activate
       "<return>" #'bili-browse-activate
       "g r" #'bili-browse-refresh
       "g j" #'bili-browse-next-item
       "g k" #'bili-browse-previous-item
       "g h" #'bili-browse-home
       "g s" #'bili-browse-search
       "g e" #'bili-browse-edit-search
       "g l" #'bili-browse-live
       "g o" #'bili-browse-open-url-command)
      (:map bili-detail-mode-map
       :nm
       "RET" #'bili-detail-activate
       "<return>" #'bili-detail-activate
       "g r" #'bili-detail-refresh
       "g p" #'bili-detail-play
       "g s" #'bili-detail-select-page
       "g o" #'bili-detail-open-in-browser
       "g j" #'bili-detail-next-action
       "g k" #'bili-detail-previous-action))
    (appkit-evil-normalize-buffers bili-evil--application-modes)))

(add-hook 'bili-browse-mode-hook #'appkit-evil-normalize-keymaps)
(add-hook 'bili-detail-mode-hook #'appkit-evil-normalize-keymaps)

(bili-evil-setup)

(with-eval-after-load 'evil
  (bili-evil-setup))

(provide 'bili-evil)

;;; bili-evil.el ends here
