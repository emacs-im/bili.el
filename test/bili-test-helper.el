;;; bili-test-helper.el --- Runtime helpers for bili.el tests  -*- lexical-binding: t; -*-

(require 'appkit-app)
(require 'appkit-loop)
(require 'appkit-surface)
(require 'bili-core)

(defun bili-test-drain (surface &optional pass-limit)
  "Drain synchronous App and SURFACE work within PASS-LIMIT passes."
  (let* ((app (appkit-surface-app surface))
         (app-loop (appkit-app-loop app))
         (surface-loop (appkit-surface-loop surface))
         (remaining (or pass-limit 16))
         first-pass)
    (while (and (> remaining 0)
                (or (not first-pass)
                    (> (appkit-loop-pending-count app-loop) 0)
                    (> (appkit-loop-pending-count surface-loop) 0)))
      (setq first-pass t
            remaining (1- remaining))
      (when (appkit-app-live-p app)
        (appkit-loop-run-pass app-loop))
      (when (appkit-surface-live-p surface)
        (appkit-loop-run-pass surface-loop))))
  surface)

(defun bili-test-stop-surface (surface)
  "Stop SURFACE and kill its host buffer."
  (when (appkit-surface-p surface)
    (let ((buffer (appkit-surface-buffer surface)))
      (when (appkit-surface-live-p surface)
        (appkit-surface-stop surface))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'bili-test-helper)

;;; bili-test-helper.el ends here
