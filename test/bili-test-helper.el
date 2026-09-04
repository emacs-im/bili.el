;;; bili-test-helper.el --- Runtime helpers for bili.el tests  -*- lexical-binding: t; -*-

(require 'appkit-app)
(require 'appkit-loop)
(require 'appkit-surface)
(require 'bili-core)

(defun bili-test-drain (surface &optional passes)
  "Run enough App and SURFACE passes to settle synchronous test Effects."
  (dotimes (_ (or passes 4))
    (when (appkit-app-live-p (appkit-surface-app surface))
      (appkit-loop-run-pass
       (appkit-app-loop (appkit-surface-app surface))))
    (when (appkit-surface-live-p surface)
      (appkit-loop-run-pass (appkit-surface-loop surface))))
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
