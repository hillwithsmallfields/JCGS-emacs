;;;; dired-voice.el
;;; Time-stamp: <03/01/15 13:18:23 jcgs>

(provide 'dired-voice)

(defvar vr-dired-commands
  '(("dired" . dired-default-directory)
    )
  "Voice commands for dired.")

(defun dired-default-directory ()
  "Run dired on the default directory of the current buffer."
  (interactive)
  (dired default-directory))
