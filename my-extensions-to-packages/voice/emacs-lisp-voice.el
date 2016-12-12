;;;; emacs-lisp-voice.el -- define voice commands for elisp
;;; Time-stamp: <2006-03-08 09:05:20 jcgs>

(provide 'emacs-lisp-voice)

(defvar vr-emacs-lisp-commands
  '(describe-variable
    describe-function
    eval-defun
    eval-current-buffer
    ("tidy end brackets" . tidy-defun-end-brackets)
    find-function
    ("in other window find function" . find-function-other-window)
    find-variable
    ("in other window find variable" . find-variable-other-window)
    )
  "Voice commands for emacs-lisp")

(put 'emacs-lisp-mode 'voice-commands vr-emacs-lisp-commands)

