;;;; use-voice-input.el -- find and configure voice input
;;; Time-stamp: <2013-10-15 12:22:11 johnstu>

(use-package 'vr-mode
	     (expand-file-name "voice" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/voice/vr-mode.el"
	     ("$GATHERED/emacs/voice/vrmode/"
	      (expand-file-name "my-extensions-to-packages/voice" user-emacs-directory)
	      (expand-file-name "editing" user-emacs-directory)
	      (expand-file-name "handsfree" user-emacs-directory)
(vr-mode "vr-mode"
		       "Toggle VR mode.  With argument ARG, turn VR mode on iff ARG is positive."
		       t)
	      (jcgs-vr-mode-setup "jcgs-vr-mode-setup")
	      (vr-mode-setup-hook . jcgs-vr-mode-setup)
	      )
	     )

;;; end of use-voice-input.el
