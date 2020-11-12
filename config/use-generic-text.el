;;;; use-generic-text.el -- find and load my generic-text system
;;; Time-stamp: <2020-11-11 20:56:08 jcgs>

(require 'jcgs-use-package)

(jcgs/use-package 'generic-text
	     (expand-file-name "editing" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/editing/generic-text.tar.gz"
	     ((expand-file-name "my-extensions-to-packages/emacspeak" user-emacs-directory)))

;;; end of use-generic-text.el
