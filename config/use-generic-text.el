;;;; use-generic-text.el -- find and load my generic-text system
;;; Time-stamp: <2013-10-15 12:22:13 johnstu>

(use-package 'generic-text
	     (expand-file-name "editing" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/editing/generic-text.tar.gz"
	     ((expand-file-name "my-extensions-to-packages/emacspeak" user-emacs-directory)))

;;; end of use-generic-text.el
