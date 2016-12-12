;;;; use-contexts.el -- find, load and config for contexts
;;; Time-stamp: <2013-10-15 12:22:11 johnstu>

(defvar context-keymap (make-keymap) "Keys for context stuff.")
(fset 'context-keymap context-keymap)
(suppress-keymap context-keymap)
(define-key context-keymap "l" 'context-load)
(if (null (key-binding "\e&")) (define-key esc-map "&" context-keymap))

(use-package contexts
	     (expand-file-name "persistence" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/persistence/contexts.el"
	     ((expand-file-name "misc" user-emacs-directory)
	      (context-load "contexts")))

;;; end of use-contexts.el
