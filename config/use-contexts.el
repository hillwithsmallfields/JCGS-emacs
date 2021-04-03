;;;; use-contexts.el -- find, load and config for contexts
;;; Time-stamp: <2020-11-11 20:56:08 jcgs>

(defvar context-keymap (make-keymap) "Keys for context stuff.")
(fset 'context-keymap context-keymap)
(suppress-keymap context-keymap)
(define-key context-keymap "l" 'context-load)
(if (null (key-binding "\e&")) (define-key esc-map "&" context-keymap))

(jcgs/use-package contexts
	     (expand-file-name "persistence" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/persistence/contexts.el"
	     ((expand-file-name "misc" user-emacs-directory)
	      (context-load "contexts")))

;;; end of use-contexts.el
