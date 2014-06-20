;;;; just-platypus-setup.el -- setup for running platypus in an emacs of its own
;;; Time-stamp: <2013-10-15 12:02:47 johnstu>

(unless (getenv "COMMON")
  (setenv "COMMON" "i:/common"))

(setq stack-trace-on-error t
      message-log-max t)

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/lisp"))

(global-font-lock-mode)

(require 'versor)

(versor-setup
 'meta
 'arrows
 'arrows-misc
 'modal
 'text-in-code
 )

(setq versor-mode-current-levels
      (mapcar 'versor-mode-levels-triplet
	      '((emacs-lisp-mode "structural" "exprs")
		("emacs-lisp-mode" "text" "words")
		(lisp-interaction-mode "structural" "exprs")
		(c-mode "program" "statement-parts")
		("c-mode" "text" "words")
		(text-mode "cartesian" "lines")
		;; (html-helper-mode "text" "words")
		;; (latex-mode "text" "words")
		(html-helper-mode "structural" "exprs")
		(latex-mode "structural" "exprs")
		)))

(mapcar 'find-file (directory-files (substitute-in-file-name "$COMMON/research/proglang/platypus06/")
				    t "\\.el$"))

(load-file (substitute-in-file-name "$COMMON/research/proglang/platypus06/platypus.el"))

;;; end of just-platypus-setup.el
