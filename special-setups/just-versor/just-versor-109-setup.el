;;; Time-stamp: <2021-11-14 18:34:44 jcgs>

(setq stack-trace-on-error t
      message-log-max t)
(unless (getenv "SYNCED")
  (setenv "COMMON" "i:/common"))

(add-to-list 'load-path (substitute-in-file-name "~/versor-versions/emacs-versor_1.09/lisp/"))

(global-font-lock-mode)

(require 'versor)
(require 'versor-demo)

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

(find-file "~/common/open-projects/emacs-versor/lisp/versor-commands.el")
(goto-char 1727)
(describe-variable 'post-command-hook)
