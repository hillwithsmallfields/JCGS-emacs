;;; Time-stamp: <2006-07-07 14:49:04 john>

(setq stack-trace-on-error t
      message-log-max t)
(unless (getenv "COMMON")
  (setenv "COMMON" "i:/common"))

(add-to-list 'load-path (substitute-in-file-name "$COMMON/open-projects/emacs-versor/lisp"))

(global-font-lock-mode)

(require 'versor)
(require 'versor-demo)

(versor-setup
 'tracking
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

(setq recorded-demo-html-directory (substitute-in-file-name "$COMMON/open-projects/emacs-versor/htdocs/new-demo"))

(defun take-screenshots ()
  (interactive)
  (set-screen-height 24)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (versor-record-screenshots))

(find-file "~/common/open-projects/emacs-versor/lisp/versor-commands.el")
(goto-char 1727)
(describe-variable 'post-command-hook)
