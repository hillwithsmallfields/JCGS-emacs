;;; Time-stamp: <2013-10-15 12:02:47 johnstu>

(setq stack-trace-on-error t
      message-log-max t)
(unless (getenv "COMMON")
  (setenv "COMMON" "i:/common"))
(unless (getenv "GATHERED")
  (setenv "GATHERED" "j:/users/jcgs/library"))

(message "COMMON=%S GATHERED=%S" (getenv "COMMON") (getenv "GATHERED"))

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/lisp"))

(global-font-lock-mode)

(require 'versor)
(require 'versor-demo)

(versor-setup
 'meta
 'arrows
 'arrows-misc
 'keypad
 'keypad-misc
 'menu
 'quiet-underlying
 'mouse
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

(setq recorded-demo-html-directory (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/htdocs/new-demo"))

(defun take-screenshots ()
  (interactive)
  (set-screen-height 24)
  (set-screen-width 96)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (versor-record-screenshots))

(find-file (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/lisp/versor-commands.el"))
(goto-char 1727)
(describe-variable 'post-command-hook)

(find-file (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/lisp/versor-tlc.el"))
