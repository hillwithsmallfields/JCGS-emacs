;;; Time-stamp: <2013-10-15 12:02:47 johnstu>

(setq stack-trace-on-error t
      message-log-max t)
(unless (getenv "COMMON")
  (setenv "COMMON" "i:/common"))
(unless (getenv "GATHERED")
  (setenv "GATHERED" "j:/users/jcgs/library"))

(message "COMMON=%S GATHERED=%S" (getenv "COMMON") (getenv "GATHERED"))

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/lisp"))
(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs/html-helper-mode/"))

(add-to-list 'auto-mode-alist (cons "\\.html$" 'html-helper-mode))

(autoload 'html-helper-mode "html-helper-mode"
  "Mode for editing HTML documents." t)

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


(require 'time-stamp)
(time-stamp-toggle-active 1)
(add-hook 'write-file-hooks 'time-stamp)

(setq display-time-day-and-date t)
(display-time)
