;;;; my font-lock settings
;;; Time-stamp: <03/12/02 19:03:07 jcgs>

(progn
  (require 'font-lock)

  (defconst lisp-font-lock-keywords-3
    (append
     lisp-font-lock-keywords-2
     '(;;
       ;; Highlight control structures
       ("(\\(setq\\|error\\|with-output-to-temp-buffer\\|setf\\|let\\*?\\)[ \t\n]" . 1)
       ;; highlight words inside `' which tend to be function names
       ("`\\([-a-zA-Z0-9_][-a-zA-Z0-9_][-a-zA-Z0-9_.]+\\)'"
	1 font-lock-keyword-face t)
       ))
    "For consideration as a value of `lisp-font-lock-keywords'.
This does even more highlighting.")

  (set-face-foreground 'default "brown")
  (set-background-color "Wheat")


  (set-face-foreground font-lock-comment-face "purple")
  (set-face-foreground font-lock-string-face "navy")
  (set-face-underline-p font-lock-string-face nil)
  (set-face-foreground font-lock-function-name-face "red")
  (set-face-background font-lock-function-name-face "yellow")
  (set-face-foreground font-lock-keyword-face "blue")

  (setq ;; font-lock-support-mode 'lazy-lock-mode
	font-lock-global-modes t
	lazy-lock-minimum-size 10000
	lazy-lock-defer-on-scrolling t
	lazy-lock-stealth-time 60
	font-lock-maximum-size '((c-mode . 500000) (t . 100000))
	lisp-font-lock-keywords lisp-font-lock-keywords-3
	c-font-lock-keywords c-font-lock-keywords-2)

  (global-font-lock-mode 1)
  )
