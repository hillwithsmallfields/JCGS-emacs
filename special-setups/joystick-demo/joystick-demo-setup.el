;;; joystick-demo-setup.el --- configuration for demoing versor with gamepad
;; Time-stamp: <2021-11-14 18:34:19 jcgs>

(setq stack-trace-on-error t
      inhibit-splash-screen t
      message-log-max t)
(unless (getenv "SYNCED")
  (setenv "COMMON" "~/common"))
(unless (getenv "GATHERED")
  (setenv "GATHERED" "~/library"))

(message "COMMON=%S GATHERED=%S" (getenv "SYNCED") (getenv "GATHERED"))

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/lisp"))
(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/joylisp/"))

(global-font-lock-mode)

(require 'versor)

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
 'joystick
 )

(setq joystick-graphical t)

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

(dolist (file '((expand-file-name "special-setups/joystick-demo/joystick-demo-setup.el" user-emacs-directory)
		"$OPEN_PROJECTS/emacs-versor/joylisp/joylisp.c"
		"$OPEN_PROJECTS/emacs-versor/lisp/versor-joystick.el"
		"$OPEN_PROJECTS/emacs-versor/lisp/versor-commands.el"
		"$OPEN_PROJECTS/emacs-versor/joylisp/joystick.el"))
  (find-file-other-window (substitute-in-file-name file))
  (skip-to-actual-code))

;; joystick-demo-setup.el ends here
