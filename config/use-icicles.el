;;;; use-icicles.el -- find and use icicles
;;; Time-stamp: <2022-03-15 10:42:11 jcgs>

;; (require 'jcgs-use-package)

;; (jcgs/use-package icicles
;; 	     "$GATHERED/emacs/icicles/"
;; 	     "http://www.emacswiki.org/cgi-bin/wiki/Icicles" ; todo: find the real download place
;; 	     ;; todo: also needs ~/library/emacs/doremi/ring+.el
;; 	     ((icicle-execute-extended-command
;; 	       "icicles-cmd.el"
;; 	       "Read command name, then read its arguments and call it.
;; This is `execute-extended-command', turned into a multi-command."
;; 	       t)
;; 	      (expand-file-name "hardware" user-emacs-directory)
;; 	      "$GATHERED/emacs/doremi"
;; 	      (bind-joystick-icicles-minibuffer-keys "joystick-icicles.el"
;; 						     "Make the minibuffer key-bindings needed to use icicles via the joystick."
;; 						     t)
;; ;;	      ([ M-x ] . icicle-execute-extended-command)
;; ;;	      ([ ?\e ?x ] . icicle-execute-extended-command)
;; ;;	      ([ ?\C-x \e \e ] . icicle-repeat-complex-command)
;; 	      )

;; 	     ;; put icicles binding onto the keypad, because Fedora
;; 	     ;; won't read the ordinary arrow keys on my home machine
;; 	     ;; (Comfort Keyboard from around 2001)
;; 	     (let ((bind-c-k (lambda (this-map)
;; 			       (define-key this-map [ kp-up ] 'icicle-previous-prefix-candidate)
;; 			       (define-key this-map [ kp-down ] 'icicle-next-prefix-candidate)
;; 			       (define-key this-map [ kp-next ] 'icicle-previous-apropos-candidate)
;; 			       (define-key this-map [ kp-prior ] 'icicle-next-apropos-candidate)

;; 			       (define-key this-map [ C-kp-up ] 'icicle-previous-prefix-candidate-action)
;; 			       (define-key this-map [ C-kp-down ] 'icicle-next-prefix-candidate-action)
;; 			       (define-key this-map [ C-kp-next ] 'icicle-previous-apropos-candidate-action)
;; 			       (define-key this-map [ C-kp-prior ] 'icicle-next-apropos-candidate-action))))

;; 	       (funcall bind-c-k minibuffer-local-completion-map)

;; 	       ;; `minibuffer-local-must-match-map` ': must-match map.
;; 	       ;; In Emacs 22+, local-completion is parent of local-must-match
;; 	       (unless (eq minibuffer-local-completion-map
;; 			   (keymap-parent minibuffer-local-must-match-map))
;; 		 (funcall bind-c-k minibuffer-local-must-match-map))

;; 	       )

;; 	     (push 'S-kp-divide doremi-other-up-events)
;; 	     (push 'kp-divide doremi-other-down-events)
;; 	     (push 'S-kp-1 doremi-other-boost-up-events)
;; 	     (push 'kp-end doremi-other-boost-down-events)

;; 	     (push 'Hat0Y-previous doremi-other-up-events)
;; 	     (push 'Hat0Y-next doremi-other-down-events)
;; 	     (push 'Hat0X-previous doremi-other-boost-up-events)
;; 	     (push 'Hat0X-next doremi-other-boost-down-events)

;; 	     (bind-joystick-icicles-minibuffer-keys ))

;;; end of use-icicles.el
