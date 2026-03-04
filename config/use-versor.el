;;;; find, load and configure versor
;;; Time-stamp: <2025-11-19 17:27:41 jcgs>

(require 'jcgs-use-package)

(setq joystick-graphical nil)

(when (and (or (file-exists-p "/dev/js0") (file-exists-p "/dev/input/js0"))
	   (or (file-exists-p "/dev/js1") (file-exists-p "/dev/input/js1"))
	   )
  (add-to-list 'load-path (substitute-in-file-name "$MY_PROJECTS/emacs-versor/joylisp/"))
  (when (and (or (file-exists-p "/dev/js0") (file-exists-p "/dev/input/js0"))
	     (or (file-exists-p "/dev/js1") (file-exists-p "/dev/input/js1")))
    (require 'joystick-chord-kbd)
    (joystick-braille-kbd-setup)))

(let ((pedals-dir (substitute-in-file-name "$MY_PROJECTS/emacs-pedals")))
  (when (file-directory-p pedals-dir)
    (add-to-list 'load-path pedals-dir)))

(jcgs/use-package versor
	     "$MY_PROJECTS/emacs-versor/lisp"
	      "https://github.com/hillwithsmallfields/emacs-versor.git" ;; "http://sourceforge.net/project/showfiles.php?group_id=97002"
	     (;; (kill-emacs-hook . versor-save-research-data)
	      "$MY_PROJECTS/emacs-versor/joylisp" ; for joystick
	      t)
	     (versor-setup 'arrows
			   'arrows-misc
			   'keypad
			   'keypad-misc
			   'meta
			   'research
			   'modal
			   ;; 'joystick
			   ;; 'local
			   'menu
			   'quiet-underlying
			   'text-in-code
			   'mouse
			   'tlc)

	     (setq versor-research-report-errors nil)

	     (setq versor-debug-adjust-whitespace t)

	     (set-face-attribute 'versor-item-face nil :underline nil)

	     (global-set-key [ C-y ] 'versor-yank)

	     (when (pedals-p)
	       (add-lispdir "$MY_PROJECTS/emacs-pedals")
	       (setq pedal:versor-change-dimension-ctrl t
		     pedals-hosts-preferring-num-lock '("hosea"))
	       (message "setting up keypad")
	       ;; (require 'keypad)
	       ;; (keypad-setup)
	       (message "adding handsfree")
	       (require 'handsfree)
	       (message "Setting up pedals from use-versor")
	       (pedals-setup)
	       (when (or (string= (downcase (system-name)) "joel.csisdmz.ul.ie")
			 (string= (downcase (system-name)) "glg.csisdmz.ul.ie"))
		 ;; only joel/glg has such a key
		 (message "adding voice-assist-key")
		 (add-lispdir "$MY_ELISP/my-extensions-to-packages/voice")
		 ;; re-assigns one of the pedals, so do after versor etc
		 (require 'voice-assist-key))))

(unless (fboundp 'versor-save-research-data)
  (defun versor-save-research-data ()
    nil))

;;; end of use-versor.el
