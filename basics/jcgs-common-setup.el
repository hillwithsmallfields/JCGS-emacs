;;;; JCGS's common emacs setup
;;; Time-stamp: <2014-06-17 11:10:23 johstu01>
;;;
;;; Things I want even in most of my specialized emacsen

(defun at-work ()
  "Return whether I'm on a work machine."
  (string-match "arm.com"
		(system-name)))

(global-font-lock-mode 1)
(setq inhibit-startup-screen t
      kill-whole-line t
      display-time-day-and-date t
      user-emacs-directory (if (getenv "DROPBOX")
			       (substitute-in-file-name "$DROPBOX/emacs")
			     (substitute-in-file-name "$HOME/Dropbox/emacs")))
(display-time)
(when (member (system-name)
	      '("ezra"))
  ;; conditional because it hit a bug on my work desktop at ARM, so I've made it laptop-only
  (display-battery-mode))
(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

(defun save-all-buffers-no-ask ()
  "Save all buffers, without prompting for each one."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers"))

(global-set-key [ f11 ] 'save-all-buffers-no-ask)

;;; jcgs-common-setup.el ends here
