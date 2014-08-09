;;;; JCGS's common emacs setup
;;; Time-stamp: <2014-08-09 11:51:51 jcgs>
;;;
;;; Things I want even in most of my specialized emacsen

(global-font-lock-mode 1)
(setq inhibit-startup-screen t
      kill-whole-line t
      display-time-day-and-date t
      parens-require-spaces nil)

(unless (and (stringp user-emacs-directory)
	     (file-directory-p user-emacs-directory)
	     (file-directory-p
	      (expand-file-name "basics" user-emacs-directory)))
  (setq user-emacs-directory
	(catch 'found
	  (dolist (raw-dir '("$HOME/JCGS-emacs" "/work/johstu01/JCGS-emacs"))
	    (let ((dir (substitute-in-file-name raw-dir)))
	      (when (and (stringp dir)
			 (file-directory-p dir)
			 (file-directory-p
			  (expand-file-name "basics" dir)))
		(throw 'found dir)))))))

(display-time)
(when (member (system-name)
	      '("ezra"))
  ;; conditional because it hit a bug on my work desktop at ARM, so I've made it laptop-only
  (display-battery-mode))
(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

(when (string-match "isaiah" (system-name))
  (setq printer-name "JCGS_print_scan_1"))

(defun save-all-buffers-no-ask ()
  "Save all buffers, without prompting for each one."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers"))

(global-set-key [ f11 ] 'save-all-buffers-no-ask)

;;; jcgs-common-setup.el ends here
