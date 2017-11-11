;;;; JCGS's common emacs setup
;;; Time-stamp: <2017-11-11 18:43:42 jcgs>
;;;
;;; Things I want even in most of my specialized emacsen

(global-font-lock-mode 1)
(setq inhibit-startup-screen t
      kill-whole-line t
      display-time-day-and-date t
      parens-require-spaces nil
      message-log-max t
      eval-expression-print-length nil
      eval-expression-print-level nil
      delete-old-versions t)

(menu-bar-mode -1)
(tool-bar-mode -1)

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

(dolist (dir '("config" "editing"))
  (add-to-list 'load-path
	       (expand-file-name dir user-emacs-directory)))

(load-library "jcgs-bindings")

(defun enable-debug (arg)
  "Switch debugging on.
With ARG negative (or provided interactively) switch it off."
  (interactive "P")
  (message "arg is %S" arg)
  (if (or (and (numberp arg)
	       (< arg 0))
	  (and arg (not (numberp arg))))
      (progn
	(message "disabling debug")
	(setq debug-on-error nil))
    (message "enabling debug")
    (setq debug-on-error t)))

;;; jcgs-common-setup.el ends here
