;;;; Emacs setup for task management only

(global-font-lock-mode 1)
(setq inhibit-startup-screen t
      kill-whole-line t
      display-time-day-and-date t)
(display-time)
(display-battery-mode)
(require 'time-stamp)

(setq user-emacs-directory (expand-file-name
			    (concat (getenv "EMACS")
				    "/")))

(add-to-list 'load-path (expand-file-name "emacs/html-helper-mode" (getenv "GATHERED")))
(load-library "html-helper-mode")
(add-to-list 'load-path (expand-file-name "webstuff" user-emacs-directory))
(load-library "journal-from-text")
(load-file "/mnt/crypted/personal/elisp/journal-bio.el")

(defun save-all-buffers-no-ask ()
  "Save all buffers, without prompting for each one."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers"))

(global-set-key [ f11 ] 'save-all-buffers-no-ask)

;;;; tasks-emacs-setup.el ends here
