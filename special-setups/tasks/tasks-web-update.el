;;;; Emacs setup for task management web pages update only
;;; Time-stamp: <2016-03-07 17:47:36 johstu01>

(setq debug-on-error t)

(load-file "$EMACS/basics/jcgs-common-setup.el")
(load-file "$EMACS/basics/host.el")
(require 'cl)

(setq user-emacs-directory (expand-file-name
			    (concat (getenv "EMACS")
				    "/")))

(load-file (expand-file-name "config/config-org-mode.el" user-emacs-directory))
(message "org-agenda-files is %S" org-agenda-files)
(load-file (expand-file-name "config/config-calendar-diary.el" user-emacs-directory))

(load-file (expand-file-name "basics/use-package.el" user-emacs-directory))

(mapc (lambda (file)
	;; at work, I don't have all my non-work files readable
	(when (file-readable-p file)
	  (find-file file)))
      org-agenda-files)
(setq org-agenda-files (delete-if-not 'file-exists-p org-agenda-files))

(jcgs/org-agenda-monitor-update nil)

(remove-hook 'kill-emacs-query-functions 'jcgs/org-maybe-push-to-mobile)

(save-buffers-kill-emacs t)

;;;; tasks-emacs-setup.el ends here
