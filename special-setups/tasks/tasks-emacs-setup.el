;;;; Emacs setup for task management only
;;; Time-stamp: <2014-09-28 19:38:09 jcgs>

(load-file "$EMACS/basics/jcgs-common-setup.el")
(load-file "$EMACS/basics/host.el")
(load-file "$EMACS/basics/ediff-fix.el")
(require 'cl)

(setq user-emacs-directory (expand-file-name
			    (concat (getenv "EMACS")
				    "/")))

(load-file (expand-file-name "config/config-org-mode.el" user-emacs-directory))
(message "org-agenda-files is %S" org-agenda-files)
(load-file (expand-file-name "config/config-calendar-diary.el" user-emacs-directory))

(find-file (expand-file-name "special-setups/tasks/tasks-emacs-setup.el" user-emacs-directory))

(if (at-work)
    (progn
      (dolist (file '("work-tasks"))
	(add-to-list 'org-agenda-files
		     (substitute-in-file-name (format "/work/johstu01/work-org/%s.org"
						      file))
		     t))
      (setq work-log-file "/work/johstu01/work-org/work-log.org"))
  (dolist (file '("wiring" "switchpanel" "Marmalade-work"))
    (add-to-list 'org-agenda-files
		 (substitute-in-file-name (format "$VEHICLES/Marmalade/%s.org"
						  file))
		 t)))

(find-file work-log-file)
(if (fboundp 'work-log-mode)
    (work-log-mode)
  (org-mode))

(mapc (lambda (file)
	;; at work, I don't have all my non-work files readable
	(when (file-readable-p file)
	  (find-file file)))
      org-agenda-files)
(setq org-agenda-files (delete-if-not 'file-exists-p org-agenda-files))
(org-agenda-list)

;;;; tasks-emacs-setup.el ends here
