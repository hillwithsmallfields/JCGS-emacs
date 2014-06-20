;;;; Emacs setup for task management only
;;; Time-stamp: <2014-06-20 15:39:15 johstu01>

(load-file "$EMACS/basics/jcgs-common-setup.el")
(load-file "$EMACS/basics/host.el")

(setq user-emacs-directory (expand-file-name
			    (concat (getenv "EMACS")
				    "/")))

(load-file (substitute-in-file-name "$EMACS/config/config-org-mode.el"))
(load-file (substitute-in-file-name "$EMACS/information-management/work-log.el"))
(message "org-agenda-files is %S" org-agenda-files)
(load-file (substitute-in-file-name "$EMACS/config/config-calendar-diary.el"))

(find-file (substitute-in-file-name "$EMACS/special-setups/tasks/tasks-emacs-setup.el"))

(if (at-work)
    (progn
      (dolist (file '("work-tasks"))
	(add-to-list 'org-agenda-files
		     (substitute-in-file-name (format "~/work-org/%s.org"
						      file))
		     t))
      (setq work-log-file "~/work-org/work-log.org"))
  (dolist (file '("wiring" "switchpanel" "Marmalade-work"))
    (add-to-list 'org-agenda-files
		 (substitute-in-file-name (format "$VEHICLES/Marmalade/%s.org"
						  file))
		 t)))

(find-file work-log-file) (goto-char (point-max))
(mapc (lambda (file)
	;; at work, I don't have all my non-work files readable
	(when (file-readable-p file)
	  (find-file file)))
      org-agenda-files)
(setq org-agenda-files (delete-if-not 'file-exists-p org-agenda-files))
(org-agenda-list)

;;;; tasks-emacs-setup.el ends here
