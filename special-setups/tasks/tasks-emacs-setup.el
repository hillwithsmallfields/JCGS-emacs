;;;; Emacs setup for task management and noticeboard only
;;; Time-stamp: <2019-07-24 23:00:12 jcgs>

(setq debug-on-error t)

(setq weather-loadable nil)

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

(load-file (expand-file-name "basics/use-package.el" user-emacs-directory))
(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-pedals"))
(unless (and (boundp 'no-versor) no-versor)
  (load-file (expand-file-name "config/use-versor.el" user-emacs-directory)))

(find-file (expand-file-name "special-setups/tasks/tasks-emacs-setup.el" user-emacs-directory))

(unless (boundp 'jcgs/org-journal-files)
  (setq jcgs/org-journal-files nil))

(when (at-home-p)
  (dolist (file '("wiring" "switchpanel" "Marmalade-work"))
    (add-to-list 'org-agenda-files
		 (substitute-in-file-name (format "$VEHICLES/Marmalade/%s.org"
						  file))
		 t))
  (setq org-reading-files (cons (substitute-in-file-name "$ORG/guide.org")
				(let ((reading-dir (substitute-in-file-name
						    "$COMMON/noticeboard-reading")))
				  (mapcar (lambda (file)
					    (expand-file-name file reading-dir))
					  '("advices-and-queries.org")))))
  (let ((incoming-journal "~/common/journal/incoming.journal"))
    (when (file-exists-p incoming-journal)
      (push incoming-journal jcgs/org-journal-files))))

(let ((hacking-journal "~/common/journal/hacking.journal"))
    (when (file-exists-p hacking-journal)
      (push hacking-journal jcgs/org-journal-files)))

(when (boundp 'jcgs/org-journal-files)
  (mapcar 'find-file jcgs/org-journal-files))

(setq org-agenda-files (delete-if-not 'file-exists-p org-agenda-files))
(mapc (lambda (file)
	(when (file-readable-p file)
	  (find-file file)))
      org-agenda-files)
(org-mobile-pull)
;; (org-agenda-list)
(org-agenda nil "c")

(defun jcgs/emacs-pre-shutdown-function ()
  "Function to run soon before shutdown."
  (when (fboundp 'jcgs/org-revert-agenda-files)
    (jcgs/org-revert-agenda-files))
  (save-all-buffers-no-ask)
  (org-mobile-push))

(let ((shutdown-file "/tmp/shutdowntime"))
  (when (file-readable-p shutdown-file)
    (find-file shutdown-file)
    (let* ((time-from-file (diary-entry-time (buffer-string)))
	   (now (decode-time))
	   (before-string (format-time-string 
			   "%H%M"
			   (time-subtract
			    (encode-time 0 (% time-from-file 100) (/ time-from-file 100)
					 (nth 3 now) (nth 4 now) (nth 5 now)
					 (nth 8 now))
			    (seconds-to-time 120)))))
      (kill-buffer)
      (message "Will prepare for shutdown at %s" before-string)
      (run-at-time before-string nil 'jcgs/emacs-pre-shutdown-function)
      )))

(add-hook 'kill-emacs-hook 'jcgs/emacs-pre-shutdown-function)

(setq debug-on-error nil)

;;;; tasks-emacs-setup.el ends here
