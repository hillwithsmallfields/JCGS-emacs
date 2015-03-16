;;;; Write clocked-in tasks into my work log file
;;; Time-stamp: <2015-03-16 21:33:06 jcgs>

(defvar jcgs/org-last-clocked-task-added-to-log nil
  "The last clocked task added to the log.")

(defvar jcgs/org-last-clocked-task-date-added-to-log nil
  "The date of the last clocked task added to the log.")

(defun jcgs/org-add-clocked-task-to-log ()
  "Add to your log the task you're currently clocking in to.
For use in `org-clock-in-hook'."
  (let* ((task (nth 4 (org-heading-components)))
	 (jira (if (string-match "\\[jira:[0-9]+\\]" task)
		   nil
		 (jcgs/org-find-ancestral-jira-task)))
	 (date (format-time-string "%Y-%m-%d")))
    (when (or (not (equal task jcgs/org-last-clocked-task-added-to-log))
	      (not (equal date jcgs/org-last-clocked-task-date-added-to-log)))
      (save-window-excursion
	(find-file work-log-file)
	(goto-char (point-max))
	;; todo: regulate this to one blank line
	(insert (format "\n\n**** Clocked in to \"%s%s\"\n" task
			(if jira
			    (format " (jira %s)" jira)
			  "")))
	(setq jcgs/org-last-clocked-task-added-to-log task
	      jcgs/org-last-clocked-task-date-added-to-log date)
	(basic-save-buffer)))))

(add-hook 'org-clock-in-hook 'jcgs/org-add-clocked-task-to-log) 

(provide 'org-mode-log-tasks)
