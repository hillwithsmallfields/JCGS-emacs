;;;; JIRA links
;;; Time-stamp: <2015-03-16 21:32:23 jcgs>

(defvar jcgs/org-jira-task-format "http://jira.arm.com/browse/EMUF-%s"
  "Format of links for jira tasks.
The task identifier is substituted in as a string.")

(defun jcgs/org-follow-jira-link (task)
  "Show jira TASK in a browser."
  (interactive "sTask: ")
  (browse-url (format jcgs/org-jira-task-format task)))

(org-add-link-type "jira" 'jcgs/org-follow-jira-link)

(defun jcgs/org-find-ancestral-jira-task ()
  "Find the jira task covering the current task."
  (save-excursion
    (let* ((pattern "\\[jira:\\([0-9]+\\)\\]")
	   (heading (nth 4 (org-heading-components))))
      (if (string-match pattern heading)
	  (match-string-no-properties 1 heading)
	(catch 'found
	  (while (> (funcall outline-level) 1)
	    (outline-up-heading 1)
	    (setq heading (nth 4 (org-heading-components)))
	    (when (string-match pattern heading)
	      (throw 'found (match-string-no-properties 1 heading))))
	  nil)))))

(provide 'org-mode-jira)
