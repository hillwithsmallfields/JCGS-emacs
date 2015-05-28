;;;; JIRA links
;;; Time-stamp: <2015-05-28 10:51:14 johstu01>

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

(defun jcgs/org-open-task-with-jira ()
  "If the task is now open, bring up its JIRA page in the browser.
Intended for use on `org-after-todo-state-change-hook'."
  (when (string= (org-get-todo-state) "OPEN")
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (when (re-search-forward "\\[jira:\\([0-9]+\\)\\]" (org-entry-end-position) t)
	(jcgs/org-follow-jira-link (match-string-no-properties 1))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-open-task-with-jira)

(provide 'org-mode-jira)
