;;;; linked tasks in org-mode
;;; Time-stamp: <2015-03-16 21:26:24 jcgs>

(defun jcgs/org-clock-in-prepare-function ()
  "My customization of task clock-in."
  (save-excursion
    (while (> (funcall outline-level) 1)
      (outline-up-heading 1)
      (when (looking-at org-complex-heading-regexp)
	(let ((state (match-string-no-properties 2)))
	  (when (equal state "TODO")
	    (org-todo "OPEN")))))))

(add-hook 'org-clock-in-prepare-hook 'jcgs/org-clock-in-prepare-function)

(defun jcgs/org-after-todo-state-change-propagate-upwards ()
  "When the last of a set of sibling tasks is marked as DONE,
mark the ancestral tasks as DONE."
  (while (> (funcall outline-level) 1)
    (outline-up-heading 1)
    (let ((not-all-done nil)
	  (on-first t))
      (org-map-entries
       (lambda ()
	 (when (and (not on-first)
		    (org-entry-is-todo-p))
	   (setq not-all-done t))
	 (setq on-first nil))
       nil
       'tree)
      (unless not-all-done
	(org-todo "DONE")))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-propagate-upwards t)

(defun jcgs/org-after-todo-state-change-move-next-marker ()
  "If this task is being marked as done, and has a :next: tag, move the tag.
Propagate :urgent: and :soon: tags as needed."
  (let ((original-tags (org-get-tags)))
    (when (and (org-entry-is-done-p)
	       (member "next" original-tags))
      (let ((is-urgent (member "urgent" original-tags))
	    (is-soon (member "soon" original-tags)))
	(org-toggle-tag "next" 'off)
	(beginning-of-line 1)
	(let ((started-at (point)))
	  (org-forward-heading-same-level 1)
	  (if (/= (point) started-at)
	      (progn
		(org-toggle-tag "next" 'on)
		(when is-urgent (org-toggle-tag "urgent" 'on))
		(when is-soon (org-toggle-tag "soon" 'on)))
	    (when (y-or-n-p "Move :next: marker to next subtree? ")
	      (outline-next-heading)
	      (org-toggle-tag "next" 'on)
	      (when is-urgent (org-toggle-tag "urgent" 'on))
	      (when is-soon (org-toggle-tag "soon" 'on)))))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-move-next-marker)

(provide 'org-mode-linked-tasks)
