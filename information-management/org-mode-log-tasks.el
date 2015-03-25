;;;; Write clocked-in tasks into my work log file
;;; Time-stamp: <2015-03-25 21:10:44 jcgs>

;; Copyright (C) 2015  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This links an org file of things to do, with a time-structured org
;; file for journalling what you're doing, by putting clocking-in
;; marks into your journalling file, so that your journal
;; automatically includes your tasks.  Also includes a JIRA link if it
;; spots one.

;;; Code:

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
