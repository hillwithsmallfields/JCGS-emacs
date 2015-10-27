;;; work-tasks.el --- org mode variant for handling work tasks

;; Copyright (C) 2014, 2015  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: outlines, convenience

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

;; Initially, this is just to make work-related tasks get archived in
;; a work directory rather than a general one.

;;; Code:

(defvar work-agenda-file (expand-file-name "/work/johstu01/work-org/work-tasks.org")
  "The name of the file containing my work agenda.")

(defvar jcgs/org-reviews-task-heading-text "Reviews/Approvals"
  "The text of the heading for doing reviews.")

(defvar jcgs/org-answering-questions-task "Answering questions, discussing, and advising"
  "The text of the heading for answering questions.")

(defvar jcgs/org-asking-questions-task "Asking questions and seeking advice"
  "The text of the heading for asking questions.")

(defvar jcgs/org-form-filling-task "Filling in forms etc"
  "The text of the heading for bureaucratic tasks.")

(defvar jcgs/org-emacs-task "Emacs"
  "The text of the heading for hacking my Emacs environment.")

(defvar jcgs/org-breaks-and-browsing "Breaks and browsing"
  "The text of the heading for browsing the web or taking a break.")

(defvar jcgs/org-background-reading-task "Background reading"
  "The text of the heading for background reading.")

(defvar jcgs/org-jcgs-task "JCGS own projects"
  "The text of the heading for doing my own projects.")

(defvar jcgs/org-ongoing-activities
  '(jcgs/org-reviews-task-heading-text
    jcgs/org-emacs-task
    jcgs/org-breaks-and-browsing
    jcgs/org-answering-questions-task
    jcgs/org-asking-questions-task
    jcgs/org-background-reading-task
    jcgs/org-form-filling-task)
  "Ongoing tasks other than technical work.")

(define-derived-mode work-tasks-mode org-mode
  "Work tasks"
  "Major mode for handling my work tasks."
  (make-local-variable 'org-archive-location)
  (setq org-archive-location "~/work-org/archive/%s::"))

(add-to-list 'auto-mode-alist
	     (cons "work-org/work-tasks.org" 'work-tasks-mode))

(defvar jcgs/org-last-creative-task nil
  "The last task that was not in `jcgs/org-ongoing-activities'.")

(defun jcgs/org-remember-last-creative-task ()
  "Set `jcgs/org-last-creative-task' as needed.
This is to resume the last task that wasn't in `jcgs/org-ongoing-activities'"
  (unless (member org-clock-current-task (mapcar 'eval jcgs/org-ongoing-activities))
    (setq jcgs/org-last-creative-task org-clock-current-task)))

(defun jcgs/org-show-last-creative-task ()
  "Show the value of 'jcgs/org-last-creative-task'."
  (interactive)
  (message (substitute-command-keys
	    "The creative task that \\[jcgs/org-resume-creative] would resume is %S")
	   jcgs/org-last-creative-task))

(defun jcgs/org-clock-in-specific (task-heading)
  "Clock in to TASK-HEADING."
  (save-window-excursion
    (when (org-clocking-p)
      (org-clock-out))
    (find-file work-agenda-file)
    (goto-char (org-find-exact-headline-in-buffer task-heading
						  nil t))
    ;; possible enhancement: bind org-timer-default-timer while doing this, so that the "Break and browsing" task can be just 5 minutes
    (org-clock-in)))

(defun jcgs/org-clock-in-or-out ()
  "Clock in (if out) or out (if in)."
  (interactive)
  (if (org-clocking-p)
      (org-clock-out)
    (jcgs/org-resume-creative)))

(defun jcgs/org-start-reviewing ()
  "Switch to the activity of reviewing colleagues' code."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-reviews-task-heading-text))

(defun jcgs/org-start-answering ()
  "Switch to the activity of answering questions."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-answering-questions-task))

(defun jcgs/org-start-asking ()
  "Switch to the activity of asking questions."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-asking-questions-task))

(defun jcgs/org-start-emacs ()
  "Switch to the activity of hacking Emacs."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-emacs-task))

(defun jcgs/org-start-break-or-browsing ()
  "Switch to the activity of taking a break, particularly browsing the web."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-breaks-and-browsing))

(defun jcgs/org-start-background-reading ()
  "Switch to the activity of reading background information (understanding)."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-background-reading-task))

(defun jcgs/org-start-paperwork ()
  "Switch to the activity of filling in forms (paperwork)."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-form-filling-task))

(defun jcgs/org-jcgs-task ()
  "Switch to the activity of doing one of my own projects (JCGS)."
  (jcgs/org-clock-in-specific jcgs/org-jcgs-task))

(defun jcgs/org-resume-creative ()
  "Resume my last creative task."
  (interactive)
  (jcgs/org-clock-in-specific jcgs/org-last-creative-task))

(defun jcgs/org-help-task-keys ()
  "Display help on my task keys."
  (interactive)
  (if (and (boundp 'jcgs-task-tracking-map)
	   (keymapp 'jcgs-task-tracking-map))
      (with-output-to-temp-buffer "*Task key bindings*"
	(let ((top-levels (where-is-internal 'jcgs-task-tracking-map)))
	  (if (= (length top-levels) 1)
	      (princ (format "These commands are prefixed by %S:\n\n" (car top-levels)))
	    (princ (format "These commands are prefixed by any of the following keys: %s:\n\n" (mapconcat 'identity top-levels ", ")))))
	(let ((pairs nil))
	  (map-keymap (lambda (binding definition)
			(push (cons binding
				    (let* ((first-line (car (split-string (documentation definition) "\n"))))
				      (if (string-match "Switch to the activity of \\(.+\\)" first-line)
					  (match-string 1 first-line)
					first-line)))
			      pairs)
			)
		      jcgs-task-tracking-map)
	  (dolist (pair (sort pairs 'car-less-than-car))
	    (let ((binding (car pair))
		  (description (cdr pair)))
	      (princ (format "%c\t%s\n" binding description))))))
    (error "Task keymap not set up")))

(add-hook 'org-clock-out-hook 'jcgs/org-remember-last-creative-task)

(setq org-clock-out-switch-to-state "OPEN"
      org-clock-in-switch-to-state "CURRENT")

(provide 'work-tasks)
;;; work-tasks.el ends here
