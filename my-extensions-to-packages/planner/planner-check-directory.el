;;;; planner-check-directory.el -- check that you're still in the right directory for your current task
;;; Time-stamp: <2007-06-18 11:03:55 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; This is meant to help to remember to keep your "current task" up to
;; date, so at the end of the day, your time will be better accounted
;; for.

(defvar planner-current-task-directories nil
  "Directories used by the current planner task.")

(defun planner-directory-check-task-hook-function (old-status new-status)
  "Note which directory the current task is based in."
  (cond
   ((string= new-status "X")
    ;; close task
    (setq planner-current-task-directories nil))
   ((string= new-status "P")
    ;; suspend task
    (setq planner-current-task-directories nil))
   ((string= new-status "o")
    (let* ((task-info (planner-current-task-info))
	   (description (planner-task-description info))
	   (link (and (string-match "\\[pos://\\(.+\\)#[0-9]+\\]" description)
		      (match-string 1 description))))
      ;; (message "planner-directory-check-task-hook-function %s-->%s for %s" old-status new-status description)
      (when link
	(setq planner-current-task-directories (list (file-name-directory (expand-file-name link))))
	(message "Directories for task: %S" planner-current-task-directories)))))
  (setq planner-ignore-mucking-around nil) ; non-nil if the user acknowledges not being in a task
  t)

(defcustom planner-remind-directory-change nil
  "Whether to remind you if you start working in a different directory."
  :type 'boolean
  :group 'planner)

(defcustom planner-prompt-directory-change nil
  "Whether to prompt you if you start working in a different directory.
If a number, that is the idle time to wait for before asking you."
  :type 'boolean
  :group 'planner)

(defcustom planner-add-directories-automatically t
  "Whether to add the current directory to the list, having mentioned it."
  :type 'boolean
  :group 'planner)

(defvar planner-ignore-mucking-around nil
  "If non-nil, the user has acknowledged that they're not in a task.")

(defvar planner-prompt-already nil
  "Whether we are already prompting.")

(defun planner-work-in-directory ()
  "Tell planner that it's alright to work in this directory."
  (interactive)
  (unless (member default-directory planner-current-task-directories)
    (setq planner-current-task-directories
	  (cons default-directory
		planner-current-task-directories))))

(defun planner-prompt-goto-today ()
  "Ask the user about changing task."
  (cancel-timer planner-directory-check-timer)
  (when (not planner-prompt-already)
    (unwind-protect
	(progn
	  (setq planner-prompt-already t)
	  (if (null planner-timeclock-current-task)
	      (if (y-or-n-p "Not in a task; find one? ")
		  (planner-goto-today)
		(setq planner-ignore-mucking-around t))
	    (if (y-or-n-p "Change task? ")
		(planner-goto-today)
	      (planner-work-in-directory))))
      (setq planner-prompt-already nil))))

(defun planner-remind-directory-change ()
  "Helper function for planner-directory-check-change-hook."
  (cancel-timer planner-directory-check-timer)
  (if (null planner-timeclock-current-task)
      (message "You are not clocked in to any task! Say what you're doing, and I'll give you credit for it.")
    (message (substitute-command-keys "Directory of %s does not match task; may switch task, or \\[planner-work-in-directory] to put this directory into task")
	     (buffer-name))
    (when planner-add-directories-automatically
      (setq planner-current-task-directories
	    (cons default-directory
		  planner-current-task-directories)))))

(defvar planner-directory-check-timer nil
  "Idle-timer for reminding you about directories.")

(defun planner-directory-check-change-hook (&rest ignore)
  "Check that you are editing in the right directory for your current planner task.
If not, remind you to switch planner task."
  (when (or planner-remind-directory-change
	    planner-prompt-directory-change)
    (condition-case evar
	(when (and default-directory
		   (buffer-file-name)	; we are editing a file
		   ;; Just as in real life, fiddling with lists of things
		   ;; to do is an excuse for not doing the things! So
		   ;; allow the user to edit plan files without
		   ;; complaint
		   (not (eq major-mode 'planner-mode))
		   (not planner-prompt-already)
		   (or (and (not planner-ignore-mucking-around)
			    (null planner-timeclock-current-task))
		       (not (member default-directory planner-current-task-directories))))
	  (if (null planner-current-task-directories)
	      (planner-work-in-directory) ; take the first one automatically
	    (when (timerp planner-directory-check-timer)
	      (cancel-timer planner-directory-check-timer))
	    (setq planner-directory-check-timer
		  (run-with-idle-timer
		   (cond
		    ((numberp planner-prompt-directory-change)
		     planner-prompt-directory-change)
		    ((and (not planner-prompt-directory-change)
			  (numberp planner-remind-directory-change))
		     planner-remind-directory-change)
		    (t 2))
		   nil
		   (if planner-prompt-directory-change
		       'planner-prompt-goto-today
		     'planner-remind-directory-change)))))
      (error nil))))

(add-hook 'after-change-functions 'planner-directory-check-change-hook)
(add-hook 'planner-mark-task-hook 'planner-directory-check-task-hook-function)

(provide 'planner-check-directory)

;;; end of planner-check-directory.el
