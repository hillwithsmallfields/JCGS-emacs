;;;; Pomodoros
;;; Time-stamp: <2015-04-08 11:25:41 johstu01>

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

;;

;;; Code:

(defun jcgs/org-timer-setup ()
  "Customizer the org timer to suit me, for pomodoro use."
  (add-hook 'org-clock-in-hook
	    (function
	     (lambda ()
	       (unless org-timer-current-timer
		 (org-timer-set-timer '(16)))
	       (org-todo "CURRENT"))))
  (add-hook 'org-clock-out-hook
	    (function
	     (lambda ()
	       (when org-timer-current-timer
		 (org-timer-cancel-timer))
	       (org-timer-stop)
	       (org-todo "OPEN")))))

(defvar jcgs/org-timer-pomodoros-done-count 0
  "Count of the pomodoros I have done.
Reset to zero whenever `jcgs/org-timer-log-pomodoro-done' decides to log
the end of a day.")

(defvar jcgs/pomodoro-log-file
  (cond
   ((file-directory-p "/work/johstu01/work-org")
    (expand-file-name "pomodoro-log.org" "/work/johstu01/work-org/"))
   ((getenv "ORG")
    (substitute-in-file-name "$ORG/pomodoro-log.org"))
   ((file-directory-p "~/Dropbox")
    (expand-file-name "~/Dropbox/pomodoro-log.org"))
   (t (expand-file-name "~/pomodoro-log.org")))
  "Where I log my pomodoro completion.")

(defun jcgs/pomodoro-log-show ()
  "Show my pomodoro log."
  (interactive)
  (find-file-other-window jcgs/pomodoro-log-file)
  (goto-char (point-max )))

(defun jcgs/org-timer-last-pomodoro-completion-day-in-file (file)
  "Find the last completion day in FILE."
  (if (file-readable-p file)
      (save-window-excursion
	(save-excursion
	  (find-file file)
	  (save-excursion
	    (goto-char (point-max))
	    (if (re-search-backward "\\*\\*\\* Date \\([_0-9]+\\)" (point-min) t)
		(match-string-no-properties 1)
	      nil))))
    nil))

(defvar jcgs/org-timer-last-pomodoro-completion-day
  (jcgs/org-timer-last-pomodoro-completion-day-in-file
   jcgs/pomodoro-log-file)
  "The day I last completed a pomodoro.
Used to reset the counter daily.")

(defvar jcgs/org-timer-pomodoros-done-log nil
  "Log of the pomodoros I have done.")

(defun jcgs/date-string (&optional time)
  "Decode date into a format I use.
Optional argument TIME is passed on."
  (format-time-string "%Y_%m_%d"))

(defvar jcgs/org-strip-timer-stuff-regexp
  "\\(.+\\)\\(: time out\\)"
  "Regexp to identify what part of a string from the timer system to keep.")

(defun jcgs/org-strip-timer-stuff (string)
  "Remove timing system related text from STRING."
  (if (string-match jcgs/org-strip-timer-stuff-regexp
		    string)
      (match-string 1 string)
    string))

(defun jcgs/org-timer-log-pomodoro-done (string)
  "Log that I have completed a timed activity slot.
Argument STRING is the log entry."
  (let* ((now (current-time))
	 (day (jcgs/date-string now))
	 (pomodoro-string (jcgs/org-strip-timer-stuff string)))
    (save-window-excursion
      (save-excursion
	(find-file jcgs/pomodoro-log-file)
	(goto-char (point-max))
	(message "Recording pomodoro: day now %S, last completion day %S" day jcgs/org-timer-last-pomodoro-completion-day)
	(when (not (equal day jcgs/org-timer-last-pomodoro-completion-day))
	  (message "%d pomodoros in previous day"
		   jcgs/org-timer-pomodoros-done-count)
	  (save-excursion
	    ;; this should have been inserted when a pomodoro was done
	    ;; on the previous day when any pomodoros were done:
	    (when (search-backward
		   (format "*** Date %s"
			   jcgs/org-timer-last-pomodoro-completion-day)
		   (point-min) t)
	      (end-of-line)
	      (insert (format " (%d pomodoros done)"
			      jcgs/org-timer-pomodoros-done-count))))
	  (setq jcgs/org-timer-pomodoros-done-count 0)
	  (jcgs/org-open-hierarchical-date day))
	(insert "**** " pomodoro-string "\n")
	(basic-save-buffer)
	(setq
	 jcgs/org-timer-last-pomodoro-completion-day day
	 jcgs/org-timer-pomodoros-done-log (cons (cons (current-time-string)
						       pomodoro-string)
						 jcgs/org-timer-pomodoros-done-log)
	 ;; todo: put this in the mode line
	 jcgs/org-timer-pomodoros-done-count (1+
					      jcgs/org-timer-pomodoros-done-count))))))

(provide 'org-mode-pomodoros)
