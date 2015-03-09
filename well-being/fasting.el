;;; fasting.el --- Calculate fasting times

;; Copyright (C) 2013, 2015  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience, calendar

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

;; Calculate how long I've been fasting.  I may add calorie-related
;; functions later.

;;; Code:

(defconst fasting-time-weekdays
  (let ((alist nil))
    (dotimes (i 7)
      (push (cons (aref calendar-day-name-array i)
		  i)
	    alist))
    alist)
  "Alist mapping weekday names to numbers.")

(defun fasting-time (started)
  "Calculate fasting time.
Argument STARTED is when the fast started."
  ;; todo: use date/time reading code from org-mode?
  (interactive
   (let* ((completion-ignore-case t)
	  (now (decode-time))
	  (today (nth 6 now))
	  (start-day (cdr
		      (assoc (completing-read "Start day: "
					      fasting-time-weekdays
					      nil t)
			     fasting-time-weekdays)))
	  (days-ago
	   (if (> today start-day)
	       (- today start-day)
	     (- (+ today 7) start-day))
	   ))
     (list
      days-ago
      )))
  (message "Started %d days ago" started)
  )

;; a more primitive one, with fixed offset, to build up into the full function

(defun hours (offset-hours offset-minutes)
  "Print hours table, with OFFSET-HOURS and OFFSET-MINUTES."
  (interactive "nOffset hours: \nnOffset minutes: ")
  (with-output-to-temp-buffer "*hours*"
    (princ "Day   Time  Hours\n")
    (dotimes (hour 133)
      (let ((hour-in-day (+ hour offset-hours)))
    (princ (format "day %d %02d:%02d %d\n" (/ hour-in-day 24) (% hour-in-day 24) offset-minutes hour))))))

(provide 'fasting)
;;; fasting.el ends here
