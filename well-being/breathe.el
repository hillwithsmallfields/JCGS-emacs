;;;; breathe.el -- breathing exercises timer (to control hyperventilation)
;;; Time-stamp: <2005-02-07 12:36:58 john>
;;; written around May 2000

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

(provide 'breathe)
(require 'cl)

(defvar breathe-big-message t
  "*Whether to display the breathing messages large.")

(defun breathe-message (msg)
  "Display the message string MSG."
  (if breathe-big-message
      (progn
	(require 'banner)
	(banner-message (downcase msg) "Uncial heavy"))
    (message msg)))

(defun breathe-slowness-times (slowness times)
  "Prompt the user to take, each lasting SLOWNESS seconds, TIMES breaths."
  (interactive "nTime for each breath: 
nNumber of breaths: ")
  (let* ((pause (/ slowness 2))
	 (in (/ pause 2))
	 (out (- slowness pause in)))
    (dotimes (i times)
      (breathe-message "In") (sit-for in)
      (breathe-message "Out") (sit-for out)
      (breathe-message "Pause") (sit-for pause))
    (breathe-message "")))

(defun breathe-for-minute (n)
  "Prompt the user to take N breaths this minute."
  (interactive "nBreathe how many times this minute: ")
  (breathe-slowness-times (/ 60 n) n))

(defvar breathing-exercise-log-file "~/misc/breathing-exercise-log"
  "*The name of the file containing your breathing exercise log.")

(defun breathing-exercise ()
  "Prompt the user to do a breathing exercise.
The idea is to reduce the removal of carbon dioxide from the blood.
Carbon dioxide in solution is carbon acid; if you breathe too fast,
the blood becomes too alkaline, which throws various body systems out
of their normal balance, causing a variety of unpleasant symptoms
including panic attacks.
This exercise may make you feel short of breath at first, but this
is (if you are basically healthy) because the level of breathing you
have come to expect may be excessive."
  (interactive)
  (mapcar 'breathe-for-minute '(6 6 5 5 4 4 5 5 6 6))
  (save-window-excursion
    (find-file breathing-exercise-log-file)
    (goto-char (point-max))
    (insert (current-time-string) "\n")
    (basic-save-buffer)
    (bury-buffer))
  (message "Breathing exercise done"))

;;; end of breathe.el
