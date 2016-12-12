;;;; load-lisp-while-idle.el -- load emacs-lisp files while idle
;;; Time-stamp: <2008-10-04 18:59:47 jcgs>

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

;;; This lets you load Emacs lisp files while your Emacs is sitting
;;; idle. When you start typing again, its suspends itself unto you pause.

(defgroup load-lisp-while-idle
  nil
  "Control of loading Emacs-Lisp files while Emacs is idle.")

(defcustom load-lisp-while-idle-delay 2
  "How many seconds to wait before starting or resuming idle-loading."
  :group 'load-lisp-while-idle
  :type 'integer)

(defvar load-lisp-while-idle-timer nil
  "The current load-lisp-while-idle timer.")

(defvar load-lisp-while-idle-resume-timer nil
  "A timer for resuming load-lisp-while-idle after it has suspended itself.")

(defvar load-lisp-while-idle-files nil
  "The queue of files waiting to be loaded while idle.")

(defun load-lisp-while-idle (&rest files)
  "Arrange to load FILES while idle."
  ;; Cancel any old timer -- partly avoids having several of them, and
  ;; partly avoids having a timing hazard as we add to the list of
  ;; files to be loaded (the function that does the actual loading
  ;; modifies the same variable) -- perhaps that should be redesigned
  ;; to use a separate variable?
  (interactive "fLoad file (while idle) : ")
  (when load-lisp-while-idle-resume-timer
    (cancel-timer load-lisp-while-idle-resume-timer)
    (setq load-lisp-while-idle-resume-timer nil))
  (when load-lisp-while-idle-timer
    (cancel-timer load-lisp-while-idle-timer)
    (setq load-lisp-while-idle-timer nil))
  (setq load-lisp-while-idle-files
	(append load-lisp-while-idle-files
		files)
	load-lisp-while-idle-timer (run-with-idle-timer 
				    load-lisp-while-idle-delay
				    t
				    'load-lisp-while-idle-do-some))
  (message "%S now on queue for loading" load-lisp-while-idle-files))

(defvar load-lisp-while-idle-current-buffer nil
  "The buffer currently being processed by load-lisp-while-idle-do-some.")

(defvar load-lisp-while-idle-marker nil
  "A marker used for reading the load-while-idle files.")

(defvar load-lisp-while-idle-burst-time 3
  "How many seconds to run for at a time.")

(defun load-lisp-while-idle-revive ()
  (interactive)
  (when (timerp load-lisp-while-idle-timer)
    (cancel-timer load-lisp-while-idle-timer))
  (setq load-lisp-while-idle-timer (run-with-idle-timer 
				    load-lisp-while-idle-delay
				    t
				    'load-lisp-while-idle-do-some)))

(defvar idle-load-verbose nil
  "*Whether to output loading messages.")

(defun load-lisp-while-idle-do-some (&optional do-the-lot)
  "Load some more of the files in `load-lisp-while-idle-files'.
Keep on evaluating sexps from those files until
`load-lisp-while-idle-burst-time' seconds have elasped.
If the user provides any input, quit for now.
With optional argument DO-THE-LOT, do all the rest of the
loading regardless of time and of user input."
  (when load-lisp-while-idle-resume-timer
    (cancel-timer load-lisp-while-idle-resume-timer)
    (setq load-lisp-while-idle-resume-timer nil))
  (when (null load-lisp-while-idle-marker)
    (setq load-lisp-while-idle-marker (make-marker)))
  (when (null load-lisp-while-idle-current-buffer)
    (setq load-lisp-while-idle-current-buffer
	  (find-file-noselect (pop load-lisp-while-idle-files)))
    (with-current-buffer load-lisp-while-idle-current-buffer
      (set-marker load-lisp-while-idle-marker
		  (point-min)
		  load-lisp-while-idle-current-buffer)))
  (let ((old-message (current-message)))
    (when idle-load-verbose
      (message "Loading %s while idle, from %s"
	       (buffer-file-name load-lisp-while-idle-current-buffer)
	       (current-time-string)))
    (let ((time-started (current-time)))
      (with-current-buffer load-lisp-while-idle-current-buffer
	(let ((more t)
	      (in-time-slot nil))
	  (save-match-data
	    (while (and more
			(or do-the-lot
			    (and (not (input-pending-p))
				 (setq in-time-slot
				       (<= (cadr
					    (time-subtract (current-time)
							   time-started))
					   load-lisp-while-idle-burst-time)))))
	      (condition-case evar
		  (eval (read load-lisp-while-idle-marker))
		(error (setq more nil
			     load-lisp-while-idle-current-buffer nil)
		       (set-marker load-lisp-while-idle-marker nil)))))
	  (setq load-lisp-while-idle-resume-timer
		(run-with-idle-timer
		 ;; Compute an idle time load-lisp-while-idle-delay
		 ;; more than the current value.
		 (time-add (current-idle-time)
			   (seconds-to-time load-lisp-while-idle-delay))
		 nil
		 'load-lisp-while-idle-do-some)))))
    (when idle-load-verbose
      (message "suspended idle-loading at %s" (current-time-string)))))

(provide 'load-lisp-while-idle)

;;; end of load-lisp-while-idle.el
