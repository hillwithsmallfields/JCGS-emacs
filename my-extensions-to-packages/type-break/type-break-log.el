;;;; type-break-log.el
;;; Time-stamp: <2021-11-14 18:24:30 jcgs>

(provide 'type-break-log)
(require 'handsfree-read-number)

(defvar type-break-started-at (cons (current-time-string) (current-time))
  "When the type break setup was loaded.")

(defvar type-break-initial-health -1
  "How good your hands were at the start of the session")

(defvar type-break-initial-health-comment nil
  "Comment about how good your hands were at the start of the session")

(defun type-break-log-to-file ()
  "Log the typing break data for this session to a file"
  (interactive)
  (let* ((health
	  (handsfree-read-percentage
	   (format "How well are the limbs concerned (was %d at start)? (0=bad, 100=fine) "
		    type-break-initial-health)
	   type-break-initial-health))
	 (health-comment (read-from-minibuffer "Annotation about RSI? ")))
    (find-file "~/.rsi-log")
    (goto-char (point-max))
    (search-backward "))" (point-min) t)
    (insert "(\""
	    (car type-break-started-at) "\" \"" (current-time-string) "\" "
	    (format " %d\n " (type-break-time-difference (cdr type-break-started-at) (current-time)))
	    (format " (%d . %d) (%d %d %d)"
		    type-break-total-keystroke-count
		    type-break-max-keystrokes

		    type-break-total-breaks
		    type-break-total-break-time
		    type-break-total-typing-time)

	    " ("
	    (if (or (null type-break-initial-health-comment)
		    (string= type-break-initial-health-comment ""))
		(format "%d" type-break-initial-health)
	      (format "(%d . \"%s\")" type-break-initial-health type-break-initial-health-comment))
	    " . "
	    (if (or (null health-comment)
		    (string= health-comment ""))
		(format "%d" health)
	      (format "(%d . \"%s\")" health health-comment))
	    ")"

	    (format " (%d . %d) (%d . %d)"
		    type-break-interval
		    type-break-good-rest-interval

		    type-break-total-deferments
		    type-break-max-deferments

		    )
	    ")\n")
    (basic-save-buffer)
    ))

(defun type-break-read-log-file ()
  "Read the typing break log file"
  (load-file "~/.rsi-log"))

(defvar type-break-log nil
  "The typing break log.")

(defun type-break-previous-session (n)
  "Return the Nth previous session data."
  (unless type-break-log (type-break-read-log-file))
  (nth (1- (- (length type-break-log) n)) type-break-log))

(defun type-break-log-session-start (le)
  (nth 0 le))

(defun type-break-log-session-end (le)
  (nth 1 le))

(defun type-break-log-session-duration (le)
  (nth 2 le))

(defsubst type-break-log-keystrokes (le)
  (nth 3 le))

(defun type-break-log-total-keystrokes (le)
  (car (type-break-log-keystrokes le)))

(defun type-break-log-max-keystrokes-burst (le)
  (cdr (type-break-log-keystrokes le)))

(defsubst type-break-log-breaks (le)
  (nth 4 le))

(defsubst type-break-log-breaks-count (le)
  (nth 0 (type-break-log-breaks le)))

(defun type-break-log-total-break-time (le)
  (if (integerp (cdr (type-break-log-breaks le)))
      (cdr (type-break-log-breaks le))
    (nth 1 (type-break-log-breaks le))))

(defun type-break-log-total-typing-time (le)
  (if (integerp (cdr (type-break-log-breaks le)))
      nil
    (nth 2 (type-break-log-breaks le))))

(defsubst type-break-log-health (le)
  (nth 5 le))

(defsubst type-break-log-initial-health (le)
  (car (type-break-log-health le)))

(defsubst type-break-log-eventual-health (le)
  (cdr (type-break-log-health le)))

(defun type-break-log-initial-health-number (le)
  (let ((initial (type-break-log-initial-health le)))
    ;; (message "Got initial health %S" initial)
    (if (numberp initial)
	initial
      (car initial))))

(defun type-break-log-initial-health-comment (le)
  (let ((i (car (type-break-log-initial-health le))))
    (if (numberp i)
	""
      (cdr i))))

(defun type-break-log-eventual-health-number (le)
  (let ((eventual (type-break-log-eventual-health le)))
    (if (numberp eventual)
	eventual
      (car eventual))))

(defun type-break-log-eventual-health-comment (le)
  (let ((i (cdr (type-break-log-initial-health le))))
    (if (numberp i)
	""
      (cdr i))))

(defsubst type-break-interval-settings (le)
  (nth 6 le))

(defun type-break-log-interval (le)
  (car (type-break-interval-settings le)))

(defun type-break-log-good-rest-interval (le)
  (cdr (type-break-interval-settings le)))

(defsubst type-break-log-deferments (le)
  (nth 7 le))

(defun type-break-log-total-deferments (le)
  (car (type-break-log-deferments le)))

(defun type-break-log-max-consecutive-deferments (le)
  (cdr (type-break-log-deferments le)))

(defun type-break-log-proportion-typing-time (le)
  (let ((typing (type-break-log-interval le))
	(resting (type-break-log-good-rest-interval le)))
    (/ typing (+ typing resting))))

(defun niltozero (x) (if (null x) 0 x))

(defun type-break-log-combine-entries (entries)
  "Combine a list of type break log ENTRIES to make a single entry."
  (let* ((start )
	 (end )
	 (duration (apply '+
			  (mapcar 'niltozero
				  (mapcar 'type-break-duration entries))))
	 (total-keystrokes (apply '+
				  (mapcar 'niltozero
					  (mapcar 'type-break-total-keystrokes entries))))
	 (keystrokes-burst (apply '+
				  (mapcar 'niltozero
					  (mapcar 'type-break-keystrokes-burst entries))))
	 (keystrokes )
	 (break-count (apply '+
			     (mapcar 'niltozero
				     (mapcar 'type-break-break-count entries))))
	 (total-break-time (apply '+
				  (mapcar 'niltozero
					  (mapcar 'type-break-total-break-time entries))))
	 (total-typing-time (apply '+
				   (mapcar 'niltozero
					   (mapcar 'type-break-total-typing-time entries))))
	 (breaks )
	 (initial-health-number (apply '+
				       (mapcar 'niltozero
					       (mapcar 'type-break-initial-health-number entries))))
	 (initial-health-common )
	 (initial-health )
	 (eventual-health-number (apply '+
					(mapcar 'niltozero
						(mapcar 'type-break-eventual-health-number entries))))
	 (eventual-health-common )
	 (eventual-health )
	 (health )
	 (interval (apply '+
			  (mapcar 'niltozero
				  (mapcar 'type-break-interval entries))))
	 (good-rest-interval (apply '+
				    (mapcar 'niltozero
					    (mapcar 'type-break-good-rest-interval entries))))
	 (interval-settings )
	 (total-deferments (apply '+
				  (mapcar 'niltozero
					  (mapcar 'type-break-total-deferments entries))))
	 (max-consecutive-deferments (apply '+
					    (mapcar 'niltozero
						    (mapcar 'type-break-max-consecutive-deferments entries))))
	 (deferments )
	 (entry ))
    entry
    ))

(defun type-break-log-to-spreadsheet ()
  "Output the typing break info to a spreadsheet import format"
  (interactive)
  (find-file (substitute-in-file-name "$SYNCED/www/work/log.csv"))
  (erase-buffer)
  (dolist (entry (type-break-log-points-for-plotting))
    (dolist (cell entry)
      (cond
       ((stringp cell)
	(insert (format "\"%s\"," cell)))
       ((numberp cell)
	(insert (format "%d," cell)))))
    (insert "\r\n"))
  (basic-save-buffer))

(defun type-break-point-for-plotting (raw-point)
  "Return a list of things for plotting from RAW-POINT."
  (list (type-break-log-session-start raw-point)
	(type-break-log-session-duration raw-point)
	 (type-break-log-total-keystrokes raw-point)
	 (type-break-log-max-keystrokes-burst raw-point)
	 (type-break-log-total-break-time raw-point)
	 (type-break-log-total-typing-time raw-point)
	 (- (type-break-log-initial-health-number raw-point)
	    (type-break-log-eventual-health-number raw-point))
	 (type-break-log-proportion-typing-time raw-point)
	;; (type-break-log-good-rest-interval raw-point)
	;; (type-break-log-interval raw-point)
	))

(defun type-break-log-points-for-plotting ()
  "Return the type break log data in a suitable form for plotting."
  (type-break-read-log-file)
  (cons (list
	 "Date"
	 "Start"
	 "Duration"
	 "Total keystrokes"
	 "Max keystroke burst"
	 "Total break time"
	 "Total typing time"
	 "Change in how my hands felt"
	 "Ratio of typing and resting"
	 )
	(mapcar 'type-break-point-for-plotting type-break-log)))

(defun type-break-log-plot ()
  "Produce a plot of the type break log."
  (interactive)
  (require 'dated-plot)
  (dated-plot-to-file (type-break-log-points-for-plotting) "~/type-break-log-together.ps"))

(defun type-break-log-plots ()
  "Produce a family of plots of the type break log."
  (interactive)
  (require 'dated-plot)
  (dated-plots-to-file (type-break-log-points-for-plotting) "~/type-break-log.ps"))

(defun type-break-plot-pairs ()
  "Produce all the pairwise scattergrams"
  (interactive)
  (pair-plots-to-file
   (mapcar 'cdr
	   (type-break-log-points-for-plotting))
   "~/type-break-log-pairs.ps"))

;;; end of type-break-log.el
