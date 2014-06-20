;;;; type-break-patches.el
;;; Time-stamp: <2011-03-28 13:56:10 johnstu>

(unless type-break-time-last-break
  ;; we depend on this not being null
  ;; it is defined as null when declared, so set it to the load time
  (setq type-break-time-last-break (current-time)))

(defvar type-break-hook nil
  "Functions to offer typing breaks to.
They should return non-nil if they have done the typing break.")

(defvar type-break-max-keystrokes 0
  "The max keystrokes in one sub-session")

(defvar type-break-total-breaks 0
  "The number of breaks taken")

(defvar type-break-total-keystroke-count 0
  "The total number of keystrokes this session (while type-break-mode enabled).")

(defun type-break-keystroke-reset ()
  (setq type-break-total-keystroke-count
	(+ type-break-total-keystroke-count type-break-keystroke-count)
	type-break-total-breaks (1+ type-break-total-breaks)
	type-break-max-keystrokes (max type-break-keystroke-count type-break-max-keystrokes)
	type-break-keystroke-count 0
	type-break-handsfree-menu-count 0
	type-break-keystroke-warning-count 0
	type-break-current-keystroke-warning-interval type-break-keystroke-warning-intervals)
  (remove-hook 'type-break-post-command-hook 'type-break-keystroke-warning))

(defvar type-break-history nil
  "Alist of time-typing . time-resting for typing breaks.
Head of the list is the most recent pair.")

(defvar type-break-total-typing-time 0
  "Total typing time so far.")

(defvar type-break-total-break-time 0
  "Total break time so far.")

(defvar type-break-start-break-hook nil
  "Hooks for starting a typing break. You may want to turn voice input off at this point.")

(defvar type-break-end-break-hook nil
  "Hooks for ending a typing break.")

(defun type-break-0 ()
  "Take a typing break.

During the break, a demo selected from the functions listed in
`type-break-demo-functions' is run.

After the typing break is finished, the next break is scheduled
as per the function `type-break-schedule'."
  (interactive)
  (type-break-cancel-schedule)
  (run-hooks 'type-break-start-break-hook)
  (message "starting typing break")
  (let* ((continue t)
	 (this-break-minimum-length
	  (type-break-adaptive-this-break-length ))
	 (done-hook nil)		; so we only do one per break
	 (hook-did-break nil)
	 (just-done-hook nil)		; so after a hook, we continue the break, if the time is still to be made up, without asking about it
	 (start-time (current-time))
	 (end-time (current-time-string
		    (type-break-time-sum this-break-minimum-length
					 start-time)))
	 (time-spent-typing (type-break-time-difference
			     type-break-time-last-break (current-time))))
    (setq type-break-time-last-break start-time)

    (while continue
      (save-window-excursion
        ;; Eat the screen.
        ;; (and (eq (selected-window) (minibuffer-window))
	;; (other-window 1))
        ;; (delete-other-windows)
        ;; (scroll-right (window-width))
        (random t)
	(setq just-done-hook nil)
	(if done-hook
	    (progn
	      (let* ((len (length type-break-demo-functions))
		     (idx (random len))
		     (fn (nth idx type-break-demo-functions))
		     (fnres nil)
		     (type-break-good-rest-interval this-break-minimum-length)
		     )
		(condition-case ()
		    (progn
		      (message "Using type break fn %S" fn)
		      (message "Press any key to resume from typing break, which should last %s (until %s)."
			       (type-break-format-time type-break-good-rest-interval)
			       end-time)
		      (setq fnres (funcall fn)))
		  (error nil))))
	  (progn
	    (setq hook-did-break (run-hook-with-args-until-success 'type-break-hook)
		  done-hook t
		  just-done-hook t)
	    (message "Hook %s break" (if hook-did-break "did" "did not do")))))
      (cond
       (just-done-hook			; got here because hook function ran and may have
					; finished early, so go round and give another form
					; of break a chance, to complete the break
	(setq continue t))
       ((numberp this-break-minimum-length) ; should be either nil or number
	(let ((break-secs (type-break-time-difference
			   start-time (current-time))))
	  (cond
	   ((>= break-secs this-break-minimum-length)
	    (setq continue nil))

	   (t
	    ;; 60 seconds may be too much leeway if the break is only 3
	    ;; minutes to begin with.  You can just say "no" to the query
	    ;; below if you're in that much of a hurry.
					;((> 60 (abs (- break-secs this-break-minimum-length)))
					; (setq continue nil))
	    (let ((more (type-break-format-time (- this-break-minimum-length
						   break-secs))))
	      (message "Asking user whether to rest %s more" more)
	      (setq continue
		    (funcall type-break-query-function
			     (format "You really ought to rest %s more (until %s).  Continue break? "
				     more end-time))))))))
       (t
	(message "No break length defined")
	(setq continue nil))))

    (let ((break-secs (type-break-time-difference
		       start-time (current-time))))
      (setq type-break-total-break-time (+ type-break-total-break-time break-secs))
      (setq type-break-total-typing-time (+ type-break-total-typing-time time-spent-typing))
      (push (cons time-spent-typing break-secs) type-break-history)
      (message "You typed for %s then rested for %s" (type-break-format-time time-spent-typing)
	       (type-break-format-time break-secs))
      (sit-for 2)))
  (message "ending typing break")
  (type-break-keystroke-reset)
  (if (fboundp 'type-break-mode-line-countdown-or-break)
      (type-break-mode-line-countdown-or-break nil))
  (type-break-schedule)
  (run-hooks 'type-break-end-break-hook))

(defvar type-break-deferments 0
  "How many times the user has deferred a typing break since last taking one")

(defvar type-break-total-deferments 0
  "How many times the user has deferred a typing break altogether")

(defvar type-break-max-deferments 0
  "The maximum times the user has deferred a typing break since last taking one")

(defvar type-break-doing-query nil
  "Whether we are currently doing a type break query.
To prevent recursion.")

(defun type-break-do-query-0 ()
  (unless type-break-doing-query
    (let ((type-break-doing-query t))
      (cond
       ((not type-break-query-mode)
	(type-break-noninteractive-query)
	(type-break-schedule type-break-query-interval)
	(remove-hook 'type-break-post-command-hook 'type-break-do-query))
       ((sit-for 2)
	(condition-case ()
	    (cond
	     ((let ((type-break-mode nil)
		    ;; yes-or-no-p sets this-command to exit-minibuffer,
		    ;; which hoses undo or yank-pop (if you happened to be
		    ;; yanking just when the query occurred).
		    (this-command this-command))
		(let ((this-time
		       (funcall type-break-query-function
				"Take a break from typing now? ")))
		  (if this-time
		      (setq type-break-max-deferments (max type-break-deferments type-break-max-deferments)
			    type-break-deferments 0)
		    (setq type-break-deferments (1+ type-break-deferments)
			  type-break-total-deferments (1+ type-break-total-deferments)))
		  this-time))
	      (type-break))
	     (t
	      (type-break-schedule type-break-query-interval)))
	  (quit
	   (type-break-schedule type-break-query-interval)))
	(remove-hook 'type-break-post-command-hook 'type-break-do-query))))))

(defun type-break-format-time-0 (secs &optional inexact)
  (let* ((mins (/ secs 60))
	 (hours (/ mins 60))
	 (mins-in-hour (% mins 60))
	 (secs-in-hour (% secs (* 60 60)))
	 (secs-in-minute (% secs 60)))
    (unless (zerop hours) (setq inexact nil)) ; if it's over an hour, don't say a-b minutes
    (let ((hour-string
	   (cond
	    ((zerop hours) "")
	    ((= hours 1) "one hour, ")
	    ((> hours 1) (format "%d hours, " hours))
	    (t "something funny about the hours")))
	  (within-hour-string
	   (cond
	    ((= secs-in-hour 61) "1 minute, 1 second")
	    ((and (>= secs-in-hour 62) (<= secs-in-hour 119))
	     (format "1 minute, %d seconds" (- secs-in-hour 60)))
	    ((= mins-in-hour 1) "1 minute")
	    ((> mins-in-hour 1) (if inexact
			    (format "%d-%d minutes" mins-in-hour (1+ mins))
			  (format "%d minutes" mins-in-hour)))
	    ((= secs-in-hour 1) "1 second")
	    (t (format "%d seconds" secs-in-hour)))))
      (concat hour-string within-hour-string))))

(defvar initial-demo-boring-interval 60
  "How often to check the clock at first")

(defvar final-demo-boring-interval 5
  "How often to check the clock towards the end")

(defvar demo-boring-speed-up-left 120
  "When to speed up checking towards the end.")

(defun type-break-demo-minibuffer ()
  "Boring typing break demo just using the minibuffer.
Thus, you can still see what you were working on; to
improve this further, it splits the current window horizontally
-- if the screen is wide enough to be worth doing this --
and arranges it so you can see twice as much of the current
buffer."
  (let ((rmsg "Press any key to resume from typing break")
	(end-time (current-time-string
		   (type-break-time-sum type-break-good-rest-interval
					(current-time))))
        elapsed timeleft tmsg)
;;    (save-window-excursion
;;      (type-break-statistics)
;;      (sit-for 10))
    (save-window-excursion
      (let* ((sw (frame-width))
	     (sh (frame-height))
	     (lnb (count-lines (point-min) (point)))
	     (lne (count-lines (point) (point-max)))
	     (lnt (+ lnb lne)))
	(when (> sw 160)
	  (split-window-horizontally (/ sw 2))
	  (save-excursion
	    (cond
	     ((and (< lnb sh) (< lne sh))
	      (message "short buffer, showing halfway")
	      (goto-line (/ lnt 2)))
	     ((< lnb sh)
	      (message "near top, going down a bit")
	      (goto-line sh))
	     ((< lne sh)
	      (message "near bottom, going up a bit")
	      (goto-line (- lnt sh)))
	     (t))
	    (recenter -1)
	    (other-window 1)
	    (recenter 1)))
	(condition-case ()
	    (progn
	      (let ((time-interval initial-demo-boring-interval)
		    (iteration 1))
		(while (not (input-pending-p))
		  (setq elapsed (type-break-time-difference
				 type-break-time-last-break
				 (current-time)))
		  (cond
		   ((numberp type-break-good-rest-interval)
		    (setq timeleft (- type-break-good-rest-interval elapsed))
		    (if (> timeleft 0)
			(progn
			  (when (<= timeleft demo-boring-speed-up-left)
			    (setq time-interval final-demo-boring-interval))
			  (setq tmsg (format "you should rest for %s more (until %s)"
					     (type-break-format-time timeleft) end-time)))
		      (progn
			(setq time-interval initial-demo-boring-interval
			      tmsg (format "typing break has lasted %s"
					   (type-break-format-time elapsed))))))
		   (t
		    (setq tmsg (format "typing break has lasted %s"
				       (type-break-format-time elapsed)))))
		  (message tmsg)
		  (setq iteration (1+ iteration))
		  (sit-for time-interval)))
	      (let ((intrusion (read-char)))))
	  (quit
	   (and (get-buffer (buffer-name)))))))))

(provide 'type-break-patches)

;;;; end of type-break-patches.el
