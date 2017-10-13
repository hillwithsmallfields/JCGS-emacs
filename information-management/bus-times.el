
;;; The input should be like this:

;; 10 Bottisham->Cambridge depart: 06:53 07:23 07:55 09:16 10:16 11:16 12:16 13:16 14:16 15:16 16:16 17:16 18:16
;; 10 Bottisham->Cambridge arrive: 07:20 07:55 08:30 09:43 10:43 11:43 12:43 13:43 14:43 15:43 16:43 17:43 18:43

;; 10 Cambridge->Bottisham depart: 06:50 07:50 08:50 09:50 10:50 11:50 12:50 13:50 14:50 15:50 16:50 17:50 18:50
;; 10 Cambridge->Bottisham arrive: 07:15 08:15 09:15 10:15 11:15 12:15 13:15 14:15 15:15 16:15 17:07 18:15 19:15

;; 11 Bottisham->Cambridge depart: 06:28 07:33 08:28 09:28 10:28 11:28 12:28 13:28 14:28 15:28 16:28 17:28 
;; 11 Bottisham->Cambridge arrive: 06:55 08:10 08:50 09:50 10:50 11:50 12:50 13:50 14:50 15:50 16:50 17:50

;; 11 Cambridge->Bottisham depart: 07:25 08:25 09:25 10:25 11:25 12:25 13:25 14:25 15:25 16:35 18:35
;; 11 Cambridge->Bottisham arrive: 07:46 08:46 09:46 10:46 11:46 12:46 13:46 14:46 15:46 17:01 18:56

;; 12 Bottisham->Cambridge depart: 07:15 08:15 09:58 10:58 11:58 12:58 13:58 14:58 15:58 16:58 17:58 
;; 12 Bottisham->Cambridge arrive: 07:50 08:50 10:20 11:20 12:20 13:20 14:20 15:20 16:20 17:20 18:20

;; 12 Cambridge->Bottisham depart: 06:55 07:55 08:55 09:55 10:55 11:55 12:55 13:55 14:55 15:55 17:05 17:35 18:05 19:05
;; 12 Cambridge->Bottisham arrive: 07:16 08:16 09:16 10:16 11:16 12:16 13:16 14:16 15:16 16:21 17:31 18:01 18:26 19:26


(defun bus-times-fill-in-hourly-repeats ()
  "Fill in the hourly repeats.
This is for data pasted from bustimes.org.uk pages."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\([0-2][0-9]\\):\\([0-5][0-9]\\)\\s-+\\(then hourly until\\)\\s-+\\([0-2][0-9]\\)"
			    (point-max) t)
    (let* ((start-hour (string-to-number (match-string 1)))
	   (minute (match-string 2))
	   (end-hour (string-to-number (match-string 4)))
	   (hour (1+ start-hour))
	   (times nil))
      (while (< hour end-hour)
	(push (format "%02d:%s" hour minute)
	      times)
	(setq hour (1+ hour)))
      (replace-match (mapconcat 'identity (reverse times) "	")
		     nil
		     t
		     nil
		     3))))

(defun bus-times-parse-line ()
  "Parse a line of bus times."
  (let ((eol (line-end-position))
	(times nil))
    (while (re-search-forward "[0-2][0-9]:[0-5][0-9]" eol t)
      (push (match-string 0) times))
    (reverse times)))

(defun bus-times-parse-buffer ()
  "Parse a timetable buffer."
  (let ((journeys nil))
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\) \\(.+\\) depart:" (point-max) t)
      (let* ((route (match-string 1))
	     (journey (match-string 2))
	     (journey-pair (assoc journey journeys))
	     (depart-times (bus-times-parse-line)))
	(unless journey-pair
	  (setq journey-pair (cons journey nil))
	  (push journey-pair journeys))
	(forward-line 1)
	(let ((arrive-tag (concat route " " journey " arrive:")))
	  (unless (looking-at arrive-tag)
	    (error "Unpaired line for %S %S at %d: wanted to be looking at %S" route journey (point) arrive-tag)))
	(goto-char (match-end 0))
	(let* ((arrive-times (bus-times-parse-line))
	       (time-pairs (mapcar* (function
				     (lambda (a b)
				       (list a b route)))
				    depart-times arrive-times)))
	  (message "Want to add %S to %S" time-pairs journey-pair)
	  (rplacd journey-pair
		  (append time-pairs (cdr journey-pair))))))
    (dolist (journey journeys)
      (rplacd journey (sort (cdr journey) (function
					   (lambda (a b)
					     (string< (car a) (car b)))))))
    journeys))

(defun bus-times-as-minutes (hm-string)
  "Convert a bus timetable string to minutes past midnight."
  (+ (* 60 (string-to-number (substring hm-string 0 2)))
     (string-to-number (substring hm-string 3 5))))

(defun bus-times-make-table (in-file out-file)
  "From IN-FILE make a timetable in OUT-FILE."
  (interactive "fInput file:
FOutput file: ")
  (find-file in-file)
  (let ((journeys (bus-times-parse-buffer)))
    (find-file out-file)
    (erase-buffer)
    (let ((hline "  |--------+--------+-------+------|"))
      (dolist (journey journeys)

	(insert "* " (car journey)
		"\n\n" hline
		"\n  | Depart | Arrive | Route | Mins |\n"
		hline "\n")
	(dolist (bus (cdr journey))
	  (let* ((dep-str (car bus))
		 (dep (bus-times-as-minutes dep-str))
		 (arr-str (cadr bus))
		 (arr (bus-times-as-minutes arr-str)))
	    (message "bus is %S" bus)
	    (insert "  |  " dep-str
		    " | " arr-str
		    "  |  " (caddr bus)
		    "   |  " (format "%02d" (- arr dep))
		    "  |\n")))
	(insert hline
		"\n\n")))))
