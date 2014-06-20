;;;; journal.el -- stuff for keeping a diary
;;; Time-stamp: <2009-04-29 12:44:11 jcgs>

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

(provide 'journal)

;; (require 'handsfree-read-number)
(require 'cl)
(require 'time-stamp)

(defvar journal-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)
    ("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
    ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  "Mapping between month names and numbers.")

;;;###autoload
(defvar journal-dates-directories
  '(("research" . (substitute-in-file-name "$COMMON/research/log/"))
    ("asr33 restoration" . (substitute-in-file-name "$COMMON/www/computing/asr33/"))
    ("talks" . (substitute-in-file-name "$COMMON/www/talks")))
  "*The directories for diary files for each journal.
This is an alist from the journal name, to a lisp form to eval to get
the directory.  Thus, finding the directory can be delayed until that
journal is chosen, which avoids looking for removable media that are
not present.")

(defvar journal-default-journal (car (car journal-dates-directories))
  "*Which journal to use by default.")

(defvar journal-month-lengths
  [0 31 28 31 30 31 30 31 31 30 31 30 31]
  "The number of days in each month.")

(defvar journal-month-full-names
  [ ""
    "January" "February" "March"
    "April" "May" "June"
    "July" "August" "September"
    "October" "November" "December"
    ]
  "The names of the months.")

(defvar journal-weekday-full-names
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "The names of the days.")

(defvar journal-new-day-previous-day-read nil
  "The previous day read.")

(defvar journal-new-day-previous-previous-day-read nil
  "The previous previous day read.")

(defun date-add-day (base-date days)
  "To BASE-DATE add DAYS.
Not a full implementation!"
  ;; todo: handle moving into adjacent months
  (let ((day-part (nthcdr 3 base-date)))
    (rplaca day-part (+ (car day-part) days))
  base-date
))

(defvar journal-history-var nil
  "History hack variable for selecting journal.")

(defun journal-current-journal (&optional specified-dir)
  "Return the journal that we are currently in, if identifiable."
  ;; todo: write the body of this
  (unless specified-dir
    (setq specified-dir default-directory))
  (catch 'found
    (let ((dirs journal-dates-directories))
      (while dirs
	(let ((directory (if (stringp (cdar dirs))
			     (cdar dirs)
			   (eval (cdar dirs)))))
	  (when (and (stringp directory)
		     (>= (length specified-dir)
			 (length directory))
		     (string= (substring specified-dir 0 (length directory))
			      directory
			      ))
	    (throw 'found (caar dirs))))
	(setq dirs (cdr dirs))))
    nil))

(defun journal-choose-journal (prompt)
  "Choose a journal, prompting with PROMPT."
  (setq journal-history-var (mapcar 'car journal-dates-directories))
  (completing-read prompt
		   journal-dates-directories
		   nil
		   t
		   journal-default-journal
		   (cons 'journal-history-var
			 (position journal-default-journal
				   journal-history-var
				   :test 'string=))))

(defun journal-calculate-date-at-point ()
  "Calculate the date at point."
  (let* ((date-at (save-excursion
		    (re-search-backward "\\[\\([0-9]+\\)[- ]\\([a-z]+\\)[- ]\\([0-9]+\\)\\]"
					(point-min) t)))
	 (found-date (if date-at
			 (list nil	; 0
			       nil	; 1
			       nil	; 2
			       (string-to-int (match-string-no-properties 3)) ; 3 = day
			       (cdr (assoc (match-string-no-properties 2) 
					   journal-monthname-alist ; journal-monthname-alist
					   )) ; 4 = month
			       (string-to-int (match-string-no-properties 1)) ; 5 = year
			       )
		       nil)))
    found-date))

(defun journal-new-day-interactive-reader (&optional two)
  "Interactive command reader for journal-new-day.
Made into a separate routine for legibility.
With optional argument, read two days to give a range within a month."
  ;; we try to extract a date from somewhere before point;
  ;; if you find one we can pass its position as a hint where to enter the new day
  (let* ((journal (journal-choose-journal "Journal: "))
	 (date-at (save-excursion
		    (re-search-backward "\\[\\([0-9]+\\)[- ]\\([a-z]+\\)[- ]\\([0-9]+\\)\\]"
					(point-min) t)))
	 (found-date (if date-at
			 (list nil	; 0
			       nil	; 1
			       nil	; 2
			       (string-to-int (match-string-no-properties 3)) ; 3 = day
			       (cdr (assoc (match-string-no-properties 2) 
					   journal-monthname-alist ; journal-monthname-alist
					   )) ; 4 = month
			       (string-to-int (match-string-no-properties 1)) ; 5 = year
			       )
		       nil))
	 ;; (thing (message "found-date=%S month=%S" found-date (match-string-no-properties 2)))
	 (direction (if (and (integerp journal-new-day-previous-day-read)
			     (integerp journal-new-day-previous-previous-day-read))
			(if (> journal-new-day-previous-day-read
			       journal-new-day-previous-previous-day-read)
			    1
			  -1)
		      nil))
	 (now (if found-date
		  (if direction
		      (date-add-day found-date direction)
		    found-date)
		(decode-time (current-time))))
	 (now-year (nth 5 now))
	 (now-month-number (nth 4 now))
	 (now-day (nth 3 now))
	 (year (handsfree-read-number "Year: " 1900 2100 now-year))
	 (month (handsfree-read-number "Month: " 1 12 now-month-number))
	 (day (handsfree-read-number (if two "Start day: " "Day: ")
				     1
				     (aref journal-month-lengths month)
				     (if (= month now-month-number)
					 now-day
				       (if (> month now-month-number)
					   1
					 (aref journal-month-lengths month)))))
	 (end-day (if two 
		      (handsfree-read-number "End day: " day (aref journal-month-lengths month) day)
		    nil))
	 (month-name (substring (aref journal-month-full-names month) 0 3)))
    ;; If the day number has gone over the end of the month, go into the next month.
    ;; Keep doing this as long as necessary.
    (while (> day (aref journal-month-lengths month))
      (decf day (aref journal-month-lengths month))
      (incf month)
      (if (> month 12)
	  (setq month 1
		year (1+ year))))
    (setq month-name (substring (aref journal-month-full-names month) 0 3)
	  journal-new-day-previous-previous-day-read journal-new-day-previous-day-read
	  journal-new-day-previous-day-read day
	  journal-default-journal journal)
    (if two
	(list journal year month month-name day end-day)
      (list journal year month month-name day

	   ;;;;;;;;; not quite right...
	    (if t;; (and (= year now-year) (= month now-month) (= day now-day))
		date-at
	      nil)))))

(defun journal-new-year (journal year)
  "In JOURNAL, make the directory for YEAR (unless it already exists).
Return the directory concerned."
  (let* ((journal-dates-directory (eval (cdr (assoc journal journal-dates-directories))))
	 (year-directory (expand-file-name (format "%04d" year)
					   journal-dates-directory)))
    (unless (file-directory-p year-directory)
      (make-directory year-directory)
      (save-window-excursion
	(find-file (expand-file-name "index.html" year-directory))
	(goto-char (point-min))
	(if (re-search-forward "<h1>.*</h1>" (point-max) t)
	    (let ((newtext
		   (concat (format "<h1>%d</h1>\n\n<dl>\n  <dt> " year)
			   (mapconcat (lambda (month)
					(format "<a href=\"%02d-%02d.html\">%s</a>" 
						(mod year 100) month 
						(aref journal-month-full-names month)))
				      '(1 2 3 4 5 6 7 8 9 10 11 12)
				      "\n  <dd>\n  <dt> ")
			   "  <dd>\n</dl>\n\n")))
	      (replace-match newtext t t)))))
    (save-window-excursion
      (find-file (expand-file-name "index.html" journal-dates-directory))
      (save-excursion
	(goto-char (point-min))
	;; todo: fill in year and month links in master index))
	))
    year-directory))

(defun journal-new-month (year-directory year month)
  "Set up a journal month file.
Return the filename."
  (let ((file (expand-file-name (format "%02d-%02d.html" (mod year 100) month) year-directory)))
    (save-window-excursion
      (find-file file)
      (unless (file-exists-p file)
	;; (message "New diary month! must do something about this!")
	(let ((month-description (format "%s %s"  (aref journal-month-full-names month) year)))
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "<title></title>" (point-max) t)
	      (replace-match (format "<title>%s</title>" month-description) t t)))
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "<h1></h1>" (point-max) t)
	      (replace-match (format "<h1>%s</h1>" month-description) t t)))))
      (journal-add-neighbouring-month-links))
    file))

(defvar journal-made-entry nil
  "Whether you have made a journal entry during this session.")

(defvar journal-entry-body-preamble "\n<p>"
  "Lead-in for journal entry.")

(defvar journal-entry-body-postamble  "</p>\n\n"
  "Lead-out for journal entry.")

(make-variable-buffer-local ' journal-entry-body-preamble)
(make-variable-buffer-local 'journal-entry-body-postamble)

;;;###autoload
(defun journal-new-day (journal year month monthname day &optional prev-date-at)
  "Start a new day's entry. The arguments are JOURNAL YEAR MONTH MONTHNAME DAY.
An optional extra argument gives where in the buffer the previous day was found."
  (interactive (journal-new-day-interactive-reader))

  (setq journal-made-entry t)		; mark that some journal activity has happened

  (if (null monthname) ; you can give just the number (but must always give the number)
      (setq monthname (substring (aref journal-month-full-names month) 0 3))
    (setq monthname (substring monthname 0 3)))

  (find-file (journal-new-month
	      (journal-new-year journal year)
	      year
	      month))

  (message "That appears to be a %s" (aref journal-weekday-full-names (nth 6 (decode-time (encode-time 0 0 0 day month year)))))
  ;; see whether we already have this day
  (if (journal-find-end-of-day journal year month day)

      (progn
	(message "Already started that day")
	(insert "\n" journal-entry-body-preamble)
	(save-excursion (insert journal-entry-body-postamble))	)

    ;; we don't already have that day, so must find a place for it
    ;; if given a hint, use that
    (if (and nil prev-date-at)
	(progn
	  (goto-char prev-date-at)
	  (if (re-search-forward "<h[2r]>" (point-max) t)
	      (goto-char (match-beginning 0))))

      ;; now the long case of trying to locate the day

      (let ((i 1))
	(catch 'found
	  (while (< i 31)
	    (let ((before (- day i))
		  (after (+ day i)))
	      ;; (message "Looking for day %d +/- %d -->%d,%d" day i before after)
	      (if (or (and (> before 0)
			   (journal-find-end-of-day journal year month before))
		      (and (< after 32)
			   (journal-find-start-of-day journal year month after)))
		  (throw 'found t))			 
	      (incf i)))
	  (search-backward "<hr>" (point-min) t))))
    (skip-syntax-backward " ")
    (let ((last-non-blank (point)))
      (skip-syntax-forward " ")
      (delete-region last-non-blank (point)))
    (insert "\n<br clear=\"all\">\n\n"
	    (format "<h2><a name=\"%02d\">[%d-%s-%02d]</a></h2>" day year monthname day)
	    "\n" journal-entry-body-preamble)
    (save-excursion
      (insert journal-entry-body-postamble))))

;;;###autoload
(defun journal-today (journal)
  "Find today, in JOURNAL."
  (interactive
   (list (journal-choose-journal "Find today in journal: ")))
  (let* ((time-list (decode-time)))
    (journal-new-day journal
		     (nth 5 time-list)
		     (nth 4 time-list)
		     nil		; will be done for us
		     (nth 3 time-list))))

(defun journal-increment-day (starting &optional increment)
  "Increment STARTING date."
  (or increment (setq increment 1))
  (let* ((day (nth 3 starting))
	 (month (nth 4 starting))
	 (month-length (aref journal-month-lengths month))
	 (year (nth 5 starting)))
    (setq day (+ day increment))
    (while (> day month-length)
      (setq day (- day month-length)
	    month (1+ month)
	    year (if (> month 12) (1+ year) year)
	    month (if (= month 13) 1 month)
	    month-length (aref journal-month-lengths month)))
    (list (nth 0 starting)
	  (nth 1 starting)
	  (nth 2 starting)
	  day
	  month
	  year
	  nil
	  (nth 7 starting)
	  (nth 8 starting))))

;;;###autoload
(defun journal-next-day ()
  "Find the next day, in the current journal."
  (interactive)
  (let* ((which-journal (journal-current-journal))
	 (current-date (journal-calculate-date-at-point))
	 (new-date (journal-increment-day current-date)))
    (journal-new-day which-journal
		     (nth 5 new-date)
		     (nth 4 new-date)
		     nil		; will be done for us
		     (nth 3 new-date))))

(defun journal-emacs-time (journal time)
  "Select or make a journal entry for TIME."
  (when (<= (length time) 3)
    (setq time (decode-time time)))
  (journal-new-day journal
		   (nth 5 time)
		   (nth 4 time)
		   (aref journal-month-full-names (nth 4 time))
		   (nth 3 time)))

(defun journal-read-web-page (url observations)
  "Log that you have read URL and made OBSERVATIONS about it."
  (interactive "sURL: 
sObservations: ")
  (journal-today "research")
  (insert "Read <a href=\"" url "\">" url "</a>: " observations))

(defun fill-in-days (journal year month monthname firstday lastday)
  "Create entries in JOURNAL for YEAR MONTH MONTHNAME from FIRSTDAY to LASTDAY."
  (interactive (journal-new-day-interactive-reader t))
  (let ((day firstday))
    (while (<= day lastday)
      (journal-new-day journal year month monthname day)
      (incf day))))

(defun journal-month-this-page ()
  "Return (year . month) for the current page, if guessable."
  (let ((fn (buffer-file-name)))
    (if (and fn
	     (string-match "\\([0-9][0-9]\\)-\\([01][0-9]\\)\\.html" fn))
	(cons (string-to-int (substring fn (match-beginning 1) (match-end 1)))
	      (string-to-int (substring fn (match-beginning 2) (match-end 2))))
      nil)))

(defun journal-month-next (year-month)
  "Return the next month to YEAR-MONTH (year . month)"
  (if (= (cdr year-month) 12)
      (cons (1+ (car year-month)) 1)
    (cons (car year-month) (1+ (cdr year-month)))))

(defun journal-month-previous (year-month)
  "Return the previous month to YEAR-MONTH (year . month)"
  (if (= (cdr year-month) 1)
      (cons (1- (car year-month)) 12)
    (cons (car year-month) (1- (cdr year-month)))))

(defun journal-add-neighbouring-month-links ()
  "Add links to next and previous months, if not present"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward ".html\">Last month</a>]" (point-max) t)
      (let ((last-month (journal-month-previous (journal-month-this-page))))
	(goto-char (point-min))
	(search-forward "</h1>")
	(insert "\n\n<p>\n[<a href=\""
		(format "%02d-%02d" (car last-month) (cdr last-month))
		".html\">Last month</a>]\n[<a href=\"index.html\">This year</a>]\n[<a href=\"../index.html\">Dates index</a>]\n</p>\n")))
    (goto-char (point-min))
    (unless (search-forward ".html\">Next month</a>]" (point-max) t)
      (let ((next-month (journal-month-next (journal-month-this-page))))
	(goto-char (point-max))
	(search-backward "<hr>")
	(goto-char (match-end 0))
	(insert "<p>\n[<a href=\""
		(format "%02d-%02d" (car next-month) (cdr next-month))
		".html\">Next month</a>]\n[<a href=\"index.html\">This year</a>]\n[<a href=\"../index.html\">Dates index</a>]\n</p>\n")))))

(defun journal-find-start-of-day (journal year month day)
  "Move to the start of the specified day, if it can be found."
  (let* ((year-string (if (stringp year)
			  year
			(format "%04d" year)))
	 (short-year-number (mod
			     (if (numberp year)
				 year
			       (string-to-int year))
			     100))
	 (month-number (if (numberp month)
			   month
			 (cdr (assoc month journal-monthname-alist))))
	 (month-file-string (format "%02d-%02d.html"
				    short-year-number
				    month-number)))
    (find-file (expand-file-name
		month-file-string
		(expand-file-name year-string
				  (eval (cdr (assoc journal
						    journal-dates-directories)))))))
  (goto-char (point-max))
  (let ((marker (format "<h2><a name=\"%02d\">[%04d-%s-%02d]</a></h2>"
			(if (numberp day)
			    day
			  (string-to-int day))
			(if (numberp year)
			    year
			  (string-to-int year))
			(substring (cond
				    ((numberp month)
				     (car (rassoc month journal-monthname-alist)))
				    ((stringp month)
				     month))
				   0 3)
			(if (numberp day)
			    day
			  (string-to-int day)))))
    ;; (message "Looking for %s" marker)
    (if (search-backward marker (point-min) t)
	t
      nil)))

(defun journal-find-end-of-day (journal year month day)
  "Move to the end of the specified day, if it can be found."
  (if (journal-find-start-of-day journal year month day)
      (progn				; yes;try to move to the end of it
	(goto-char (match-end 0))	; uses match-data from journal-find-start-of-day
	(if (re-search-forward "<h[2r]>" (point-max) t)
	    (goto-char (match-beginning 0)))	 
	t)
    nil))

;; (defun journal-current-day ()
;;   "Get the current day."
;;   (interactive)
;;   (save-excursion
;;     (if (re-search-backward "\\[\\([0-9]+\\)-\\([a-z]+\\)-\\([0-9]+\\)\\]" (point-min) t)
;; 	(let* ((year (string-to-int (match-string-no-properties 1)))
;; 	      (month (cdr (assoc (match-string-no-properties 2) journal-monthname-alist)))
;; 	      (day (string-to-int (match-string-no-properties 3)))
;; 	      (dow (aref journal-weekday-full-names (nth 6 (decode-time (encode-time 0 0 0 day month year)))))
;; 	      )
;; 	  (message "%d-%d-%d is a %s" year month day dow)
;; 	  (list year month day dow))
;;       nil)))

(defun journal-reflection ()
  "Add a reflection section to the journal."
  (interactive)
  (tempo-template-html-blockquote)
  (tempo-template-html-italic))

(require 'html-helper-mode)
(require 'generic-text)

;; copy across all the properties of html-helper-mode particularly
;; because I want all the modal functions that I define in
;; generic-text (which is why I require generic-text, above)
(let ((props (symbol-plist 'html-helper-mode)))
  (while props
    (put 'html-journal-helper-mode
	 (car props)
	 (cadr props))
    (setq props (cddr props))))

(defvar html-journal-helper-map (copy-keymap html-helper-mode-map)
  "Keymap for html journal mode.")

(define-key html-journal-helper-map "\C-c\C-dn" 'journal-new-day)

(defvar journal-abbrev-table (make-abbrev-table)
  "Abbrev table for journal mode.
Causes insertion of links to other files where available.")

(defvar html-journal-helper-mode-hook nil
  "Functions to run when going into html-journal-helper-mode.")

;;;###autoload
(defun html-journal-helper-mode ()
  "A variant of HTML helper mode, for keeping journals (records, diaries)."
  (interactive)
  (html-helper-mode)
  (setq major-mode 'html-journal-helper-mode
	mode-name "HTML Journal"
	local-abbrev-table journal-abbrev-table
	)
  (run-hooks 'html-journal-helper-mode-hook)
  (use-local-map html-journal-helper-map)
  (abbrev-mode 1)
  )

;;; end of journal.el
