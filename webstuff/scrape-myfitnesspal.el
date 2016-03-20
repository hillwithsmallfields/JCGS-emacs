;;; Scrape data from myfitnesspal.com food diary pages
;;; Time-stamp: <2016-03-20 20:49:41 jcgs>
;;; The resulting data should convert easily to CSV etc.
;;; Started by John Sturdy <jcg.sturdy@gmail.com> on 2016-03-18

;; sample url: http://www.myfitnesspal.com/food/diary?date=2016-03-17

(defun myfitnesspal-fetch-days-ago (days)
  "Fetch the myfitnesspal food diary for DAYS ago."
  (interactive "nGet data for days ago: ")
  (let ((calculated-date (time-add (current-time) (days-to-time (- days)))))
    (message "Calculated date is %s" (current-time-string calculated-date))
    (myfitnesspal-fetch calculated-date)))

(defun myfitnesspal-fetch (date)
  "Fetch the myfitnesspal food diary for DATE."
  (let ((url (format-time-string
	      "http://www.myfitnesspal.com/food/diary?date=%Y-%m-%d" date)))
    (message "url is %s" url)
    (browse-url url)))

(defun convert-to-numbers (strings-list)
  "Convert the STRINGS-LIST into a list of numbers."
  (mapcar (function
	   (lambda (str)
	     (string-to-number (delete ?, str))))
	  strings-list))

(defun zip-cons-lists (a b)
  (let ((result nil))
    (while (and a b)
      (setq result (cons (cons (car a) (car b)) result)
	    a (cdr a)
	    b (cdr b)))
    (nreverse result)))

(defun myfitnesspal-parse-file (file)
  "Parse a myfitnesspal FILE.
The result is an alist element of meal data."
  (save-window-excursion
    (let ((buffer-to-scrape (find-buffer-visiting file)))
      (if (bufferp buffer-to-scrape)
	  (progn
	    (set-buffer buffer-to-scrape)
	    (revert-buffer t t))
	(find-file file)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "// </food/diary\\?date=\\(....\\)-\\(..\\)-\\(..\\)>")
      (let* ((year (string-to-number (match-string-no-properties 1)))
	     (month (string-to-number (match-string-no-properties 2)))
	     (yesterday-day (string-to-number (match-string-no-properties 3)))
	     (yesterday-time (encode-time 1 1 1 yesterday-day month year))
	     (today-time (time-add yesterday-time (days-to-time 1)))
	     (decoded-today (decode-time today-time))
	     (year-number (nth 5 decoded-today))
	     (month-number (nth 4 decoded-today))
	     (day-number (nth 3 decoded-today))
	     (column-labels-string (save-excursion
				     (re-search-forward "Breakfast\\s-+\\(.+\\)$")
				     (match-string-no-properties 1)))
	     (column-labels (mapcar 'intern
				    (split-string column-labels-string
						  "\\s-+" t)))
	     (meals nil))
	(while (search-forward "Add Food" (point-max) t)
	  (save-excursion
	    (re-search-forward "^\t[0-9,\t ]+$" (point-max) t)
	    (let* ((meal-data (convert-to-numbers
			       (split-string (match-string-no-properties 0) "\\s-+" t))))
	      (re-search-backward "Breakfast\\|Lunch\\|Supper\\|Snacks")
	      (let* ((meal-name (intern (match-string-no-properties 0))))
		(push (cons meal-name (zip-cons-lists column-labels meal-data))
		      meals)))))
	(re-search-forward  "^Totals\\s-+\\([0-9,\t ]+\\)$")
	(push (cons 'Totals (zip-cons-lists
			     (convert-to-numbers
			      (split-string (match-string-no-properties 1) "\\s-+" t))
			     column-labels))
	      meals)
	(cons today-time (cons (list year-number month-number day-number)
			       (nreverse meals)))))))

(defvar myfitnesspal-accumulated-data-file "~/myfitnesspal-data-el"
  "File containing accumulated scrapes.")

(defun myfitnesspal-read-accumulated-data ()
  "Read the accumulated data."
  (if (file-exists-p myfitnesspal-accumulated-data-file)
      (save-window-excursion
	(find-file myfitnesspal-accumulated-data-file)
	(save-excursion
	  (goto-char (point-min))
	  (read (current-buffer))))
    nil))

(defun myfitnesspal-write-accumulated-data (data)
  "Write the accumulated data."
  (save-window-excursion
    (find-file myfitnesspal-accumulated-data-file)
    (save-excursion
      (erase-buffer)
      (insert "(")
      (dolist (entry data)
	(insert (format "%S\n" data)))
      (insert ")\n")
      (basic-save-buffer))))

(defun myfitnesspal-work-backwards (starting-back)
  "Work through a series of report pages."
  (interactive "nStart how many days ago: ")
  (let ((accumulated (myfitnesspal-read-accumulated-data))
	(page-file-name nil)
	(another t)
	(back starting-back))
    (while another
      (myfitnesspal-fetch-days-ago back)
      (setq page-file-name (read-file-name "File of saved web page: "
					   nil page-file-name))
      (push (myfitnesspal-parse-file page-file-name) accumulated)
      (setq another (y-or-n-p "Fetch another page? ")
	    back (1+ back)))
    (myfitnesspal-write-accumulated-data accumulated)
    (message "Finished %d days ago" back)))
    
