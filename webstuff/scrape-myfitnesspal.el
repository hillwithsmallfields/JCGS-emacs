;;; Scrape data from myfitnesspal.com food diary pages
;;; Time-stamp: <2016-04-01 22:22:33 jcgs>
;;; The resulting data should convert easily to CSV etc.
;;; Started by John Sturdy <jcg.sturdy@gmail.com> on 2016-03-18

;; sample url: http://www.myfitnesspal.com/food/diary?date=2016-03-17

(defun myfitnesspal-fetch-days-ago (days)
  "Fetch the myfitnesspal food diary for DAYS ago."
  (interactive "nGet data for days ago: ")
  (let ((calculated-date ))
    (message "Calculated date is %s" (current-time-string calculated-date))
    (myfitnesspal-fetch calculated-date)))

(defvar latest-year nil
  "The latest year read, as a number.")

(defvar latest-month nil
  "The latest month read, as a number.")

(defvar latest-day nil
  "The latest day read, as a number.")

(let ((now (decode-time)))
  (setq latest-year (nth 5 now)
	latest-month (nth 4 now)
	latest-day (nth 3 now)))

(defun read-date (prompt)
  "Read a date from the user, using the minibuffer."
  (setq latest-year (read-from-minibuffer (format "%s (year): " prompt)
						 (number-to-string latest-year) nil t)
	latest-month (read-from-minibuffer (format "%s (month in %d): " prompt latest-year)
						  (number-to-string latest-month) nil t)
	latest-day (read-from-minibuffer (format "%s (day in %s-%s): " prompt latest-year latest-month)
						(number-to-string latest-day) nil t))
  (encode-time 0 0 0 latest-day latest-month latest-year))

(defun myfitnesspal-fetch (date)
  "Fetch the myfitnesspal food diary for DATE."
  (interactive (list (read-date "Fetch food diary for")))
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
      (re-search-forward "</food/diary\\?date=\\(....\\)-\\(..\\)-\\(..\\)>")
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

(defvar myfitnesspal-accumulated-data nil
  "Alist containing accumulated scrapes.")

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

(defvar latest-page-file-name nil
  "The latest filename used in scraping data.")

(defun myfitnesspal-process-one-day (date)
  "Fetch, analyze and store the data for DATE."
  (interactive (list (read-date "Process food diary for")))
  (myfitnesspal-fetch date)
  (setq latest-page-file-name (read-file-name "File of saved web page: "
				       nil latest-page-file-name))
  (push (myfitnesspal-parse-file latest-page-file-name) myfitnesspal-accumulated-data))

(defun myfitnesspal-work-backwards (starting-back)
  "Work through a series of report pages."
  (interactive "nStart how many days ago: ")
  (let ((another t)
	(back starting-back))
    (setq myfitnesspal-accumulated-data (myfitnesspal-read-accumulated-data)
	  latest-page-file-name nil)
    (while another
      (myfitnesspal-process-one-day
       (time-add (current-time) (days-to-time (- days))))
      (setq another (y-or-n-p "Fetch another page? ")
	    back (1+ back)))
    (myfitnesspal-write-accumulated-data myfitnesspal-accumulated-data)
    (message "Finished %d days ago" back)))
    
(defun myfitnesspal-fetch-most-recent-unfetched ()
  "Fetch the most recent page that isn't in the database."
  (interactive)
  (setq myfitnesspal-accumulated-data
	(sort myfitnesspal-accumulated-data (function
					     (lambda (a b)
					       (if (= (car a) (car b))
						   (< (cadr a) (cadr b))
						 (< (car a) (car b)))))))
  )
