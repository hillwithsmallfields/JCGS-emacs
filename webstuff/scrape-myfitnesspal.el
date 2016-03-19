;;; Scrape data from myfitnesspal.com food diary pages
;;; Time-stamp: <2016-03-19 20:50:10 jcgs>
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

(defun myfitnesspal-parse-file (file)
  "Parse a myfitnesspal FILE.
The result is an alist element of meal data."
  (save-window-excursion
    (find-file file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "// </food/diary\\?date=\\(....\\)-\\(..\\)-\\(..\\)>")
      (let* ((year (string-to-number (match-string-no-properties 1)))
	     (month (string-to-number (match-string-no-properties 2)))
	     (yesterday-day (string-to-number (match-string-no-properties 3)))
	     (yesterday-time (encode-time 1 1 1 yesterday-day month year))
	     (today-time (time-add yesterday-time (days-to-time 1)))
	     (meals nil))
	(while (search-forward "Add Food" (point-max) t)
	  (save-excursion
	    (re-search-forward "^\t[0-9,\t ]+$" (point-max) t)
	    (let* ((meal-data (convert-to-numbers 
			       (split-string (match-string-no-properties 0) "\\s-+" t))))
	      (re-search-backward "Breakfast\\|Lunch\\|Supper\\|Snacks")
	      (push (cons (match-string-no-properties 0) meal-data)
		    meals))))
	(re-search-forward  "^Totals\\s-+\\([0-9,\t ]+\\)$")
	(push (cons "Totals" (convert-to-numbers
			      (split-string (match-string-no-properties 1) "\\s-+" t)))
	      meals)
	(cons today-time meals)))))

(defvar myfitnesspal-accumulated-data-file "~/myfitnesspal-data-el"
  "File containing accumulated scrapes.")

(defun myfitnesspal-read-accumulated-data ()
  "Read the accumulated data."
  (save-window-excursion
    (find-file myfitnesspal-accumulated-data-file)
    (save-excursion
      (goto-char (point-min))
      (read (current-buffer)))))

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

(defun myfitnesspal-work-backwards ()
  "Work through a series of report pages."
  (let ((accumulated (myfitnesspal-read-accumulated-data))
	(page-file-name nil)
	(another t)
	(back 0))
    (while another
      (myfitnesspal-fetch-days-ago back)
      (unless page-file-name
	(setq page-file-name (read-file-name "File of saved web page: ")))
      (push (myfitnesspal-parse-file page-file-name) accumulated)
      (setq another (y-or-n-p "Fetch another page? ")
	    back (1+ back)))
    (myfitnesspal-write-accumulated-data accumulated)))
    
