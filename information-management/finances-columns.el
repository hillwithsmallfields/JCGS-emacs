;;;; finances-columns.el --- adjust the text I copy from my online bank statements
;;; Time-stamp: <2017-01-30 21:26:42 jcgs>

(defvar finances-columns-recent-transactions-regexp
  (concat "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)"
	  "\\s-+"
	  "\\(.+?\\)"
	  "[ 	 ]+"
	  "£\\([0-9]+\\.[0-9]+\\)"
	  ".*$")
  "Regexp for the raw data from my bank recent transactions.")

(defvar finances-columns-recent-transactions-template
  "\\3/\\2/\\1,\"\\4\",,,\\5"
  "Template for replacing column data.")

(defvar finances-columns-recent-transactions-with-balances-regexp
  (concat "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)"
	  "\\s-+"
	  "\\(.+?\\)"
	  "[ 	 ]+"
	  "£\\([0-9]+\\.[0-9]+\\)"
	  "\\s-+£"
	  "\\([0-9]+\\.[0-9]+\\)"
	  ".*$")
  "Regexp for the raw data from my bank recent transactions, keeping the balances.")

(defvar finances-columns-recent-transactions-with-balances-template
  "\\3/\\2/\\1,\"\\4\",,,\\5,\\6"
  "Template for replacing column data, keeping the balances.")

(defun finances-columns-tidy (&optional with-balances)
  "Tidy my bank statement columns.
With optional WITH-BALANCES, keep the balance column."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (if with-balances
		       finances-columns-recent-transactions-with-balances-regexp
		     finances-columns-recent-transactions-regexp))
	  (template (if with-balances
			finances-columns-recent-transactions-with-balances-template
		      finances-columns-recent-transactions-template))) 
      (while (re-search-forward pattern
				(point-max) t)
	(replace-match template)))))

(defconst finances-date-regexp "\\([0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\)"
  "Pattern for dates.")

(defun finances-split-by-days ()
  "Split my bank statement into daily stanzas."
  (interactive)
  (let ((this-line (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	(this-column (current-column)))
    (let ((last-old-split (save-excursion (goto-char (point-max))
					  (beginning-of-line 0)
					  (re-search-backward "^,*$"
							      (point-min)
							      t))))
      (message "Last old split at %d" last-old-split)
      (when last-old-split
	(goto-char last-old-split)))
    (beginning-of-line 2)
    (unless (looking-at finances-date-regexp)
      (error "No date on line after first split: %s"
	     (buffer-substring-no-properties (line-beginning-position)
					     (line-end-position))))
    (let ((prev-day (match-string-no-properties 1)))
      (message "First date %s" prev-day)
      (while (not (eobp))
	(beginning-of-line 2)
	(when (looking-at finances-date-regexp)
	  (let ((day (match-string-no-properties 1)))
	    (message "day %s prev-day %s" day prev-day)
	    (unless (string-equal day prev-day)
	      (save-excursion
		(insert ",\n"))
	      (setq prev-day day))))))
    (goto-char (point-min))
    (search-forward this-line)
    (move-to-column this-column)))

(defun bank-date-to-iso-date (bank-date)
  "Convert BANK-DATE to an ISO-8601 date."
  (let ((day (substring bank-date 0 2))
	(month-in (substring bank-date 3 6))
	(year (substring bank-date 7 11)))
    (format "%s-%02d-%s"
	    year
	    (1+ (position month-in calendar-month-abbrev-array :test 'equal))
	    day)))

(defun bank-spreadsheet-to-iso-dates (file)
  "Convert a bank spreadsheet to ISO dates.
Argument FILE is the file to convert."
  (interactive "fConvert dates in file: ")
  (find-file file)
  (let ((raw-date-regexp (concat "[0-3][0-9] \\("
				 (mapconcat 'identity
					    calendar-month-abbrev-array
					    "\\|")
				 "\\) [12][0-9][0-9][0-9]")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward raw-date-regexp (point-max) t)
	(replace-match (bank-date-to-iso-date (match-string-no-properties 0)))))))

(defvar finances-spreadsheet-mode-map
  (let ((map (make-sparse-keymap "Finances spreadsheet")))
    (define-key map "\C-c\C-h" 'finances-columns-tidy)
    (define-key map "\C-c\C-v" 'finances-split-by-days)
    (define-key map "\C-c\C-r" 'reverse-region)))

(define-derived-mode finances-spreadsheet-mode csv-mode "Finances spreadsheet"
  "Major mode for handling my finances transfer spreadsheet.")

;;; finances-columns ends here
