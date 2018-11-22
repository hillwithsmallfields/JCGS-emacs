;;;; journal-from-diary.el -- convert diary entries to journal entries
;;; Time-stamp: <2018-11-15 19:23:48 jcgs>

(provide 'journal-from-diary)

;;;###autoload
(defun journal-from-diary (year-string)
  "Convert the lines of the current file into journal entries, assuming YEAR-STRING."
  (interactive "sYear: ")
  (let ((year (string-to-number year-string)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\([A-Z][a-z][a-z]\\) \\([0-9][0-9]\\) \\([A-Z][a-z][a-z]\\)? \\([0-9][0-9]:[0-9][0-9]\\)? +\\(.+\\)$"
	      (point-max) t)
	(let* ((month-string (match-string-no-properties 1))
	       (month (cdr (assoc month-string journal-monthname-alist)))
	       (day-string (match-string-no-properties 2))
	       (day (string-to-number day-string))
	       (dayofweek (match-string-no-properties 3))
	       (time-string (match-string-no-properties 4))
	       (text (match-string-no-properties 5)))
	  (message "month=%S day=%S dayofweek=%S time=%S text=%s"
		   month-string day-string dayofweek time-string text)
	  (save-excursion
	    (journal-new-day "personal" year month month-string day)
	    (save-excursion
	      (insert text "."))
	    (capitalize-word 1)))))))

;;; end of journal-from-diary.el
