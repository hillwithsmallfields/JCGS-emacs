;;;; journal-from-text.el -- convert plain-text journalling into html journal format
;;; Time-stamp: <2018-11-15 19:25:21 jcgs>

(provide 'journal-from-text)
(require 'journal)
(require 'convert-special-characters)

;;;###autoload
(defun journal-from-text-file (file journal)
  "Read plain-text journal from FILE, and make proper entries from it into JOURNAL."
  (interactive
   (list
    (read-file-name "File to get journal from: ")
    (journal-choose-journal "Add entries to journal: ")))
  (let* ((now (decode-time))
	 (this-year (nth 5 now))
	 (this-month (nth 4 now))
	 (first-in-day t)
	 (case-fold-search t)
	 (journal-buffer nil)
	 (in-list nil)
	 (on-right nil))
    (save-window-excursion
      (find-file-read-only file)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (cond
	   ((looking-at "^\\([jfmasond][a-z]+ \\)?\\([0-9]*\\)\\([snrt][tdh]\\)?\\(, \\([0-9]+\\)\\)?:$")
	    (save-excursion
	      (let ((given-month-as-string (match-string 1))
		    (day (match-string 2))
		    (all-heading (match-string 0))
		    (given-year (match-string 5)))
		(message "Got heading \"%S\"" all-heading)
		(message "Given month %S, this-month=%S, given day %S" given-month-as-string this-month day)
		(message "Given year %S, assumed year %S" given-year this-year)
		(when given-year
		  (setq this-year (string-to-number given-year)))
		(when (stringp given-month-as-string)
		  (message "Got new month string %S" given-month-as-string)
		  (setq this-month (cdr (assoc (substring given-month-as-string 0 3) journal-monthname-alist)))
		  (message "Set month to %S" this-month))
		(journal-new-day journal
				 this-year
				 this-month (substring (aref journal-month-full-names this-month) 0 3)
				 (string-to-number day))
		(message "Adding to buffer %S and setting journal-buffer to it" (current-buffer))
		(setq journal-buffer (current-buffer)
		      first-in-day t))))
	   ((looking-at "^\\* *\\(.+\\)$")
	    (save-excursion
	      (let ((item-text (match-string-no-properties 1)))
		(message "Item text %s journal-buffer %S" item-text journal-buffer)
		(unless journal-buffer
		  (error "No output buffer set --- possibly no date header yet?"))
		(set-buffer journal-buffer)
		(unless in-list
		  (insert "\n\n<ul>\n")
		  (setq in-list t))
		(insert "  <li>" item-text "\n"))))
	   ((looking-at "^.+[a-z].+$")
	    (let ((para-text (match-string 0)))
	      (message "  Got text line %s" para-text)
	      (while (string-match "%[-a-z0-9]+%" para-text)
		(let ((leading (substring para-text 0 (match-beginning 0)))
		      (picture-name (substring para-text (1+ (match-beginning 0)) (1- (match-end 0))))
		      (trailing (substring para-text (match-end 0))))
		  (setq para-text (concat leading
					  "<img align=\""
					  (if on-right "right" "left")
					  "\" src=\"" picture-name ".jpg\">"
					  trailing)
			on-right (not on-right))))
	      (save-excursion
		(unless journal-buffer
		  (error "No output buffer set --- possibly no date header yet?"))
		(set-buffer journal-buffer)
		(when in-list
		  (insert "</ul>\n\n")
		  (setq in-list nil))
		(if first-in-day
		    (setq first-in-day nil)
		  (tempo-template-html-paragraph))
		(let ((start (point)))
		  (insert para-text)
		  (fill-paragraph nil)
		  (unless (search-forward "</p>" (point-max) t)
		    (end-of-line 1))
		  (insert "\n")
		  (save-excursion
		    (save-restriction
		      (narrow-to-region start (point))
		      (save-window-excursion
			(switch-to-buffer (current-buffer))
			(require 'convert-special-characters)
			(convert-special-characters 'html)
			(journal-people-references)
			(goto-char (point-min))
			(fill-paragraph nil)))))))))
	  (beginning-of-line 2))))))

;;; end of journal-from-text.el
