;;;; bbdb-iterators.el -- scan the names database
;;; started by John Sturdy, 1999-01-07
;;; Time-stamp: <2012-03-08 17:22:26 johnstu>

(provide 'bbdb-iterators)

(defun hack-fill-in-record (record)
  (let ((rname (bbdb-record-name record)))
  (if rname
      (progn
	(if (and (null (bbdb-record-addresses record))
		 (main-adr-seek rname))
	    (progn
	      (beginning-of-line 2)
	      (let ((start (point)))
		(re-search-forward "^$")
		(let ((rawstr (buffer-substring start (point))) (sit-for 2))
		  (if (> (length rawstr) 2)
		      (bbdb-record-set-addresses record (address-string-to-list rawstr)))))))
	(if (and (null (bbdb-record-phones record))
		 (main-adr-seek rname))
	    (progn
	      (let ((start (point)))
		(end-of-line 1)
		(let ((rawstr (buffer-substring start (point))))
		  (if (> (length rawstr) 2)
		      (bbdb-record-set-phones record
					      (phone-string-to-list
					       rawstr)))))))
	(bbdb-change-record record nil)
	)
    )))

;; (mapcar 'hack-fill-in-record (bbdb-records))

(defun bbdb:adjoin-to-field (record field value)
  "In RECORD, adjoin to FIELD the VALUE.
The caller of this should then call bbdb-change-record;
bbdb:apply-to-records does this for you."
  (let ((old-value (bbdb-record-getprop record field)))
    (if old-value
	(if (not (string-match value old-value))
	    (bbdb-record-putprop record
				 field
				 (concat old-value
					 "; "
					 value)))
      (bbdb-record-putprop record
			   field
			   value))))

(defun bbdb:remove-from-field (record field regexp)
  "In RECORD, remove from FIELD anything matching REGEXP.
The caller of this should then call bbdb-change-record;
bbdb:apply-to-records does this for you."
  (let ((previous-string (bbdb-record-getprop record field))
	x
	(modified-string nil)
	)
    (if previous-string
	(progn
	  (while (and
		  (not (equal modified-string previous-string))
		  (string-match regexp (if modified-string modified-string previous-string)))
	    (setq
	     x modified-string
	     modified-string (concat
			      (substring (if modified-string
					     modified-string
					   previous-string)
					 0 (match-beginning 0))
			      (substring (if modified-string
					     modified-string
					   previous-string)
					 (match-end 0)))
	     previous-string x
	     ))
	  (if modified-string
	      (bbdb-record-putprop record
				   field
				   modified-string))))))

(defun bbdb:split-from-notes (record regexp)
  "If the notes field of RECORD contains REGEXP, remove and return match."
  (let ((old-notes (bbdb-record-notes record)))
    (if (and old-notes (string-match regexp old-notes))
	(let* ((start-pos (match-beginning 0))
	       (end-pos (match-end 0))
	       (pre-match (substring old-notes 0 start-pos))
	       (match (substring old-notes start-pos end-pos))
	       (post-match (substring old-notes end-pos)))
	  (bbdb-record-set-notes record (concat pre-match post-match))
	  match)
      nil)))

(defun bbdb:move-note-to-field (record field regexp)
  "Move from the notes of RECORD into FIELD anything that matches REGEXP."
  (let ((old-notes 'foo)
	(new-notes 'bar)
	(value nil)
	)
    (while (and (setq value (bbdb:split-from-notes record regexp))
		(not (equal old-notes
			 (setq new-notes (bbdb-record-notes record)))))
      (bbdb:adjoin-to-field record field value)
      (setq old-notes new-notes))))

(defun bbdb:apply-to-records (fn)
  "Apply FN to bbdb records, handling database updates."
  (let* ((records (bbdb-records))
	 (i (length records)))
    (while records
      (let ((record (car records)))
	(when (funcall fn record)
	  (bbdb-change-record record nil)))
      (message "%d" i)
      (setq records (cdr records)
	    i (1- i)
	    )))
  (bbdb-save-db)
  nil)

;; (bbdb:apply-to-records '(lambda (rec) (bbdb:move-note-to-field rec 'societies "CUGCR")))

(defun bbdb:apply-to-records-defining (fn field)
  "Apply FN to bbdb records that have FIELD defined, handling database updates."
  (let* ((records (bbdb-records))
	 (i (length records)))
    (while records
      (let ((record (car records)))
	(when (bbdb-record-getprop record field)
	  (when (funcall fn record)
	    (bbdb-change-record record nil))))
      (message "%d" i)
      (setq records (cdr records)
	    i (1- i)
	    )))
  (bbdb-save-db)
  nil)

;;; end of bbdb-iterators.el
