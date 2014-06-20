;;;; I don't yet know why, but it seems to have dropped some info I like

(defun put-person-on-phone-list (person phonelist)
  "Put PERSON onto PHONELIST."
  (interactive "sPerson: 
sPhone list: ")
  (let ((record (bbdb-search-simple person nil)))
    (bbdb:adjoin-to-field record 'phone-list phonelist)
    (bbdb-change-record record nil)))

(defun put-people-on-phone-list (people phonelist)
  "Put PEOPLE onto PHONELIST."
  (dolist (person people)
    (put-person-on-phone-list person phonelist)))

(defun put-person-on-card-list (person)
  "Put PERSON on my Christmas card list."
  (let ((record (bbdb-search-simple person nil)))
    (bbdb:adjoin-to-field record 'christmas-card "yes")
    (bbdb-change-record record nil)))

(defun read-old-bbdb ()
  (interactive)
  (find-file "~/.bbdb-cb1")
  (goto-char (point-min))
  (beginning-of-line 5)
  (let* ((old-bbdb (read (current-buffer))))
    (message "%d entries" (length old-bbdb))
    (dolist (old-entry old-bbdb)
      (let* ((name (concat (aref old-entry 0) " " (aref old-entry 1)))
	     (old-details (aref old-entry 7)))
	;; (message "Processing %s" name)
	(when old-details
	  ;; (message "Got old details")
	  (let* ((new-entry (bbdb-search-simple name nil))
		 (new-details (and new-entry (aref new-entry 7))))
	    (when (and old-details (not (equal old-details new-details)))
	      ;; (message "Changing details")
	      (if new-details
		  (message "Conflict: %s %s %s" name old-details new-details))
	      (when new-entry
		;; (message "setting new entry to old details")
		(aset new-entry 7 old-details)
		;; (message "Record is now %s" new-entry)
		(bbdb-change-record1 new-entry nil)))))))))

(defun bbdb-change-record1 (record need-to-sort)
  "Update the database after a change to the given record.  Second arg 
NEED-TO-SORT is whether the name has changed.  You still need to worry 
about updating the name hash-table."
  ;; (message "In bbdb-change-record1 %s" (aref record 1))
  (if inside-bbdb-change-record
      record
    (let ((inside-bbdb-change-record t)
	  unmigrated)
      (bbdb-invoke-hook 'bbdb-change-hook record)
      ;; (message "done hook")
      (bbdb-debug (if (bbdb-record-deleted-p record)
		      (error "bbdb: changing deleted record")))
      (if (/= (cdr bbdb-file-format-migration) bbdb-file-format)
	  (progn
	    ;; (message "unmigrate")
	     (bbdb-unmigrate-record (setq unmigrated (bbdb-copy-thing record)))
	     ;; (message "made unmigrated = %s" unmigrated)
	     ))
      ;; Do the changing
      ;; (message "Do the changing")
      (if (memq record (bbdb-records))	; checks file synchronization too.
	  (progn
	    ;; (message "in bbdb-records already")
	    (if (not need-to-sort);; If we don't need to sort, overwrite it.
		(progn
		  ;; (message "not sorting; record is %s" record)
		  (bbdb-overwrite-record-internal1 record unmigrated)   
		  (bbdb-debug
		   (if (not (memq record (bbdb-records)))
		       (error "Overwrite in change doesn't work"))))
	      ;; Since we do need to sort, delete then insert
	      (message "sorting so delete then insert")
	      (bbdb-delete-record-internal record)
	      (bbdb-debug
	       (if (memq record (bbdb-records))
		   (error "Delete in need-sort change doesn't work")))
	      (bbdb-insert-record-internal record unmigrated)
	      (bbdb-debug
	       (if (not (memq record (bbdb-records)))
		   (error "Insert in need-sort change doesn't work")))))
	;; Record isn't in database so add it.
	(message "Record isn't in database so add it.")
	(bbdb-insert-record-internal record unmigrated)
	(bbdb-debug (if (not (memq record (bbdb-records)))
			(error "Insert in change doesn't work"))))
      (setq bbdb-modified-p t)
      (bbdb-invoke-hook 'bbdb-after-change-hook record)
      record)))

(defun bbdb-overwrite-record-internal1 (record unmigrated)
  (bbdb-with-db-buffer
    (if (memq record bbdb-changed-records) nil
	(setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((print-escape-newlines t)
	  (tail bbdb-records))
      ;; Look for record after RECORD in the database.  Use the
      ;; beginning marker of this record (or the marker for the end of
      ;; the database if no next record) to determine where to stop
      ;; deleting old copy of record
      (while (and tail (not (eq record (car tail))))
	(setq tail (cdr tail)))
      (if (null tail) (error "bbdb: unfound %s" record))
      (let ((cache (bbdb-record-cache record)))
	(bbdb-debug
	 (if (<= (bbdb-cache-marker cache) (point-min))
	     (error "doubleplus ungood: cache marker is %s"
		    (bbdb-cache-marker cache)))
	 (goto-char (bbdb-cache-marker cache))
	 (if (and (/= (point) bbdb-end-marker)
		  (not (looking-at "[\[]")))
	     (error "doubleplus ungood: not inserting before a record (%s)"
		    (point))))
	
	(goto-char (bbdb-cache-marker cache))
	(bbdb-record-set-cache record nil)
	;; (message "after set-cache, record is %s, unmigrated is %s" record unmigrated) ;  is OK here
	(if unmigrated (bbdb-record-set-cache unmigrated nil))

	(insert (prin1-to-string (or record)) "\n")
	(message "inserted %s" (prin1-to-string (or record)))
	(delete-region (point)
		       (if (cdr tail)
			   (bbdb-record-marker (car (cdr tail)))
			 bbdb-end-marker))
	(bbdb-record-set-cache record cache)

	(bbdb-debug
	 (if (<= (if (cdr tail)
		     (bbdb-record-marker (car (cdr tail)))
		   bbdb-end-marker)
		 (bbdb-record-marker record))
	     (error "doubleplus ungood: overwrite unworks")))

	(setq bbdb-modified-p t)
	record))))

(defun try-to-flush-to-file ()
  (bbdb:apply-to-records
   (function
    (lambda (record)
      (let* ((alist (aref record 7)))
	(when (listp alist)
	  
	  ))))))
