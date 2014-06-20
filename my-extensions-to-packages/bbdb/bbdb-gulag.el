;;;; bbdb-gulag.el -- keep inactive records elsewhere and bring back as needed
;;; Time-stamp: <2003-04-28 21:52:45 jcgs>
;;; started 2002-09-10

;;; maybe rename to bbdb-gulag
(provide 'bbdb-gulag)

(defvar bbdb-gulag:gulag-file "~/.bbdb-gulag"
  "Where the gulag data is kept.")

(defvar bbdb-gulag:latest-created-record nil
  "The most recent record passed to the bbdb-create-hooks.")

(defun bbdb-gulag:recreation-hook-function (record)
  "Function to go onto bbdb-create-hook, that helps to try to retrieve passivated records.
It doesn't try to do it here, as the name and so on have not yet been filled in, but simply
marks it for the attention of a change hook that will pick it up shortly. See
bbdb-gulag:recreation-fill-in-hook-function for details thereof."
  (setq bbdb-gulag:latest-created-record record))

(add-hook 'bbdb-create-hook 'bbdb-gulag:recreation-hook-function)

(defun bbdb-gulag:get-record (string)
  "Return a gulag record matching STRING."
  (let ((found (find-buffer-visiting bbdb-gulag:gulag-file)))
    (save-window-excursion
      (find-file bbdb-gulag:gulag-file)
      (save-excursion
	(goto-char (point-min))
	(if (search-forward string (point-max) t)
	    (progn
	      (beginning-of-line 1)
	      (prog1
		  (read (current-buffer))
		(if found
		    (bury-buffer (current-buffer))
		  (kill-buffer (current-buffer)))))
	  nil)))))

(defun bbdb-gulag:best-of (a b)
  "Choose one of A or B according to which is probably better data."
  (cond
   ((and (null a) (null b)) a)
   ((and (null a) b) b)
   ((and a (null b) a))
   ((or (and (listp a) (listp b)) (and (stringp a) (stringp b)))
    (if (> (length b) (length a))
	b
      a))
   (t a)))

(defun bbdb-gulag:recreation-fill-in-hook-function (record)
  "Function to go onto bbdb-change-hook, that tries to retrieve passivated records."
  (let ((net (car (bbdb-record-net record))))
    (when (and (eq record bbdb-gulag:latest-created-record) (stringp net))
      (setq bbdb-gulag:latest-created-record nil)
      (let ((gulag-data (bbdb-gulag:get-record net)))
	;; (message "Got gulag data %S" gulag-data)
	(when gulag-data
	  (dotimes (slot (length gulag-data))
	    (aset record slot
		  (bbdb-gulag:best-of (aref gulag-data slot)
					  (aref record slot))))
	  (bbdb-change-record record t))))))

(add-hook 'bbdb-change-hook 'bbdb-gulag:recreation-fill-in-hook-function)

(defun bbdb-gulag:ensure-is-in-gulag (record)
  "Ensure RECORD is in the gulag store; useful before deleting it from the main DB."
  (save-window-excursion
    (find-file bbdb-gulag:gulag-file)
    (let ((print-escape-newlines t)
	  (net (car (bbdb-record-net record)))
	  (cache (bbdb-record-cache record)))
      (when (stringp net)
	(bbdb-record-set-cache record nil)
	(save-excursion
	  (goto-char (point-min))
	  (if (search-forward net (point-max) t)
	      (progn
		(beginning-of-line 1)
		(let ((start (point)))
		  (beginning-of-line 2)
		  (delete-region start (point))
		  (insert (prin1-to-string record) "\n")))
	    (progn
	      (goto-char (point-max))
	      (search-backward ")")
	      (insert (prin1-to-string record) "\n"))))
	(bbdb-record-set-cache record cache)))
    (bury-buffer (current-buffer))))

(defun bbdb-exile-to-gulag (record)
  "Exile RECORD to the gulag database."
  (bbdb-gulag:ensure-is-in-gulag record)
  (bbdb-delete-record-internal record))

(defun bbdb-record-contains-special-fields (record)
  "Return whether RECORD contains any of my special fields"
  (catch 'found
      (dolist (field '(phone-list distlists christmas-card societies))
	(when (bbdb-record-getprop record field)
	  (throw 'found t)))
      nil))

(defun bbdb-keep-only-annotated-records ()
  "Throw out all records that I haven't particularly annotated"
  (interactive)
  (require 'bbdb-iterators)
  (let ((potential-unpersons nil))
    (bbdb:apply-to-records
     (function (lambda (record)
		 (unless (bbdb-record-contains-special-fields record)
		   (message "Putting %S on purge list" record)
		   (push record potential-unpersons)))))
    (message "%d unannotated" (length potential-unpersons))
    (dolist (record potential-unpersons)
      (message "Exiling %S" record)
      (bbdb-exile-to-gulag record))))

(defun bbdb-retrieve-exiles-matching (pattern)
  "Retrieve entries from the gulag database that match PATTERN.
They are reinstated as normal entries."
  (interactive "sRetrieve exiles matching pattern: ")
  (let ((returnees nil))
    (save-window-excursion
      (find-file bbdb-gulag:gulag-file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward pattern (point-max) t)
	  (beginning-of-line 1)
	  (push (read (current-buffer))
		returnees))))
    (dolist (returnee returnees)
      (let ((email (car (aref returnee 6)))
	    ;; also get the name into a useful form to search, and use for constructing returning real entry!!!!!!!!!!!!!!!!
	    )
	(if (stringp email)
	    (let ((record (bbdb-search-simple nil email)))
	      (if record
		  (message "Already got %S" email)
		(let ()
		  ;; create and fill in record here -- use email and name
		  )))
	  (message "Could not locate %S for retrieval, because of missing net address" email))))))

