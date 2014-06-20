;;;; grab data
;;; Time-stamp: <2004-11-01 11:46:30 john>

(defun grab-email-addresses-region (start end)
  "Find plausible email addresses in region"
  (interactive "r")
  (save-excursion
    (let ((pairs nil))
      (goto-char start)
      (while (re-search-forward "\"\\([^\"]+\\)\" +<\\([^@>]+@[^>]+\\)>" end t)
	(let* ((name (match-string 1))
	       (address (match-string 2)))
	  (if (string-match "\\(.+\\), \\(.+\\)" name)
	      (setq name (concat (substring name (match-beginning 2) (match-end 2))
				 " "
				 (substring name (match-beginning 1) (match-end 1)))))
	  (push (cons name address)
		pairs)))
      (if (interactive-p)
	  (message "Got %S" pairs))
      pairs)))

(defun bbdb:add-addresses-mentioned-in-message ()
  "Add the addresses mentioned in the message."
  (interactive)
  (let ((pairs (grab-email-addresses-region (point-min) (point-max)))
	(records nil)
	)
    (dolist (pair pairs)
      (let* ((name (car pair))
	     (address (cdr pair))
	     (record (bbdb-search (bbdb-records) name nil address)))
	(if record
	    (message "Already got %s <%s>" (car pair) (cdr pair))
	  (when (y-or-n-p (format "Create record for %s <%s> " (car pair) (cdr pair)))
	    (setq record (bbdb-create-internal name nil address nil nil nil))
	    (push record records)))))
    (bbdb-display-records records)))
