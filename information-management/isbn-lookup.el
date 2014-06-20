;;;; isbn-lookup.el
;;; Initially written 2014-05-10 by JCG Sturdy
;;; Time-stamp: < >

(defun isbn-lookup (isbn)
  "Look up ISBN."
  (interactive "sLook up ISBN: ")
  (let ((fetcher (get-buffer-create "*ISBN fetch*"))
	(result nil))
    (set-buffer fetcher)
    (erase-buffer)
    (shell-command
     (format "curl http://www.isbnsearch.org/isbn/%s" (delete ?- isbn))
     fetcher)
    ;; (display-buffer fetcher)
    (if (search-forward "<div class=\"bookinfo\">" (point-max) t)
	(let ((start (point)))
	  (if (search-forward "</div>" (point-max) t)
	      (let ((end (point)))
		(goto-char start)
		(if (re-search-forward "<h2>\\(.+\\)</h2>" end t)
		    (let ((title-line (match-string-no-properties 1)))
		      (cond
		       ((string-match "\\(.+?\\)\\s-*(\\([^)]+\\))\\s-*(\\(.+\\))" title-line)
			(let ((title (match-string-no-properties 1 title-line))
			      (subtitle (match-string-no-properties 2 title-line))
			      (subsubtitle (match-string-no-properties 3 title-line)))
			  (push (cons 'Title title)
				result)
			  (push (cons 'Sub-Title subtitle)
				result)
			  (push (cons 'Sub-Sub-Title subsubtitle)
				result)))
		       ((string-match "\\(.+?\\)\\s-*(\\([^)]+\\))" title-line)
			(let ((title (match-string-no-properties 1 title-line))
			      (subtitle (match-string-no-properties 2 title-line)))
			  (push (cons 'Title title)
				result)
			  (push (cons 'Sub-Title subtitle)
				result)))
		       (t (push (cons 'Title title-line)
				result))))
		  (error "No title line"))
		(while (re-search-forward "<p><strong>\\([-A-Za-z0-9]+\\):</strong>\\s-+\\(.+\\)</p>" end t)
		  (let ((key (intern (match-string-no-properties 1)))
			(value (match-string-no-properties 2)))
		    (when (string-match "\\(.*\\)<[^>]+>\\(.+\\)</a>\\(.*\\)" value)
		      (setq value (concat (match-string 1 value)
					  (match-string 2 value)
					  (match-string 3 value))))
		    (push (cons key value)
			  result)))
		(if (interactive-p)
		    (with-output-to-temp-buffer "*ISBN info*"
		      (dolist (pair result)
			(princ (format "%s: %s\n" (car pair) (cdr pair))))))
		result)
	    (error "No end to bookinfo")))
      (error "No start to bookinfo"))))

(defun org-table-headings ()
  "Get the headings of a table."
  (save-excursion
    (goto-char (org-table-begin))
    ;; todo: return an alist (or vector, if easily searchable) of column titles to column numbers
    ))

;; todo: write org-table-get-field-by-name, with equivalent spec to org-table-get-field but taking a name

;;; isbn-lookup.el ends here
