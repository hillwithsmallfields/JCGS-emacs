;;;; isbn-lookup.el
;;; Initially written 2014-05-10 by JCG Sturdy
;;; Time-stamp: <2015-02-03 23:01:46 jcgs>

(defun isbn-lookup (isbn &optional raw)
  "Look up ISBN.
Process the result unless optional RAW is given."
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
		(unless raw
		  (setq result (mapcar 'isbn-normalize-pair result)))
		(if (interactive-p)
		    (with-output-to-temp-buffer "*ISBN info*"
		      (dolist (pair result)
			(princ (format "%s: %s\n" (car pair) (cdr pair))))))
		result)
	    (error "No end to bookinfo")))
      (error "No start to bookinfo"))))

(defun isbn-normalize-pair (pair)
  "Normalize PAIR.
This will convert dates to iso format, and put the conventional hyphens into ISBNs."
  (let ((fn (get (intern (car pair)) 'isbn-normalizer-function)))
    (if fn
	(funcall fn pair)
      pair)))

(defmacro def-isbn-normalizer (key fn)
  "Define that KEY is to be normalized using FN."
  `(put ',(intern key) 'isbn-normalizer-function ',fn))

;; (def-isbn-normalizer "ISBN-10" (lambda (pair) pair))
;; (def-isbn-normalizer "ISBN-13" (lambda (pair) pair))

(def-isbn-normalizer "Published" (lambda (pair)
				   (let* ((date  (cdr pair))
					  (matched (string-match "\\([A-Z][a-z]+\\) \\([0-9]+\\)" date))
					  (month (1+ (position (match-string-no-properties 1 date)
							       calendar-month-name-array
							       :test 'string=)))
					  (year (match-string-no-properties 2 date)))
				     (cons (car pair)
					   (format "%s-%02d" year month)))))


(defun org-table-headings ()
  "Get the headings of a table, as a vector."
  (save-excursion
    (let ((headings (progn
		      (goto-char (org-table-begin))
		      (beginning-of-line 2)
		      (let ((accu nil))
			(while (re-search-forward "|\\s-*\\([^|]+?\\)\\s-*|" (point-at-eol) t)
			  (push (match-string-no-properties 1) accu)
			  (backward-char 1))
			(apply 'vector (nreverse accu))))))
      (put-text-property (org-table-begin) (org-table-end)
			 'org-table-headings
			 headings)
      headings)))

(defun org-table-column-number (column-name)
  "Return the column number for COLUMN-NAME."
  (1+ (position column-name
		(or (get-text-property (point) 'org-table-headings)
		    (org-table-headings))
		:test 'string=)))

(defun org-table-goto-column-by-name (name)
  "Go to the column named NAME."
  (interactive
   (list
    (completing-read "Column name: "
		     (map 'list 'identity (org-table-headings))
		     nil t)))
  (org-table-goto-column (org-table-column-number name)))

(defun org-table-get-field-by-name (name &optional replace)
  "Return the value of the field named NAME in the current row.
With optional argument REPLACE, replace the field with this value.
The return value is always the old value."
  (let ((str (org-table-get-field (org-table-column-number name)
				  replace)))
    (set-text-properties 0 (length str) nil str)
    ;; todo: trim whitespace from string
    str))

;; todo: get details of a row, and fill in missing fields

;;; isbn-lookup.el ends here
