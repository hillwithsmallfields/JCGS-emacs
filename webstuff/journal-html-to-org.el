;;;; journal-html-to-org.el --- convert my old HTML journals to org-mode
;;; Time-stamp: <2016-07-18 21:26:42 jcgs>

(require 'journal)
(require 'replace-regexp-list)

(defvar journal-html-html-date-regexp
    "\\([0-9]\\{4\\}\\)-\\([a-z]\\{3\\}\\)-\\([0-9]\\{2\\}\\)"
  "Regexp for dates.")

(defvar journal-html-html-date-header-regexp
  (concat "<h2>.+\\[\\(" journal-html-html-date-regexp "\\)\\].+</h2>")
  "Regexp for date headers.")

(defun journal-html-to-org-date (html-date)
  "Convert HTML-DATE to an org journal date."
  (let ((match (string-match journal-html-html-date-regexp html-date)))
    (if match
	(let* ((year (string-to-number (match-string-no-properties 1 html-date)))
	       (month-name (match-string-no-properties 2 html-date))
	       (month (cdr (assoc month-name journal-monthname-alist)))
	       (day (string-to-number (match-string-no-properties 3 html-date)))
	       (encoded (encode-time 0 0 0 day month year)))
	  (list year month day)))))

(org-add-link-type "journal-people" 'journal-people-open)
(add-hook 'org-store-link-functions 'journal-people-store-link)

(defun journal-people-open (path)
  "Open the people entry at PATH."
  ;; todo: complete this
  )

(defun journal-people-store-link ()
  "Store a people link."
  (let ((link "")			; todo: complete
	(description ""))		; todo: complete
    (org-store-link-props
     :type "journal-people"
     :link link
     :description description)))

(org-add-link-type "journal-date" 'journal-date-open)
(add-hook 'org-store-link-functions 'journal-date-store-link)

(defun journal-date-open (path)
  "Open the date entry at PATH."
  ;; todo: complete this
  )

(defun journal-date-store-link ()
  "Store a date link."
  (let ((link "")			; todo: complete
	(description ""))		; todo: complete
    (org-store-link-props
     :type "journal-date"
     :link link
     :description description)))

(defvar journal-html-to-org-edits
  '(("<a href=\"../../people/\\([^\"]+\\)\">\\([^<]+\\)</a>" . "[[journal-people:\\1][\\2]]")
    ("<a href=\"\\([0-9][0-9]-[0-9][0-9]\\).html#\\([0-9][0-9]\\)\">\\([^<]+\\)</a>" . "[[journal-date:\\1-\\2][\\3]]")
    ("<a href=\"../[0-9][0-9][0-9][0-9]/\\([0-9][0-9]-[0-9][0-9]\\).html#\\([0-9][0-9]\\)\">\\([^<]+\\)</a>" . "[[journal-date:\\1-\\2][\\3]]")
    )
  "Edits to make to convert a journal file.")

(defun journal-html-get-file-entries (file)
  "Return the journal entries in FILE."
  (save-excursion
    (switch-to-buffer (get-buffer-create " *Conversion*"))
    (erase-buffer)
    (insert-file-contents file)
    ;; (replace-regexp-alist journal-html-to-org-edits)
    (goto-char (point-max))
    (unless (and (re-search-backward "^.*<hr>" (point-min) t)
		 (> (point)
		    (- (point-max) 400)))
      (error "Could not find realistic end of journal"))
    (let ((entry-end (point))
	  (entries nil))
      (while (re-search-backward journal-html-html-date-header-regexp
				 (point-min) t)
	(message "Found one starting at %d" (point))
	(let* ((raw-date (match-string-no-properties 1))
	       (entry-start (line-end-position))
	       (date (journal-html-to-org-date raw-date)))
	  (message "Got %S at %d" date entry-start)
	  (goto-char entry-end)
	  (let ((paragraphs nil))
	    (while (search-backward "</p>" entry-start t)
	      (let ((para-end (point)))
		(unless (search-backward "<p>" entry-start t)
		  (error "Could not find start of paragraph ending at %d"
			 para-end))
		(let ((raw-para-text (buffer-substring-no-properties
				      (+ (point) 3)
				      para-end)))
		  (subst-char-in-string ?\n 32 raw-para-text t)
		  (push raw-para-text paragraphs))))
	    (push (cons date paragraphs)
		  entries))
	  (goto-char entry-start)
	  (beginning-of-line 1)
	  (setq entry-end (point))
	  (message "resuming search from %d" (point))))
      (bury-buffer)
      entries)))

(defun journal-html-to-org-test (input-file)
  "Test the reader.
Argument INPUT-FILE is the input file."
  (interactive "fInput file: ")
  (let ((entries (journal-html-get-file-entries input-file)))
    (with-output-to-temp-buffer "*Entries*"
      (dolist (entry entries)
	(princ (format "\n\n%S:\n" (car entry)))
	(dolist (para (cdr entry))
	  (princ (format "    %s\n\n" para)))))
    (find-file "/tmp/journal.org")
    (journal-html-to-org-add-entries entries)))

(defun journal-html-to-org-add-entries (entries)
  "Add ENTRIES to the current file."
  (dolist (entry entries)
    (apply 'work-log-open-date (car entry))
    (insert "\n")
    (dolist (para (cdr entry))
      (insert "   " para "\n\n"))))

(defun journal-html-to-org-add-file (org-file html-file)
  "Into ORG-FILE add HTML-FILE.
It always adds the new file at the end."
  (interactive "FOrg file: \nfHTML file: \n")
  (find-file org-file)
  (goto-char (point-max))
  (journal-html-to-org-add-entries
   (journal-html-get-file-entries html-file))
  (replace-regexp-alist journal-html-to-org-edits)
  (fill-individual-paragraphs (point-min) (point-max)))

(defun file-name-last-directory (directory)
  "Return the last directory part of DIRECTORY."
  (if (string-match ".*/\\([^/]+\\)/.*" directory)
      (match-string 1 directory)
    directory))

(defun journal-html-to-org-directory (directory)
  "Make up an org version of the journal files in DIRECTORY."
  (interactive "DConvert journal in directory: ")
  (let* ((dir-dirname (file-name-last-directory directory))
	 (output-directory (expand-file-name ".." directory))
	 (org-filename (expand-file-name (concat dir-dirname ".org") output-directory))
	 (html-files (directory-files directory t "[0-9][0-9]-[0-9][0-9]\\.html")))
    (find-file org-filename)
    (erase-buffer)
    (journal-html-to-org-add-entries
     (apply 'concatenate 'list
	    (mapcar 'journal-html-get-file-entries
		    html-files)))
    (replace-regexp-alist journal-html-to-org-edits)
    (fill-individual-paragraphs (point-min) (point-max))
    (basic-save-buffer)))

(defun journal-html-to-org-subdirectories (directory)
  "Make up org versions of subdirectories of DIRECTORY."
  (interactive "DConvert journal in subdirectories of: ")
  (mapcar 'journal-html-to-org-directory
	  (directory-files directory t
			   "[0-9]\\{4\\}")))

(provide 'journal-html-to-org)
