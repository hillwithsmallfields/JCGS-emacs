;;;; journal-html-to-org.el --- convert my old HTML journals to org-mode
;;; Time-stamp: <2016-07-16 22:18:15 jcgs>

(require 'journal)
(require 'replace-regexp-list)

(defun journal-html-to-org-date (html-date)
  "Convert HTML-DATE to an org journal date."
  (let ((match (string-match "\\([0-9]\\{4\\}\\)-\\([a-z]\\{3\\}\\)-\\([0-9]\\{2\\}\\)" html-date)))
    (if match
	(let* ((year (string-to-number (match-string-no-properties 1 html-date)))
	       (month-name (match-string-no-properties 2 html-date))
	       (month (cdr (assoc month-name journal-monthname-alist)))
	       (day (string-to-number (match-string-no-properties 3 html-date)))
	       (encoded (encode-time 0 0 0 day month year)))
	  (format-time-string "%F %A" encoded)))))

(defvar journal-html-to-org-edits
  '(("<h2>.+\\[\\([-0-9a-z]+\\).+</h2>" . "*** \\,(journal-html-to-org-date \\1)")
    ("<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>" . "[people:\\1|\\2]")
    ("</?p>" . "")
    )
  "Edits to make to convert a journal file.")

(defun journal-html-to-org-add-file (org-file html-file)
  "Into ORG-FILE add HTML-FILE.
It always adds the new file at the end."
  (interactive "FOrg file: \nfHTML file: \n")
  (find-file org-file)
  (goto-char (point-max))
  (let ((top-of-new-text (point)))
    (insert-file-contents html-file)
    (goto-char top-of-new-text)
    (search-forward "<body>")
    (delete-region top-of-new-text (point))
    (search-forward "<hr>")
    (delete-region (match-beginning 0) (point-max))
    (replace-regexp-alist journal-html-to-org-edits
			  top-of-new-text (point-max) nil nil t)))

(defun file-name-last-directory (directory)
  "Return the last directory part of DIRECTORY."
  (if (string-match ".*/\\([^/]+\\)/.*" directory)
      (match-string 1 directory)
    directory))

(defun journal-html-to-org-directory (directory)
  "Make up an org version of the journal files in DIRECTORY."
  (interactive "DConvert journal in directory: ")
  (let* ((dir-dirname (file-name-last-directory directory))
	 (org-filename (expand-file-name (concat dir-dirname ".org") directory))
	 (html-files (directory-files directory t "[0-9][0-9]-[0-9][0-9]\\.html")))
    (find-file org-filename)
    (erase-buffer)
    (dolist (html-file html-files)
      (journal-html-to-org-add-file org-filename html-file))
    (basic-save-buffer)))

(provide 'journal-html-to-org)
