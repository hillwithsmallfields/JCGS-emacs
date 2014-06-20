;;; recent-changes.el -- handling updating of pages in web trees

(provide 'recent-changes)

;;; <title>Manage a list of locally edited pages</title>

(require 'cl)
(require 'page-attributes)

(defun eligible-for-recenting (filename)
  "Return whether FILENAME should be logged in recent-changes-file."
  (and (> (length filename) (length recent-changes-base-directory))
       (string= recent-changes-base-directory
		(standardize-pathname-delimiters
		 (substring filename 0 (length recent-changes-base-directory))))))

(defvar recent-changes-move-on-change t
  "*Whether to move recent changes to the insertion point on each change.")

(defvar recent-changes-added-here-string "Changes added here!"
  "Marker for adding or moving changes.")

(defun update-recent-changes-file (filename)
  "Update the recent changes file, which is named by recent-changes-file."
  (save-window-excursion
    (find-file recent-changes-file)
    (if (and
	 (or (not (boundp 'html-edit-quietly))
	     (not html-edit-quietly))
	 (eligible-for-recenting filename)
	 (not (string= (buffer-file-name) ; check it isn't the recent.html!
		       filename)))
	(save-excursion
	  (let* ((trimmed-name (standardize-pathname-delimiters
				(trimmed-name filename)))
		 (found-it (progn
			     (goto-char (point-min))
			     (search-forward trimmed-name (point-max) t))))
	    (if found-it
		(progn
		  (if recent-changes-move-on-change
		      (let ((eol (point-at-eol)))
			(beginning-of-line 1)
			(let ((old-change-string
			       (buffer-substring (point) eol)))
			  (delete-region (point) eol)
			  (delete-blank-lines)
			  (if (search-forward recent-changes-added-here-string
					      (point-max) t)
			      (progn
				(beginning-of-line 1)
				(insert old-change-string "\n")
				(beginning-of-line 0)
				))
			  )))
		  (let ((eol (point-at-eol)))
		    (beginning-of-line 1)
		    (if (re-search-forward "<strong>(\\(.\\))</strong>" eol t)
			(progn
			  (if (not (string= (match-string 1) "n"))
			      (replace-match "<strong>(e)</strong>")))
		      (progn
			(goto-char eol)
			(insert "<strong>(e)</strong>")))
		    (beginning-of-line 1)
		    (if (re-search-forward " *<i>(.+)</i>" eol t)
			(progn
			  (replace-match (format " <i>(%s)</i>"
						 (current-time-string))))
		      (progn
			(goto-char eol)
			(insert (format " <i>(%s)</i>"
					(current-time-string)))))))
	      (if (search-forward recent-changes-added-here-string
				  (point-max) t)
		  (progn
		    (beginning-of-line 1)
		    (insert "  <li> <a href=\""
			    trimmed-name
			    "\">"
			    trimmed-name
			    (format "</a> <strong>(e)</strong> <i>(%s)</i>\n" 
				    (current-time-string)))))))))
    ;; hmmm, my loop detection hasn't worked in this case!
    ;; (basic-save-buffer)
    (bury-buffer)
    ))

(defun make-recent-changes-transfer-script (pattern rcpscriptfile rcp-remote-prefix ftpscriptfile ftp-remote-directory)
  "Mark changed pages matching PATTERN for uploading via RCPSCRIPTFILE, staging at RCP-REMOTE-PREFIX, and FTPSCRIPTFILE, into FTP-REMOTE-DIRECTORY.
See update-recent-changes-file for more about this package."
  (interactive "sMark for upload pages matching regexp: 
Frcp script file for uploading pages matching %s: 
srcp remote prefix: 
Fftp script file for uploading pages matching %s: 
sftp remote directory: ")
  (save-window-excursion
    (let ((pages nil))
      (find-file recent-changes-file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward pattern (point-max) t)
	  (end-of-line 1)
	  (let ((eol (point)))
	    (beginning-of-line 1)
	    (if (re-search-forward "href=\"\\([^\"]+\\).+(\\(e\\))" eol t)
		(progn
		  (setq pages (cons (buffer-substring (match-beginning 1)
						      (match-end 1))
				    pages))
		  (delete-region (match-beginning 2) (match-end 2))
		  (goto-char (match-beginning 2))
		  (insert "p")
		  ))
	    (goto-char eol))))
      (basic-save-buffer)
      (let ((rcp-pages pages))
	(find-file rcpscriptfile)
	(erase-buffer)
	(insert "rcp " (trimmed-name ftpscriptfile) " "
		rcp-remote-prefix ftpscriptfile "\n")
	(while rcp-pages
	  (insert "rcp " (car rcp-pages) " " rcp-remote-prefix (car rcp-pages) "\n")
	  (setq rcp-pages (cdr rcp-pages)))
	(basic-save-buffer))
      (let ((ftp-pages pages))
	(find-file ftpscriptfile)
	(erase-buffer)
	(insert "cd " ftp-remote-directory "\nverbose\nhash\n")
	(while ftp-pages
	  (insert "put " (car ftp-pages) " " (car ftp-pages) "\n")
	  (setq ftp-pages (cdr ftp-pages)))
	(insert "quit\n")
	(basic-save-buffer)))))

(defun mark-transfers-as-done ()
  "Mark transfers, as scripted by make-recent-changes-transfer-script, as done."
  (interactive)
  (save-window-excursion
    (find-file recent-changes-file)
    (save-excursion
      (goto-char (point-min))
      (search-forward "<li>")
      (while (search-forward "(p)" (point-max) t)
	(replace-match "(t)"))
      (basic-save-buffer))))

(defun changed-pages-list ()
  "Return an alist of titles of recently changed pages, against filename and date changed."
  (save-window-excursion
    (find-file recent-changes-file)
    (save-excursion
      (goto-char (point-min))
      (let ((the-files nil))
	(while
	    (re-search-forward
	     "<li> <a href=\"\\([-a-zA-Z_/+]+\.html\\)\">\\([^<]+\\)</a> <strong>(.)</strong> <i>(\\([^)]+\\))</i>"
	     (point-max) t)
	  (push (list (match-string 2)
		      (expand-file-name (match-string 1) recent-changes-base-directory)
		      (match-string 3))
		the-files))
	(nreverse the-files)))))

(defun find-changed-pages (pattern)
  "Find all changed pages whose title matches PATTERN.
Report on any that did not exist."
  (interactive "sFind changed pages matching regexp: ")
  (with-output-to-temp-buffer "*Missing pages*"
    (let ((changed-pages (changed-pages-list)))
      (while changed-pages
	(let ((this-page (pop changed-pages)))
	  (when (string-match pattern (car this-page))
	    (let ((filename (cadr this-page)))
	      (unless (file-exists-p filename)
		(princ (format "\"%s\" in file %s -- last edited %s\n"
			       (car this-page) (cadr this-page) (caddr this-page))))
	      (find-file filename))))))))

;;; end of recent-changes.el
