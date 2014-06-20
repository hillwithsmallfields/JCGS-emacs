;;; Time-stamp: <2005-02-20 20:51:06 jcgs>
;;; <title>Make status tables for directories</title>

(provide 'status-table)
(require 'page-status)
(require 'page-attributes)
(require 'webmaster-macros)

(defvar auto-table-start-string
  "<!-- start of automatically generated table -->"
  "Marker for start of automatically generated table (so the code can remove
the previous version automatically).")

(defvar auto-table-end-string
  "<!-- end of automatically generated table -->"
  "Marker for end of automatically generated table (so the code can remove
the previous version automatically).")

(defun auto-table-start (&optional tabular)
  "Remove old auto-table markers, and start new ones."
  (if (search-backward auto-table-start-string (point-min) t)
      (let ((start (point)))
	(if (search-forward auto-table-end-string (point-max) t)
	    (progn
	      (delete-region start (point))
	      (message "Deleted old table"))
	  (progn
	     (message "Could not find old table to delete")))))
  (insert auto-table-start-string
	  "\n<!-- prepared: " (current-time-string) "-->\n")
  (when tabular
	(insert "<center>\n<table border align=\"center\">"
		" <!-- links for " directory
		" at " (current-time-string)
		" -->\n")
	(insert "  <tr><th>Title</th>"
		"<th><code>filename"
		"(content/total size)</code></th>"
		"<th><i>status</i></th>"
		"<th>started</th>"
		"</tr>\n"))

  )

(defun auto-table-end (&optional tabular)
  "Complete new auto-table markers."
  (when tabular
    (insert "</table>\n</center>\n"))
  (insert "\n" auto-table-end-string))

(defun file-contents (file)
  "Return the contents of FILE, as a string."
  (save-window-excursion
    (find-file file)
    (prog1 (buffer-string)
      (kill-buffer nil))))

(defun page-unlisted (file)
  "Return whether a page claims to be unlisted"
  (save-window-excursion
    (find-file file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "<!-- *unlisted *-->" (point-max) t))))

(defvar indent-page-table nil
  "Whether to indent the table we produce.")

;;;###autoload
(defun link-make-directory (directory types &optional tabular part-of-larger-flat-list status-regexp)
  "Insert an unsorted list of the files in DIRECTORY that match TYPES.
Use optional TABULAR to make a status table instead of a list.
Unless optional PART-OF-LARGER-FLAT-LIST given, previous list around point is removed.
If optional STATUS-REGEXP given, only include those whose (find-page-status ..)
matches STATUS-REGEXP.
Any directory containing a file called sitemap-omit-directory is not descended.
Directories may give a neat title in a file called title.txt in the directory."
  (interactive "DInsert index for directory: 
sInsert index entries for file of type: ")
  ;; for now, we don't use recursive tables
  (let* ((html-helper-never-indent (not indent-page-table))
	 (relative-to webmaster:page-site-homepage-directory-name)
	 (rel-to-len (length relative-to)))
    (when tabular (setq part-of-larger-flat-list t))
    (unless part-of-larger-flat-list
      (html-helper-indent) (insert "<ul> <!-- links for " directory " -->\n"))
    (let ((rel-to-log nil)
	  (files (directory-files directory t)))
      (dolist (file files)
	(let* ((true-file (file-truename file))
	       ;; should this be relative to the current directory? but maybe something else is relative to this?
	       (rel-file (if (>= (length true-file) rel-to-len)
			     (substring true-file rel-to-len)
			   ".dot."))
	       (url (concat webmaster:page-site-homepage-url rel-file)))
	  (when nil (html-helper-indent) (insert  "<!-- Looking at " file " (truly " true-file ") as " rel-file " in " directory " (relative to " relative-to ") -->\n"))
	  (unless (string-match "/\\.[^/]*$" file)
	    (if (file-directory-p (expand-file-name file directory))
		;; For a directory, we may recurse to list its files too.
		(if (or (string-match-any webmaster:keep-out-of-directories file)
			(file-exists-p (expand-file-name "sitemap-omit-directory" file)))
		    (progn (html-helper-indent) (insert "<!-- skipping sub-directory " rel-file " -->\n"))
		  (progn
		    (unless (or tabular part-of-larger-flat-list)
		      (let* ((title-file (expand-file-name "title.txt" file))
			     (title (if (file-exists-p title-file)
					(file-contents title-file)
				      rel-file)))
			(html-helper-indent) (insert "<li> <a href=\"" rel-file "\">" title "</a>:\n")))
		    (message "Sub-directory %s" file)
		    (link-make-directory (concat file "/")
					 types
					 tabular
					 part-of-larger-flat-list
					 status-regexp)
		    (unless (or tabular part-of-larger-flat-list)
		      (html-helper-indent) (insert "<!-- end of sub-directory " rel-file " -->\n"))))
	      ;; plain file:
	      (if (string-match types file)
		  (let ((status-string (and status-regexp (find-page-status url))))
		    ;; (insert "<!-- status " (if status-string status-string "?") " -->\n")
		    (if (and
			 (or (null status-regexp)
			    (and (stringp status-string)
				 (string-match status-regexp status-string)))
			 (not (page-unlisted file)))
			(let ((attributes (file-attributes file))
			      (title (find-page-title file rel-file)))
			  (html-helper-indent)
			  (if tabular
			      (let ((content-length (find-page-content-length rel-file))
				    (priority (find-page-revision-priority url))
				    (started (find-page-started url "?")))
				(insert "  <tr align=\"center\">"
					"<th><a href=\"" rel-file "\">" 
					title "</a></th>\n"
					"    <td><code><a href=\"" rel-file "\">" rel-file "</a>(")
				(when (and (numberp content-length)
					   (not (zerop content-length)))
				  (insert (int-to-string content-length) "/"))
				(insert (if attributes
					    (int-to-string (nth 7 attributes))
					  "unknown")
					")</code></td>\n"
					"    <td><i>"
					(if status-string status-string "not known"))
				(when priority (insert "(" priority ")"))
				(insert "</i></td>\n"
					"    <td>" started "</td>\n"
					"  </tr>\n"))
			    ;; not tabular
			    (let* ((priority (if status-regexp (find-page-revision-priority url "z?") nil))
				   (buffer-read-only nil)) ; relevant when processing self, as read-only while getting data
			      (insert "<li> ")
			      (when status-regexp (insert priority " "))
			      (insert "<a href=\"" rel-file "\">"
				      title "</a>")
			      (end-of-line 1)
			      (if status-regexp (insert " (" (if status-string status-string "unknown") ")"))
			      (html-helper-indent) (insert "\n")
			      )))))))))))
    (unless part-of-larger-flat-list
      (html-helper-indent) (insert "</ul>"))))

;;;###autoload
(defun link-make-table (directory types &optional tabular)
  "Make a status table for files in DIRECTORY of TYPES."
  (interactive "DInsert index for directory: 
sInsert index entries for file of type: ")
  (setq tabular current-prefix-arg)
  (auto-table-start tabular)
  (link-make-directory directory types tabular nil "")
  (auto-table-end tabular))

;;;###autoload
(defun link-make-work-table (directory &optional status-regexp)
  "Make a work list for files in DIRECTORY whose status matches STATUS-REGEXP."
  (interactive "DInsert work list for directory: 
sMake entries for status (regexp, defaults sensibly): ")
  (link-make-directory directory "\\.html$" nil t 
		       (if (and (stringp status-regexp)
				(not (zerop (length status-regexp))))
			   status-regexp
			 "\\(stub\\)\\|\\(sketch\\)\\|\\(more\\)\\|\\(in progress\\)")))


;;;###autoload
(defun update-index-status-comments ()
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>\\(<!-- status: .+-->\\)?" (point-max) t)
      (let* ((url (match-string-no-properties  1))
	     (anchor (match-string-no-properties 2))
	     (comment (match-beginning 3))
	     (status (save-match-data (find-page-status url)))
	     (newcomment (if status 
			     (format "<!-- status: %s -->" status)
			   nil)))
	(message "%s (%s) : %s (%s)" url status anchor comment)
	(if status
	    (if comment
		(replace-match newcomment t t nil 3)
	      (insert newcomment)))))))

;;; end of status-table.el

