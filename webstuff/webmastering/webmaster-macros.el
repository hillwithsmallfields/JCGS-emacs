;;; <title>iterate through pages and sites, etc</title>
;;; Time-stamp: <2005-01-18 12:05:56 john>

(provide 'webmaster-macros)
(require 'cl)

;;;###autoload
(defmacro webmaster:throughout-buffer (&rest forms)
  "Run FORMS working down through the buffer."
  `(save-excursion
     (goto-char (point-min))
     ,@forms))

;;;###autoload
(defmacro html-with-undisturbed-timestamps (&rest body)
  "Run BODY without disturbing the timestamps of the current file."
  `(let ((html-helper-timestamp-start "^UNLIKELY-TIMESTAMP$")
	 (html-edit-quietly t))
     ,@body
     (basic-save-buffer)))

(defvar webmaster:latest-url-position nil
  "Where the current URL was found.")

;;;###autoload
(defun webmaster:apply-to-urls-in-current-page (quietly functions &optional funargs)
  "Possibly QUIETLY, to all href and src urls in the current page, apply each of FUNCTIONS in turn.
QUIETLY means without disturbing the in-file timestamp, the recent edits
file, and so forth.
Each function should take one (string) argument, and return a string,
which is used as the first argument to the next function.
The result of the last function is used to replace the original URL.
If optional FUNARGS are given, it is used as an extra argument list
for all the FUNCTIONS.
If any of the functions returns nil, the rest are not evaluated, and the
replacement is not done."
  (if quietly
      (html-with-undisturbed-timestamps
       (apply 'webmaster:apply-to-urls-in-current-page nil
	      functions
	      funargs))
    (unless buffer-read-only
      (webmaster:throughout-buffer
       (let ((ignore-case t))
	 (while (re-search-forward "\\(href\\|src\\)=\"?\\([^\">]+\\)\"?" (point-max) t)
	   (setq webmaster:latest-url-position (point))
	   (let* ((ref (match-string-no-properties 1))
		  (url (match-string-no-properties 2))
		  ;; try hard to avoid marking file spuriously as changed
		  (orig-url (copy-sequence url))
		  (fns functions))
	     (save-match-data
	       (while (and fns url)
		 (setq url (apply (car fns) url funargs)
		       fns (cdr fns))))
	     (when (and url (not (string= url orig-url)))
	       (replace-match (format "%s=\"%s\"" (downcase ref) url) t t)))))))))

(defvar webmaster:edit-files "\\.s?html$"
  "Regexp for files to edit when doing things in directory trees.")

(defvar webmaster:keep-out-of-directories '("^\\.?\\.$")
  "List of regexps for directories not to descend into when doing things in web trees")

;;;###autoload
(defun webmaster:define-site-skip-directory (directory)
  "Define DIRECTORY to be skipped on scans of web trees."
  (push directory webmaster:keep-out-of-directories))

(webmaster:define-site-skip-directory "CVS")
(webmaster:define-site-skip-directory "old")
(webmaster:define-site-skip-directory "wdoc")
(webmaster:define-site-skip-directory "winfo")
(webmaster:define-site-skip-directory "whelp")

(defun string-match-any (regexps string)
  "Like string-match, but takes a list of regexps.
Stops once one of them has matched."
  (catch 'found
    (dolist (regexp regexps)
      (let ((match (string-match regexp string)))
	(when match
	  (throw 'found match))))
    nil))

(defvar webmaster:latest-file-in-tree nil
  "The current file in the tree traversal")

;;;###autoload
(defun webmaster:apply-throughout-tree (tree fn &optional fnargs record keep-all-buffers)
  "Throughout TREE apply FN to FNARGS in each file.
TREE should be a directory name."
  (unless record (setq record (list nil)))
  (if (file-directory-p tree)
      (let ((files (directory-files tree nil nil t)))
	(dolist (file files)
	  (let ((fullfile (expand-file-name file tree)))
	    (when (or (string-match webmaster:edit-files file)
		      (and (file-directory-p fullfile)
			   (not (string-match-any webmaster:keep-out-of-directories file))))
	      (message "webmaster:apply-throughout-tree recursing to handle %s" fullfile)
	      (webmaster:apply-throughout-tree
	       fullfile
	       fn fnargs record keep-all-buffers)
	      (message "webmaster:apply-throughout-tree returning from handling %s" fullfile)))))
    (save-window-excursion
      (let ((already-visiting (find-buffer-visiting tree)))
	(find-file tree)
	(message "webmaster:apply-throughout-tree processing leaf file %s" tree)
	(push tree (car record))
	(setq webmaster:latest-file-in-tree tree)
	(webmaster:throughout-buffer
	 (apply fn fnargs))
	(message "webmaster:apply-throughout-tree processed leaf file %s" tree)
	(unless (or already-visiting
		    keep-all-buffers
		    (buffer-modified-p))
	  (kill-buffer nil)))))
  record)

;;;###autoload
(defun webmaster:apply-to-urls-throughout-tree (tree quietly functions &optional funargs)
  "To all href and src urls in TREE, possibly QUIETLY, apply each of FUNCTIONS in turn.
TREE should be a directory name.
QUIETLY means without disturbing the in-file timestamp, the recent edits
file, and so forth.
Each function should take one (string) argument, and return a string,
which is used as the first argument to the next function.
The result of the last function is used to replace the original URL.
If optional FUNARGS are given, it is used as an extra argument list
for all the FUNCTIONS.
If any of the functions returns nil, the rest are not evaluated, and the
replacement is not done."
  (webmaster:apply-throughout-tree
   tree
   (function
    (lambda ()
      (webmaster:apply-to-urls-in-current-page quietly functions funargs)))))

;;; end of webmaster-macros.el
