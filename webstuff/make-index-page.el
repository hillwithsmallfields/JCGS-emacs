;;;; make-index-page.el -- make an index page for a directory
;;; Time-stamp: <2007-06-18 21:26:45 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


(defvar index-start-comment "<!-- automatically generated index -->\n"
  "Start marker for automatically generated index.")

(defvar index-end-comment "<!-- end of index -->\n"
  "End marker for automatically generated index.")

(defun find-page-title (page-file)
  "Return the title of PAGE-FILE."
  (save-excursion
    (cond 
     ((string-match "\\.html$" page-file)
      (let ((visiting (get-file-buffer page-file)))
	(find-file page-file)
	(prog1
	    (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward "<title>\\(.+\\)</title>" (point-max) t)
		  (match-string-no-properties 1)
		(file-name-nondirectory page-file)))
	  (unless visiting (kill-buffer (current-buffer))))))
     (t (file-name-nondirectory page-file)))))

(defvar only-hours t
  "*Whether to leave other index pages alone.")

(defun make-index-page (directory &optional pattern)
  "Make an index page for DIRECTORY."
  (interactive "DDirectory: 
sPattern (default \\(\\.html\\|\\.pdf\\|\\.txt\\)$: ")
  (if (or (null pattern)
	  (string= pattern ""))
      (setq pattern "\\(\\.html\\|\\.pdf\\|\\.txt\\)$"))
  (catch 'skip
    (let* ((files-to-index (directory-files directory t pattern))
	   (subdirs-to-index (directory-files directory t "[^.]$"))
	   (subdirs-indexed nil)
	   (index-file-name (expand-file-name "index2.html" directory))
	   (existing (file-exists-p index-file-name))
	   (visiting (get-file-buffer index-file-name)))
      (dolist (subdir subdirs-to-index)
	(when (and (file-directory-p subdir)
		   (not (string-match "_files" subdir)))
	  (make-index-page subdir pattern)
	  (setq subdir (file-name-nondirectory subdir))
	  (let ((sub-index (concat
			    ;; don't use expand-file-name as we don't want the long form
			    subdir "/index2.html")))
	    (when (file-readable-p sub-index)
	      (message "subdir %s; sub-index %s" subdir sub-index)
	      (push (cons sub-index subdir) subdirs-indexed)))))
      (when (or files-to-index subdirs-indexed)
	(find-file index-file-name)
	(goto-char (point-min))
	(dolist (tag '("h1" "title"))
	  (save-excursion
	    (when (re-search-forward (format "<%s>\\(.*\\)</%s>" tag tag)
				     (point-max) t)
	      (replace-match (format "Index of %s"
				     (file-name-nondirectory
				      (directory-file-name directory)))
			     t t nil 1))))
	(if (search-forward index-start-comment (point-max) t)
	    (let ((start (match-end 0)))
	      (if (search-forward index-end-comment (point-max) t)
		  (delete-region start (match-beginning 0))
		(error "Found start marker but no end marker"))
	      (goto-char start))
	  (if (and
	       ;; only-ours
	       existing)
	      (throw 'skip 'skip)
	    (cond
	     ((re-search-forward "</h1>" (point-max) t)
	      (beginning-of-line 2))
	     ((re-search-forward "<hr>" (point-max) t)
	      (beginning-of-line 0))
	     ((re-search-forward "</body>" (point-max) t)
	      (beginning-of-line 0))))
	  (delete-blank-lines) (delete-blank-lines)
	  (insert "\n" index-start-comment)
	  (save-excursion
	    (insert index-end-comment "\n")))
	(insert "<ul>\n")
	(dolist (subdir subdirs-indexed)
	  (insert "  <li> <a href=\"" (car subdir) "\">"
		  "Index of subdirectory " (cdr subdir) "</a>\n"))
	(dolist (file files-to-index)
	  (insert "  <li> <a href=\"" (file-name-nondirectory file) "\">" 
		  (find-page-title file) "</a>\n"))
	(insert "</ul>\n")
	(basic-save-buffer)
	(unless visiting (kill-buffer (current-buffer)))))))

(provide 'make-index-page)

;;; end of make-index-page.el
