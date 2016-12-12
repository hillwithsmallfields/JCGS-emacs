;;;; wikipedia-cache-tidy.el -- tidy up my saved wikipedia pages
;;; Time-stamp: <2009-08-17 19:19:30 jcgs>

;; Copyright (C) 2007, 2008, 2009, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: www

;; This file is NOT part of GNU Emacs.

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

(require 'flatten-saved-pages)

(defun de-wiki-xrefs ()
  "Make wikipedia cross-references be local links."
  (interactive)
  (while (re-search-forward "a href=\"\\(?:http://en.wikipedia.org\\)?/wiki/\\([^\"]+\\)\"" (point-max) t)
      (replace-match "a href=\"\\1.html\"" t)))

(defun de-wiki-file (file)
  (interactive "fDe-wiki-xrefs in file: ")
  (if (file-directory-p file)
      (when (or (interactive-p)
		(not (string-match "\\.$" file)))
	(mapc 'de-wiki-file (directory-files file t "\\.html$")))
    (let ((visiting (find-buffer-visiting file)))
      (find-file file)
      (de-wiki-xrefs)
      (basic-save-buffer)
      (unless visiting
	(bury-buffer)))))

(defun unscript ()
  "Remove scripts from the buffer."
  (interactive)
  (while (search-forward "<script " (point-max) t)
    (let ((start (match-beginning 0)))
      (when (search-forward "</script>" (point-max) t)
	(delete-region start (point))))))

(defun un-final-junk ()
  "Remove the trailing stuff."
  (interactive)
  (when (search-forward "<div id=\"p-cactions\"" (point-max) t)
      (let ((start (match-beginning 0)))
	(when (search-forward "</body></html>" (point-max) t)
	  (delete-region start (match-beginning 0))))))

(defun un-edit ()
  "Remove the edit tags."
  (interactive)
  (while (search-forward "\">edit</a>]" (point-max) t)
      (let ((end (point)))
	(when (search-backward "[" (point-min) t)
	  (delete-region (point) end))))
    (goto-char (point-min))
    (while (search-forward "action=edit" (point-max) t)
      (when (search-forward ">" (point-max) t)
	(let ((end (point)))
	  (when (search-backward "<" (point-min) t)
	    (let ((start (point)))
	      (delete-region start end)
	      (goto-char start)
	      (when (search-forward "</a>" (point-max) t))))))))

(defun un-lt-IE ()
  "Remove some IE-related stuff."
  (interactive)
  (while (search-forward "<!--[if lt IE 5.5000]>" (point-max) t)
    (let ((start (match-beginning 0)))
      (when (search-forward "<!--[if lt IE 7]>" (point-max) t)
	(delete-region start (point))))))

(defun remove-division (division)
  "Remove the next occurrence of DIVISION."
  (interactive "sRemove division: ")
  (when (re-search-forward (format "<div[^>]+id=\"%s\"" division)
			   (point-max)
			   t)
    (let ((start-start (match-beginning 0))
	  (start-end (match-end 0))
	  (inner-end nil)
	  (inner-start nil))
      ;; delete any inner divisions
      (while (and (setq inner-end (search-forward "</div" (point-max) t))
		  (setq inner-start (search-backward "<div" start-end t)))
	(delete-region inner-start inner-end)
	(goto-char start-end))
      (search-forward "</div>" (point-max) t)
      (delete-region start-start (point)))))

(defun un-site-notice ()
  "Remove the site notice."
  (interactive)
  (remove-division "siteNotice"))

(defun flatten-css-links ()
  "Flatten the css links."
  (interactive)
  (while (re-search-forward "<link rel=\"stylesheet\" type=\"text/css\" media=\"[^\"]+\" href=\"\\(.+/\\)\\(.+\\)\\.css\">"
			    (point-max) t)
      (replace-match "" t t nil)))

(defun remove-end-comments ()
  "Remove some guff at the end."
  (interactive)
  (while (re-search-forward "Pre-expand include size\\|Saved in parser cache" (point-max) t)
    (when (search-backward "<!--" (point-min) t)
      (let ((start (point)))
	(search-forward "-->" (point-max) t)
	(delete-region start (match-end 0))))))

(defun compress-leading-whitespace ()
  "Compress the spaces before tags."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-+<" (point-max) t)
      (replace-match " <" t t))))

(defun tidy-wp-buffer (&optional step-by-step)
  "Tidy a wikipedia buffer.
With optional STEP-BY-STEP, pause after each step."
  (interactive "P")
  (save-excursion
    (mapc (function
	   (lambda (fn)
	     (goto-char (point-min))
	     (funcall fn)
	     (when step-by-step (y-or-n-p (format "Just done %s? " fn)))))
	  '(delete-trailing-whitespace
	    de-wiki-xrefs
	    un-site-notice
	    flatten-css-links
	    un-lt-IE
	    unscript
	    un-final-junk
	    remove-end-comments
	    un-edit
	    compress-leading-whitespace))))

(defun tidy-wp-directory ()
  "Tidy the HTML."
  ;; todo: do this as a side-effect of indexing
  (interactive)
  (dolist (file (directory-files
		 (substitute-in-file-name "$WIKIPEDIA")
		 t "\\.html$"))
    (find-file file)
    (goto-char (point-min))
    (let ((page-hack-version (if (re-search-forward page-hack-pattern (point-max) t)
				 (string-to-number (match-string 1))
			       nil)))
      (when (or (not (numberp page-hack-version))
		(< page-hack-version current-page-hack-version))
	(tidy-wp-buffer)
	(mark-hacked-page)
	(basic-save-buffer)))
    (kill-buffer nil)))

(defun wikipedia-cache-tidy ()
  "Tidy up my wikipedia cache directory."
  (interactive)
  (let* ((dir (substitute-in-file-name "$WIKIPEDIA")))

    ;; replace any html-named files that have a plain-named file that
    ;; is larger, with the plain one
    (mapc (function
	   (lambda (html)
	     (let* ((plain (file-name-sans-extension html)))
	       (when (and (file-exists-p plain)
			  (> (nth 7 (file-attributes plain))
			     (nth 7 (file-attributes html))))
		 (message "fix: %s, %s" html plain)
		 (rename-file plain html t)))))
	  (directory-files dir t "\\.html$"))

    ;; rename any files that don't have a dot in their names, to html
    ;; names
    (mapc (function
	   (lambda (plain)
	     (let* ((html (concat plain ".html")))
	       (unless (or (file-directory-p plain)
			   (file-exists-p html))
		 (message "Plain: %S" plain)
		 (rename-file plain html)))))
	  ;; apply to all files that don't have a dot in their names
	  (directory-files dir t "^[^.]+$"))

    ;; now flatten the pages (raise pictures etc out of the _files
    ;; directories); and do some other processing while we're at it
    (flatten-saved-pages dir)
    (tidy-wp-directory)))

(require 'cl)

(defun wikipedia-index ()
  "Index my wikipedia cache directory."
  (interactive)
  (let* ((dir (substitute-in-file-name "$WIKIPEDIA"))
	 (files (mapcar (function
			 (lambda (html)
			   (let* ((visiting (find-buffer-visiting html))
				  (title "unknown -- possibly not HTML")
				  (complete nil))
			     (find-file html)
			     (goto-char (point-min))
			     (condition-case evar
				 (when (re-search-forward "<title>\\(.+\\) - Wikipedia, the free encyclopedia</title>"
							  (point-max)
							  t)
				   (setq title (match-string-no-properties 1)
					 complete t))
			       (error (setq title "Title search failed")))
			     (condition-case evar
				 (catch 'incomplete
				   (while (re-search-forward "<img.+src=\"\\([^\"]+\\)\"" (point-max) t)
				     (unless (file-exists-p (match-string-no-properties 1))
				       (setq complete nil)
				       (throw 'incomplete t))))
			       (error (setq title "Image search failed")))
			   (unless visiting (kill-buffer nil))
			     (list (file-name-nondirectory html)
				   title
				   complete))
			   ))
			(directory-files dir t "\\.html$"))))
    (find-file (expand-file-name "index.html" dir))
    (erase-buffer)
    (insert "<html><head><title>Index</title></head><body>")
    (let ((letters nil)
	  (letters-place (point))
	  (current-letter 0))
      (insert "<h1>Wikipedia cache index at " (current-time-string) "</h1><ul>\n")
      (dolist (file files)
	(message "Indexing %s" file)
	(let ((this-letter (aref (first file) 0)))
	  (unless (eq this-letter current-letter)
	    (setq current-letter this-letter)
	    (insert "\n</ul>\n<h2><a name=\"" this-letter "\">" this-letter "</a></h2>\n<ul>\n")
	    (push this-letter letters))
	  (insert (format "  <li> <a href=\"%s\">%s</a> (%s)\n"
			  (first file)
			  (second file)
			  (if (third file)
			      "complete"
			    (format "incomplete -- reload from <a href=\"http://en.wikipedia.org/wiki/%s\">here</a>"
				    (file-name-sans-extension (first file))))))))
      (insert "\n</ul></body></html>\n")
      (goto-char letters-place)
      (insert "\n<p>")
      (dolist (letter (nreverse letters))
	(insert "<a href=\"#" letter "\">" letter "</a> "))
      (insert "</p>\n"))
    (basic-save-buffer)))

(defun do-wikipedia-stuff ()
  "Tidy and index my saved pages."
  (interactive)
  (let ((start (current-time)))
    (shell-command "mv $GATHERED/wpnew/* $WIKIPEDIA")
    (wikipedia-cache-tidy)
    (message "Completed cache tidy")
    (wikipedia-index)
    (message "Completed index")
    (let ((end (current-time)))
      (message "That took %f seconds" (float-time (subtract-time end start))))))

;;; end of wikipedia-cache-tidy.el
