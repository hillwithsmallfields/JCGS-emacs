;;;; flatten-saved-pages.el -- flatten pages saved by browsers
;;; Time-stamp: <2009-05-29 16:51:48 jcgs>

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

(provide 'flatten-saved-pages)

;;; This goes through a directory looking for "_files" directories,
;;; and moves the pictures in them up to the main directory,
;;; modifiying the referring web pages accordingly.

(defvar current-page-hack-version 1)

(defvar page-hack-pattern "<!-- page hack version \\([0-9]+\\)-->")

(defun mark-hacked-page ()
  "Mark the page hack."
  (goto-char (point-min))
  (if (re-search-forward page-hack-pattern (point-max) t)
      (replace-match (int-to-string current-page-hack-version) t t nil 1)
    (goto-char (point-max))
    (insert (format "<!-- page hack version %d-->" current-page-hack-version))))

(defun flatten-saved-page (referring-file page-dir page-pattern)
  "Flatten the web page REFERRING-FILE whose auxiliary files match in PAGE-DIR/PAGE-PATTERN."
  (interactive "fFlatten page in file:
sFlatten aux files in directory matching:
sFlatten aux files matching: ")
  (when (and (stringp referring-file)
	     (file-exists-p referring-file))
    (let* ((new-containing-directory (file-name-directory referring-file))
	   (was-visiting (find-buffer-visiting referring-file))
	   (pattern (format "\"\\(%s\\)/\\(%s\\)\"" page-dir page-pattern)))
      (message "new-containing-directory %S; was-visiting %S; pattern %S"
	       new-containing-directory was-visiting pattern)
      (save-window-excursion
	(find-file referring-file)
	(save-excursion
	  (goto-char (point-min))
	  (let ((page-hack-version (if (re-search-forward page-hack-pattern (point-max) t)
					(string-to-number (match-string 1))
				     nil)))
	    (when (or (not (numberp page-hack-version))
		       (< page-hack-version current-page-hack-version))
	      (save-excursion
		(goto-char (point-min))
		(while (re-search-forward pattern (point-max) t)
		  (let* ((old-directory-name (match-string-no-properties 1))
			 (short-name (match-string-no-properties 2))
			 (raw-old-name (concat old-directory-name "/" short-name))
			 (old-full-name (expand-file-name short-name
							  old-directory-name))
			 (new-full-name (expand-file-name short-name
							  new-containing-directory)))
		    (message "Want to move %S to %S" old-full-name new-full-name)
		    (when t
		      (save-excursion
			(condition-case evar
			    (rename-file old-full-name new-full-name t)
			  (error (message "Error in renaming %s to %s" old-full-name new-full-name)))
			(goto-char (point-min))
			(while (search-forward raw-old-name (point-max) t)
			  (replace-match
			   (if nil
			       new-full-name
			     short-name)
			   t t))))))
		(mark-hacked-page))
	      (when (<= (length (directory-files page-dir)) 2)
		(delete-directory page-dir))
	      (when (boundp 'flatten-saved-page-hooks)
		(run-hooks 'flatten-saved-page-hooks))
	      (let ((write-file-hooks nil)
		    (write-file-functions nil)
		    (local-write-file-hooks nil))
		(basic-save-buffer)))))
	(unless was-visiting
	  (kill-buffer nil))))))

(defun flatten-saved-pages (dir)
  "Flatten saved pages in DIR."
  (interactive "DFlatten pages in directory: ")
  (mapcar (lambda (files-dir)
	    (message "holder %s" (file-name-directory files-dir))
	    (let* ((base (substring (file-name-nondirectory files-dir) 0 -6))
		   (fred (message "base %s" base))
		   (page (car (directory-files
			       dir
			       t
			       (format "%s\\.html?$" base)))))
	      (message "Processing %s" page)
	      (flatten-saved-page page
				  (file-name-nondirectory files-dir)
				  ".*?")))
	  (directory-files dir t "_files$")))

;;; end of flatten-saved-pages.el
