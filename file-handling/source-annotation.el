;;;; source-annotation.el -- Find annotation for source files
;;; Time-stamp: <2007-08-28 16:38:03 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: annotation

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

(require 'html-helper-mode)

(defvar annotation-filename-extension ".html"
  "The extension to use for annotation files.")

(define-derived-mode annotation-mode html-helper-mode "Annotation"
  "Major mode for annotation of source code.")

(defun annotate-mark-visited ()
  "Mark that the given file has been visited, if there is no other commentary."
  (when (save-excursion
	  (skip-syntax-forward "-")
	  (looking-at "<h2><a name=\""))
    (insert "\n<p>Visited " (current-time-string) ".</p>\n")))

(defvar annotatable-files-regexp "\\.\\(c\\|el\\|h\\)\\(?:\\.gz\\)?$"
  "Regexp matching the names of files to annotate.")

(defvar pending-find-files nil
  "Files to find at the end of the current command.
Each entry is either a filename, or a list of:
  filename
  mode
  string to search for
  function to run.")

(defun do-pending-find-other-files ()
  "Find any files on `pending-find-files'."
  (let ((window (selected-window))
	(pop-up-windows t))
    (condition-case evar
	;; (message "Finding extra files %S" pending-find-files)
	(while pending-find-files
	  (pop-to-buffer
	   (let* ((file-spec (pop pending-find-files)))
	     (if (stringp file-spec)
		 (find-file-noselect file-spec)
	       (let ((file-name (nth 0 file-spec))
		     (file-mode (nth 1 file-spec))
		     (search-pattern (nth 2 file-spec))
		     (function (nth 3 file-spec)))
		 (let* ((file-buf (find-file-noselect file-name)))
		   (with-current-buffer file-buf
		     (when file-mode
		       (funcall file-mode))
		     (when search-pattern
		       (goto-char (point-min))
		       (re-search-forward search-pattern (point-max) t)
		       (re-search-forward "^\\s-*$" (point-max) t))
		     (when function
		       (funcall function)))
		   file-buf))))))
      (error nil))
    (select-window window)))

(defvar source-annotations-directory (file-truename "~/annotations/")
  "*Directory for annotations file. Must be a truename.")

(defun load-path-truenames ()
  "Return the truenames of the directories in load-path."
  (mapcar 'file-truename load-path))

(defun find-accompanying-annotation-file ()
  "Find the annotation file accompanying the file of the current buffer.

If the buffer is not visiting a file in a directory in
muse-source-annotations-alist, this does nothing.

It is meant for use as a `find-file-hook', but can also be used interactively."
  (interactive)
  (condition-case evar
      (let* ((true-source (file-truename source-directory))
	     (this-source-file (file-truename (buffer-file-name))))
	(when (or (string-match (concat true-source
					".+" annotatable-files-regexp)
				this-source-file)
		  (and (string-match annotatable-files-regexp
				     (file-name-nondirectory this-source-file))
		       (member (substring (file-name-directory this-source-file)
					  0 -1)
			       (load-path-truenames))))
	  (let* ((annotated-directory (file-name-nondirectory
				       (substring
					(file-name-directory this-source-file)
					0 -1)))
		 (this-annotation-file (expand-file-name
					(concat annotated-directory "-notes"
						annotation-filename-extension)
					source-annotations-directory)))
	    (when (file-exists-p this-annotation-file)
	      (push (list this-annotation-file
			  'annotation-mode
			  (format "name=\"%s.*\""
				  (file-name-sans-extension
				   (file-name-nondirectory this-source-file)))
			  'annotate-mark-visited)
		    pending-find-files)))))
    (error nil)))

(add-hook 'find-file-hook 'find-accompanying-annotation-file)

(add-hook 'post-command-hook 'do-pending-find-other-files)

(provide 'source-annotation)

;;; end of source-annotation.el
