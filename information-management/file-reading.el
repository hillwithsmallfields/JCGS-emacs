;;; file-reading.el --- Keep track of what files I have read  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Maintain a log of files that I've indicated that I have read.

;;; Code:

(defvar jcgs/files-read-log (substitute-in-file-name "$ORG/files-read.csv")
  "Files that I have indicated that I have read.")

(defun jcgs/files-read-log-trim-name (name)
  "Trim NAME."
  (catch 'trimmed
    (dolist (base-description '("$OPEN_PROJECTS"
				"$EHOME/JCGS-emacs"
				"$EHOME/JCGS-org-mode"))
      (let ((base (substitute-in-file-name base-description)))
	(when (eq (compare-strings name 0 (length base)
				   base nil nil)
		  t)
	  (throw 'trimmed (concat base-description
				  (substring name (length base)))))))
    name))

(defun jcgs/files-read-log-get-existing-comment (name)
  "Get the existing comment for NAME."
  (save-window-excursion
    (find-file jcgs/files-read-log)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (concat "^"
				     (jcgs/files-read-log-trim-name name)
				     ",[-0-9]+,\\(.+\\)$")
			     (point-max) t)
	  (match-string-no-properties 1)
	nil))))

(defun jcgs/files-read-log-get-existing-comments ()
  "Get all the existing comment for any files."
  (let ((results nil))
    (save-window-excursion
      (find-file jcgs/files-read-log)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^[^,]+,[-0-9]+,\\(.+\\)$"
				  (point-max) t)
	  (push (match-string-no-properties 1)
		results))))
    results))

(defvar jcgs/files-read-log-read-note-with-completion nil
  "Whether to use completion when reading the note.")

(defun jcgs/files-read-log-as-read (&optional note)
  "Log that I have read the file in this buffer.
Optional NOTE may be added."
  (interactive
   (list (if jcgs/files-read-log-read-note-with-completion
	     (completing-read "Note about file: "
			      (jcgs/files-read-log-get-existing-comments)
			      nil nil
			      (jcgs/files-read-log-get-existing-comment
			       (buffer-file-name)))
	   (read-from-minibuffer "Note about file: "
				 (jcgs/files-read-log-get-existing-comment
				  (buffer-file-name))))))
  (unless (buffer-file-name)
    (error "This command is for file buffers only"))
  (let ((trimmed-form (jcgs/files-read-log-trim-name (buffer-file-name))))
    (save-window-excursion
      (find-file jcgs/files-read-log)
      (goto-char (point-max))
      (if (search-backward (concat trimmed-form ",") (point-min) t)
	  ;; already got this file; update note
	  (if (looking-at "^[^,]+,[-0-9]+,\\(.+\\)")
	      (replace-match note t t nil 1)
	    (end-of-line 1)
	    (insert "," note))
	;; new file for the collection
	(insert trimmed-form ","
		(format-time-string "%F"))
	(when note
	  (insert "," note))
	(insert "\n")
	(sort-lines nil (point-min) (point-max)))
      (basic-save-buffer)
      (bury-buffer))))

(provide 'file-reading)
;;; file-reading.el ends here
