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

(defun jcgs/log-file-as-read (&optional note)
  "Log that I have read the file in this buffer.
Optional NOTE may be added."
  (interactive "sNote about file: ")
  (unless (buffer-file-name)
    (error "This command is for file buffers only"))
  (let* ((full-form (buffer-file-name))
	 (trimmed-form full-form))
    (catch 'trimmed
      (dolist (base-description '("$OPEN_PROJECTS"
				  "$EHOME/JCGS-emacs"
				  "$EHOME/JCGS-org-mode"))
	(let ((base (substitute-in-file-name base-description)))
	  (when (eq (compare-strings full-form 0 (length base)
				     base nil nil)
		    t)
	    (setq trimmed-form (concat base-description
				       (substring full-form (length base))))
	    (throw 'trimmed t)
	    ))
	))
    (save-window-excursion
      (find-file jcgs/files-read-log)
      (goto-char (point-max))
      (unless (search-backward (concat trimmed-form ",") (point-min) t)
	(insert trimmed-form ","
		(format-time-string "%F"))
	(when note
	  (insert "," note))
	(insert "\n")
	(sort-lines nil (point-min) (point-max))
	(basic-save-buffer)
	(bury-buffer)))))

(provide 'file-reading)
;;; file-reading.el ends here
