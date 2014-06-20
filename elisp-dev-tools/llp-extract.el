;;; llp-extract.el --- Literate Lisp Programming -- extract Lisp from LaTeX

;; Copyright (C) 2007, 2009  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: hypermedia, lisp, tex

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Extract things from an environment \begin{lisp}{filename}...\end{lisp}

;;; Code:

(defun llp-extract (file)
  "Extract Lisp code from FILE."
  (interactive "fExtract Lisp code from file: ")
  (find-file file)
  (save-excursion
    (let ((resulting-files nil)
	  (prefix nil)
	  (extension ".lisp")
	  (lispdir nil)
	  (substitutions nil))
      (goto-char (point-min))
      (when (re-search-forward "\\\\lispprefix{\\([^}]+\\)}" (point-max) t)
	(setq prefix (match-string-no-properties 1))
	(message "prefix %S" prefix))
      (goto-char (point-min))
      (when (re-search-forward "\\\\lispdir{\\([^}]+\\)}" (point-max) t)
	(setq lispdir (expand-file-name (match-string-no-properties 1)))
	(message "lispdir %S" lispdir))
      (goto-char (point-min))
      (when (re-search-forward "\\\\lispextension{\\([^}]+\\)}" (point-max) t)
	(setq extension (match-string-no-properties 1))
	(message "extension %S" extension))
      (goto-char (point-min))
      (while (re-search-forward "\\\\substitution{\\([^}]+\\)}{\\([^}]+\\)}" (point-max) t)
	(push (cons (match-string-no-properties 1)
		    (match-string-no-properties 2))
	      substitutions))
      (goto-char (point-min))
      (while (re-search-forward "\\\\begin{lisp}{\\([^}]+\\)}" (point-max) t)
	(let* ((subfiles (split-string (match-string-no-properties 1) "[, ]+"))
	       (start (point)))
	  (if (search-forward "\\end{lisp}" (point-max) t)
	      (let ((end (match-beginning 0)))
		(when (search-backward "\\end{verbatim}" start t)
		  (setq end (match-beginning 0)))
		(when (search-backward "\\begin{verbatim}" start t)
		  (setq start (match-end 0)))
		(let ((text (buffer-substring-no-properties start end)))
		  (dolist (subfile subfiles)
		    (message "Adding to %S, %S" subfile text)
		    (let ((full (expand-file-name (concat prefix subfile extension) lispdir)))
		      (save-excursion
			(find-file full)
			(if (assoc subfile resulting-files)
			    (goto-char (point-max))
			  (erase-buffer)
			  (insert ";;; extracted from " file " at " (current-time-string) " by llp-extract\n"))
			(let ((inserted-start (point)))
			  (message "Putting into %S, %S" (buffer-file-name) text)
			  (insert "\n" text "\n")
			  (goto-char inserted-start)
			  (while (re-search-forward "^% ?" (point-max) t)
			    (replace-match "" t t))
			  (dolist (sub substitutions)
			    (goto-char inserted-start)
			    (while (re-search-forward (car sub) (point-max) t)
			      (replace-match (cdr sub))))))
		      (unless (assoc subfile resulting-files)
			(push (cons subfile full) resulting-files))))))
	    (error "Lisp beginning but no ending"))))
      (dolist (subfile resulting-files)
	(save-excursion
	  (find-file (cdr subfile))
	  (delete-trailing-whitespace)
	  (goto-char (point-min))
	  (while (search-forward "\n\n" (point-max) t)
	    (let ((blank-wanted-end (point)))
	      (re-search-forward "^." (point-max) t)
	      (delete-region blank-wanted-end (1- (point)))))
	  (goto-char (point-max))
	  (insert "\n;;; end of " (car subfile) "\n")
	  (basic-save-buffer))))))

(provide 'llp-extract)

;;; llp-extract.el ends here
