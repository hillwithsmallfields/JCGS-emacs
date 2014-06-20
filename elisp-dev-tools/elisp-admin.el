;;; elisp-admin.el --- JCGS' header checking

;; Copyright (C) 2007  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: maint

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; 


;;; History:
;; 

;;; Code:

(defun boilerplate-read-keywords (prompt)
  "Read keywords, using PROMPT."
  (let ((keywords nil)
	(completion (cons (cons "" "Empty one to terminate")
			  finder-known-keywords))
	keyword)
    (while (not (string= (setq keyword (completing-read prompt completion))
			 ""))
      (push keyword keywords))
    (mapconcat 'identity (nreverse keywords) ", ")))

(defvar boilerplate-excused '("vocab-cache.el")
  "File names for which the boilerplate check does not apply.")

(defun jcgs-check-boilerplate ()
  "Check the boilerplate of the current file."
  (interactive)
  (when (and (stringp (buffer-file-name))
	     (eq major-mode 'emacs-lisp-mode)
	     (string-match (file-truename (substitute-in-file-name "$COMMON"))
			   (file-truename (buffer-file-name)))
	     (not (member (file-name-nondirectory (buffer-file-name))
			  boilerplate-excused))
	     (not (string-match "config" (buffer-file-name))))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "either version 3 of the License" (point-max) t)
	(replace-match "either version 3 of the License" t t)))
    (let ((verified-result (lm-verify (buffer-file-name)
				      nil
				      t
				      t)))
      (message "Verified: %S" verified-result)
      (when (and (not (and (save-excursion
			     (goto-char (point-min))
			     (search-forward ";; Author: John C. G. Sturdy <john@cb1.com>" (point-max) t))
			   (save-excursion
			     (goto-char (point-min))
			     (search-forward ";; Maintainer: John C. G. Sturdy <john@cb1.com>" (point-max) t))
			   (save-excursion
			     (goto-char (point-min))
			     (search-forward ";; Created: " (point-max) t))
			   (save-excursion
			     (goto-char (point-min))
			     (search-forward ";; Keywords:" (point-max) t))))
		 (yes-or-no-p (format "Fix boilerplate for %s? " (file-name-nondirectory (buffer-file-name)))))
	(save-excursion
	  (let* ((copyright (save-excursion
			      (goto-char (point-min))
			      (search-forward "Copyright (C).*$" (point-max) t)))
		 (created (if (save-excursion
				(goto-char (point-min))
				(re-search-forward "Copyright (C) \\([0-9]\\{4\\}\\)" (point-max) t))
			      (match-string 1)
			    (read-from-minibuffer (format "When was %s created: " (file-name-nondirectory (buffer-file-name)))))))
	    (goto-char (point-min))
	    (if copyright
		(progn
		  (goto-char copyright)
		  (move-beginning-of-line 2))
	      (re-search-forward "^[; ]*$")
	      (forward-char 1))
	    (unless copyright
	      (insert ";; Copyright (C) " (substring (current-time-string) -4) ", John C. G. Sturdy\n"))
	    (insert "\n;; Author: John C. G. Sturdy <john@cb1.com>\n")
	    (insert ";; Maintainer: John C. G. Sturdy <john@cb1.com>\n")
	    (insert ";; Created: " created "\n")
	    (insert ";; Keywords: "(boilerplate-read-keywords (format "Keywords (%s): " (file-name-nondirectory (buffer-file-name)))) "\n")
	    (insert "\n;; This file is NOT part of GNU Emacs.\n\n")))
	(when (y-or-n-p "Run checkdoc? ")
	  (checkdoc))))))

(provide 'elisp-admin)

;;; elisp-admin.el ends here
