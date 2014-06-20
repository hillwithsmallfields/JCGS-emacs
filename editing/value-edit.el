;;; value-edit.el --- Edit text values in various common ways

;; Copyright (C) 2012  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, data

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

;; Some edits to textual representations of data values are very
;; predicatable.  This does a few of them for you.

;;; Code:

(require 'thingatpt)

(defmacro edit-textual-value (where old-text-name &rest body)
  "Framework for editing a textual value.
WHERE is where to do it around.
OLD-TEXT-NAME is the symbol to use for the old value.
BODY are the forms to execute."
  `(save-excursion
     (goto-char ,where)
     (let ((bounds (bounds-of-thing-at-point 'symbol)))
       (when bounds
	 (let* ((start (car bounds))
		(end (cdr bounds))
		(,old-text-name (buffer-substring start end))
		(new-text (progn
			    ,@body)))
	   (when (stringp new-text)
	     (delete-region start end)
	     (insert new-text)))))))

(defun value-toggle (where)
  "Toggle the truth value at WHERE (point, interactively)."
  (interactive "d")
  (edit-textual-value where old-value
		      (cond
		       ((string= old-value "True") "False")
		       ((string= old-value "False") "True")
		       ((string= old-value "true") "false")
		       ((string= old-value "false") "true")
		       ((and (memq major-mode '(lisp-mode emacs-lisp-mode))
			     (string= old-value "t")) "nil")
		       ((string= old-value "nil") "t")
		       ((string= old-value "#t") "#f")
		       ((string= old-value "#f") "#t")
		       ((string= old-value "0") "1")
		       ((string-match old-value "[-0-9]+") "0")
		       ((string= old-value "yes") "no")
		       ((string= old-value "no") "yes")
		       ((string= old-value "ja")
			(cond
			 ((string-match "german" current-input-method-title) "nein")
			 ((string-match "dutch" current-input-method-title) "nee")
			 (t nil)))
		       ((string= old-value "nein") "ja")
		       ((string= old-value "nee") "ja")
		       ((string= old-value "oui") "non")
		       ((string= old-value "non") "oui") ; probably used in several languages
		       (t nil))))

(defun value-increment (where)
  "Increment the textual value at WHERE (point, interactively)."
  (interactive "d")
  (edit-textual-value where old-value
		      (cond
		       ((string-match "0x\\([0-9a-f]+\\)" old-value)
			(format "0x%x" (1+ (string-to-number (match-string 1 old-value) 16))))
		       ((string-match "0\\([0-7]+\\)" old-value)
			(format "0%o" (1+ (string-to-number (match-string 1 old-value) 8))))
		       ((string-match "\\([-0-9]+\\)" old-value)
			(format "%d" (1+ (string-to-number old-value)))))))

(defun value-decrement (where)
  "Decrement the textual value at WHERE (point, interactively)."
  (interactive "d")
  (edit-textual-value where old-value
		      (cond
		       ((string-match "0x\\([0-9a-f]+\\)" old-value)
			(format "0x%x" (1- (string-to-number (match-string 1 old-value) 16))))
		       ((string-match "0\\([0-7]+\\)" old-value)
			(format "0%o" (1- (string-to-number (match-string 1 old-value) 8))))
		       ((string-match "\\([-0-9]+\\)" old-value)
			(format "%d" (1- (string-to-number old-value)))))))

(provide 'value-edit)
;;; value-edit.el ends here
