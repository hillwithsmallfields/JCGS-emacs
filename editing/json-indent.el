;;; json-indent.el --- Adjust the indentation of a json structure  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: convenience, languages

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

;; 

;;; Code:

(defvar json-brackets-regexp "[][{}]"
  "Regexp matching what JSON counts as brackets.")

(defvar json-deepening-brackets '(91 123)
  "List of characters representing deepening brackets in JSON.")

(defun json-indent-expr ()
  "Re-indent the JSON expression starting at point."
  (interactive)
  (let ((depth 0)
	(indentation (current-column))
	(indent-step 4)
	(indent-stack nil)
	(previous (point)))
    (catch 'done
      (while (re-search-forward json-brackets-regexp (point-max) t)
	(save-excursion
	  (let* ((bracket-position (match-beginning 0))
		 (bracket (char-after bracket-position))
		 (bracket-column (1- (current-column)))
		 (deepening (memq bracket json-deepening-brackets)))
	    (if deepening
		(setq depth (1+ depth))
	      (setq depth (1- depth))
	      (if (= depth 0)
		  (throw 'done t)))
	    (back-to-indentation)
	    (let* ((margin-point (point))
		   (margin-column (current-column))
		   (line-start (line-beginning-position))
		   (at-margin (= margin-point bracket-position)))
	      (message "%d %c col=%d %s" depth bracket margin-column (if at-margin "at margin" ""))
	      (if deepening
		  (progn
		    (push indentation indent-stack)
		    (setq indentation (if at-margin
					  (+ indentation indent-step)
					(+ bracket-column indent-step)
					)))
		(setq indentation (pop indent-stack)))
	      (cond
	       ((> indentation margin-column)
		(indent-to indentation))
	       ((< indentation margin-column)
		(move-to-column indentation t)
		(delete-region (point) margin-point)))
	      ;; (save-excursion
	      ;; 	(while (> (point) previous)
	      ;; 	  (beginning-of-line 0)
	      ;; 	  (indent-to indentation)
	      ;; 	  )
	      ;; 	)
	      (setq previous (point))

	      )))))))

(provide 'json-indent)
;;; json-indent.el ends here
