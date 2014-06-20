;;;; read-table-data.el
;;; Time-stamp: <2005-01-18 19:06:25 jcgs>

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

(provide 'read-table-data)
(require 'cl)

(defun parse-options (options-string)
  "Parse the OPTIONS-STRING to the obvious alist."
  (let ((pair-strings (split-string options-string))
	(pairs nil))
    (dolist (pair-string pair-strings)
      (if (string-match "^\\([^=]+\\)=\"\\([^\"]*\\)\"$" pair-string)
	  (push (cons (substring pair-string (match-beginning 1) (match-end 1))
		      (substring pair-string (match-beginning 2) (match-end 2)))
		pairs)
	(push (cons pair-string nil)
	      pairs)))
    (nreverse pairs)))

;;;###autoload
(defun read-table-data (start end)
  "Parse the HTML table between START and END.
The result is a list of rows, in order.
Each row is a list of cells.
Each cell is a list of:
 a string representing the class option of the cell, or nil;
 the tag of the cell (td or th)
 the cell contents as a string
 an alist of the cell tag options."
  (save-excursion
    (goto-char end)
    (let ((rows nil)
	  (row-start nil))
      (while (save-excursion
	       (setq row-start
		     (re-search-backward "<tr" start t)))
	(let ((cells nil)
	      (cell-end nil)
	      (cell-start nil))
	  (while (setq cell-end (re-search-backward "</t[dh]>" row-start t))
	    (setq cell-start (re-search-backward "<\\(t[dh]\\)\\([^>]*\\)>" row-start))
	    (let* ((tag (match-string-no-properties 1))
		   (data (buffer-substring-no-properties (match-end 0) cell-end))
		   (options-string (if (match-beginning 2) (match-string-no-properties 2) nil))
		   (options-list (if options-string
				     (parse-options options-string)
				   nil)))
	      ;; (message "%s: %s" tag data)
	      (push (list (cdr (assoc "class" options-list))
			  tag
			  data
			  options-list)
		    cells)))
	  (goto-char row-start)
	  (push cells rows)))
      rows)))

;;;###autoload
(defun read-table-from-file (file)
  "Read the table data from FILE.
Will read the first table."
  (save-window-excursion
    (find-file file)
    (save-excursion
      (goto-char (point-min))
      (let* ((start (if (re-search-forward "<table[^>]*>" (point-max) t)
			(match-beginning 0)
		      nil))
	     (end (if (re-search-forward "</table[^>]*>" (point-max) t)
		      (match-end 0)
		    nil))
	     (table (if (and start end)
			(read-table-data start end))))
	table))))
