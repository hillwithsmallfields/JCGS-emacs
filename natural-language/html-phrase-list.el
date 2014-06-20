;;;; html-phrase-list.el -- read a phrase list from HTML
;;; Time-stamp: <2006-01-25 10:39:57 jcgs>

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

(provide 'html-phrase-list)

(defvar phrase-list-descrs
  '(("Basque_language" "<li><strong>\\([^<]+\\)</strong>.*= *\\([^<]+\\)</li>" "BSQ" "ENG"))
  "Descriptions of how to read some known phrasebook files.")

(defun read-phrase-list (regexp from-label to-label)
  "Read a phrase list"
  (interactive)
  (save-excursion
    (let ((result nil))
      (goto-char (point-min))
      (while (re-search-forward regexp (point-max) t)
	(let ((from (match-string-no-properties 1))
	      (to (match-string-no-properties 2)))
	  (message "Got %s --> %s" from to)
	  (push (list (cons "#TYPE" "phrase") (cons from-label from) (cons to-label to)) result)))
      (nreverse result))))

(defun convert-phrase-list (html-file csv-file regexp from-label to-label)
  "Convert the HTML phrase list in HTML-FILE to a mulvo table in CSV-FILE, using FROM-LABEL and TO-LABEL as the column names."
  (interactive
   (let* ((phrase-list-web-page-file-name (read-file-name "Convert phrase list web page file: " nil nil t))
	  (phrase-list-web-page-file-directory (file-name-directory phrase-list-web-page-file-name))
	  (base-file-name (file-name-nondirectory (file-name-sans-extension phrase-list-web-page-file-name)))
	  (csv-file-default-name (expand-file-name (concat base-file-name ".csv")
						   phrase-list-web-page-file-directory))
	  (csv-file-name (read-file-name "Write to CSV file: "
					 phrase-list-web-page-file-directory
					 csv-file-default-name
					 nil csv-file-default-name))
	  (descr (assoc base-file-name phrase-list-descrs))
	  (regexp (read-from-minibuffer "Pattern: " (if descr
							(second descr)
						      "<li><strong>\\([^<]+\\)</strong>.*= *\\([^<]+\\)</li>")))
	  (from-label (read-from-minibuffer "Language on left: " (if descr (third descr) nil)))
	  (to-label (read-from-minibuffer "Language on right: " (if descr (fourth descr) nil)))
	  )
     (list phrase-list-web-page-file-name csv-file-name regexp from-label to-label)))
  (find-file html-file)
  (goto-char (point-min))
  (let ((data (read-phrase-list regexp from-label to-label)))
    (csv-write-data-to-file csv-file data 'utf-8-unix)))
			      
;;; end of html-phrase-list.el
