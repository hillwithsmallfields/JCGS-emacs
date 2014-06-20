;;;; word-count.el -- count words in a document
;;; Time-stamp: <2007-03-02 22:45:06 jcgs>

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

(defun count-words-file (file)
  (interactive "fCount words in file: ")
  (save-excursion
    (find-file file)
    (save-excursion
      (goto-char (point-min))
      (let ((words 0))
	(while (forward-word 1)
	  (setq words (1+ words)))
	(message "%s: %d words" (file-name-nondirectory file) words)
	words))))

(defun count-words-directory (directory pattern)
  (interactive "DDirectory: 
sCount words in files matching regexp: ")
  (let ((files (directory-files directory t pattern))
	(total 0))
    (while files
      (setq total (+ total (count-words-file (car files)))
	    files (cdr files)))
    (message "Total: %d words" total)
    total))

(provide 'word-count)

;;; end of word-count.el
