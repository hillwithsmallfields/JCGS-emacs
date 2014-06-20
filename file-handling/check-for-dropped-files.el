;;; check-for-dropped-files.el --- compare directory trees looking for absences

;; Copyright (C) 2009  John C G Sturdy

;; Author: John C G Sturdy <john.sturdy@ul.ie>
;; Keywords: files

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

(defun hash-files-in-directory (dir &optional hash)
  "Put all the leafnames in DIR into HASH.
Start a new HASH if not given one.
Return HASH."
  (or hash (setq hash (make-hash-table)))
  (message "scanning %s" dir)
  (dolist (file (directory-files dir t nil t))
    (if (file-directory-p file)
	(unless (or (string-match "\\.$" file)
		    (string-match "xvpics" file))
	  (hash-files-in-directory file hash))
      (puthash (file-name-nondirectory file)
	       (cons file (nth 7 (file-attributes file)))
	       hash)))
  hash)

(defun check-for-dropped-files (old-dir new-dir)
  "Check that all non-directory files in OLD-DIR are also present in NEW-DIR.
Comparison is done using the leaf names only.  This is meant for
when you have re-arranged a directory tree, and want to make sure
you didn't lose anything, wrt a preserved copy of the original."
  (interactive "DOld directory:
DNew directory: ")
  (let ((old-hash (hash-files-in-directory old-dir))
	(new-hash (hash-files-in-directory new-dir)))
    (maphash (function
	      (lambda (file full)
		(unless (gethash file new-hash)
		  (message "%s missing (%s, %d bytes in original)"
			   file (car full) (cdr full)))))
	     old-hash)))

(provide 'check-for-dropped-files)
;;; check-for-dropped-files.el ends here
