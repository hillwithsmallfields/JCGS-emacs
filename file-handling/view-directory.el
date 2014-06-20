;;; view-directory.el --- view all files in a directory

;; Copyright (C) 2011  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
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

;; 

;;; Code:

(defun view-directory (dir &optional starting-after)
  "View all files in DIR.
If STARTING-AFTER is non-nil, it is names a file to search for;
viewing will start on the file after that one."
  (interactive "DView files in directory: ")
  (let* ((files (directory-files dir t))
	 (file (car files))
	 )
    (when starting-after
      (while (and files
		  (not (string= starting-after (file-name-nondirectory file))))
	(setq files (cdr files)
	      file (car files)))
      (setq files (cdr files)
	    file (car files)))
    (while (and files
		(not (file-regular-p file)))
      (setq files (cdr files)
	    file (car files)))
    (view-directory-from-this-file file)))

(defun view-directory-from-this-file (file)
  "View the directory containing FILE, starting from that file."
  (interactive "fView starting at file: ")
  (find-file file)
  (make-local-variable 'view-scroll-auto-exit)
  (setq view-exit-action 'view-directory-from-next-file
	view-scroll-auto-exit t)
  (view-mode 1))

(defun view-directory-from-next-file (this-buffer)
  "View the file after the one held in THIS-BUFFER."
  (interactive "bView file following that in buffer: ")
  (view-directory default-directory (file-name-nondirectory (buffer-file-name this-buffer))))

(provide 'view-directory)
;;; view-directory.el ends here
