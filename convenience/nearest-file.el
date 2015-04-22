;;; nearest-file.el --- Find the nearest file of a given name, or switch to the file buffer with the nearest name  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Sturdy

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

;; 

;;; Code:

(defun directory-depth (filename &optional starting-from)
  "Return the directory depth of FILENAME.
If STARTING-FROM is given, start from there instead of the start of FILENAME."
  (let ((i (or starting-from 1))
	(end (length filename))
	(count 0))
    (while (< i end)
      (when (= (aref filename i) ?/)
	(setq count (1+ count)))
      (setq i (1+ i)))
    ;; (message "Depth of %S from %d is %d" filename starting-from count)
    count))

(defun nearest-buffer-below (name starting-directory)
  "Choose the buffer visiting a file NAME nearest below STARTING-DIRECTORY.
The buffer chosen will have the non-directory name of NAME, and the fewest
intervening directory levels below STARTING-DIRECTORY."
  (let* ((best-deeper-buffer-so-far nil)
	 (best-depth-so-far most-positive-fixnum)
	 (directory-pattern (concat "^" (regexp-quote starting-directory)))
	 (starting-directory-length (length starting-directory)))
    ;; (message "Looking for buffers in directories below %S (pattern %S) checking depth from %d" starting-directory directory-pattern starting-directory-length)
    (dolist (buf (buffer-list))
      (let ((full-name (buffer-file-name buf))
	    (depth nil))
	;; (message "  Inner loop: full-name=%S (file-name-nondirectory full-name)=%S" full-name (if (stringp full-name) (file-name-nondirectory full-name) "n/a"))
	(when (and (stringp full-name)
		   (string= (file-name-nondirectory full-name) name)
		   (string-match directory-pattern full-name)
		   (or (not (bufferp best-deeper-buffer-so-far))
		       (< (setq depth (directory-depth full-name starting-directory-length))
			  best-depth-so-far)))
	  (setq best-deeper-buffer-so-far buf
		best-depth-so-far (or depth ; unset if this is the first one found
				      (directory-depth full-name starting-directory-length)))
	  ;; (message " Inner loop got %S (file %S) at depth (starting from %d) of %S" best-deeper-buffer-so-far full-name starting-directory-length best-depth-so-far)
	  )))
    best-deeper-buffer-so-far))

(defun switch-to-nearest-file-buffer (name)
  "Switch to the buffer visiting the nearest file called NAME.
First, try all the buffers visiting files of that name in subdirectories
of `default-directory', and if none is found there, try working back up
the directory tree."
  (interactive
   (let ((names nil))
     (dolist (buf (buffer-list))
       (let ((filename (buffer-file-name buf)))
	 (when filename
	   (pushnew (file-name-nondirectory filename) names))))
     (list (completing-read "Switch to buffer: " names nil t))))
  (let ((buffer nil)
	(dir (file-truename default-directory)))
    (while (and dir (null buffer))
      ;; (message "Outer loop: buffer=%S dir=%S" buffer dir)
      (setq buffer (nearest-buffer-below name dir)
	    dir (file-name-directory (substring dir 0 -1))))
    (if (bufferp buffer)
	(switch-to-buffer buffer)
      (error "Could not find which buffer is nearest: algorithm found %S" buffer))))

(provide 'nearest-file)
;;; nearest-file.el ends here
