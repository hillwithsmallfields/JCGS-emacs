;;;; remove-spaces-from-filenames.el -- remove spaces from filenames
;;; Time-stamp: <2007-01-30 09:56:13 john>

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

(defun remove-spaces-from-filenames (dir)
  "Rename files that had spaces in their names."
  (interactive "DDirectory: ")
  (let ((files (directory-files dir nil " " t)))
    (while files
      (let* ((old-name (car files))
	     (new-name (subst-char-in-string ?  ?_ old-name)))
	(rename-file (expand-file-name old-name dir)
		     (expand-file-name new-name dir)))
      (setq files (cdr files)))))

(provide 'remove-spaces-from-filenames)

;;; end of remove-spaces-from-filenames.el
