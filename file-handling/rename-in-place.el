;;;; rename-in-place.el -- rename the file in the current buffer
;;; Time-stamp: <2013-06-05 11:42:45 johnstu>

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

(provide 'rename-in-place)

;;;###autoload
(defun rename-file-in-buffer (newname)
  "Rename the current buffer's file to NEWNAME and continue to visit it."
  (interactive "FRename to: ")
  (let ((p (point))
	(m (mark)))
    (if (file-directory-p newname)
	(setq newname (expand-file-name (file-name-nondirectory (buffer-file-name))
					newname)))
    (rename-file buffer-file-name newname)
    ;; (find-alternate-file newname)
    (set-visited-file-name newname t t)
    (set-mark m) (goto-char p)))

(defun rename-directory (olddir newdir)
  "Rename OLDDIR to NEWDIR.
In buffers visiting files in OLDDIR, visit the same file in NEWDIR instead."
  (interactive "DRename directory: 
FRename %s to: ")
  (rename-file olddir newdir)
  (let ((old-dir-len (length olddir)))
    (mapcar (function
	     (lambda (buffer)
	       (set-buffer buffer)
	       (let* ((old-file (buffer-file-name))
		      )
		 (when (and (stringp old-file)
			    (string= olddir
				      (substring old-file 0 old-dir-len)))
		   (find-alternate-file
		    ....
		    )
		   ))))
	    (buffer-list))))

;;; end of rename-in-place.el
