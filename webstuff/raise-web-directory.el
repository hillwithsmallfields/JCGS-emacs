;;;; raise-web-directory.el
;;; Time-stamp: <2005-01-18 19:06:26 jcgs>
;;; bring buffers in a given directory to the top
;;; also manage list of such directories

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

(provide 'raise-web-directory)

(defvar web-directories nil
  "List of directories which appear to contain web pages.")

;;;###autoload
(defun web-directories (&optional force)
  "Return a list of directories appearing to contain web pages.
Looks along your buffer list for possibilities."
  (if (and web-directories (not force))
      web-directories
    (progn
      (setq web-directories nil)
      (dolist (b (buffer-list))
	(set-buffer b)
	(when (and (eq major-mode 'html-helper-mode)
		   (not (rassoc default-directory web-directories)))
	  (push (cons (file-name-nondirectory (substring default-directory 0 -1))
		      default-directory)
		web-directories))))
    web-directories))

;;;###autoload
(defun raise-web-directory (directory-name)
  "Raise the directory whose last name part is DIRECTORY-NAME"
  (interactive (list
		(completing-read "Raise web directory: "
				 (web-directories))))
  (let ((directory (cdr (assoc directory-name (web-directories)))))
    (dolist (b (buffer-list))
      (set-buffer b)
      (when (and (eq major-mode 'html-helper-mode)
		 (string= directory default-directory))
	(switch-to-buffer b)))
    (let ((index (expand-file-name "index.html" directory)))
      (if (file-exists-p index)
	  (find-file index)
	(let ((index2 (expand-file-name (concat directory-name ".html")
					directory)))
	  (when (file-exists-p index2)
	    (find-file index2)))))))


  
