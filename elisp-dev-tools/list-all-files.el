;;;; list-all-files.el -- list emacs source files
;;; Time-stamp: <2021-11-14 18:24:26 jcgs>

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

(defun fill-in-files (dir pattern)
  (interactive "DDirectory: 
sPattern: ")
  (insert "<ul>\n")
  (dolist (file (directory-files (expand-file-name dir source-directory)
				 nil pattern))
    (insert "  <li> <a href=\"#" file "\">" file "</a>\n"))
  (insert "</ul>\n")
  (dolist (file (directory-files (expand-file-name dir source-directory)
				 nil pattern))
    (insert "<h2><a name=\"" file "\"></a>"
	    "<a href=\"http://cvs.savannah.gnu.org/viewvc/emacs/emacs/" dir "/" file
	    "?view=log\">" file "</a>"
	    "</h2>\n\n")))


(defun fill-all-files ()
  (interactive)
  (find-file (substitute-in-file-name "$SYNCED/www/computing/emacs/source-notes.html"))
  (goto-char (point-min))
  (while (re-search-forward "<a href=\"\\(.+\\.html\\)\">\\(.+\\)/</a>"
			    (point-max) t)
    (let ((file (match-string-no-properties 1))
	  (dir (match-string-no-properties 2)))
      (message "File %s, dir %s" file dir)
      (save-excursion
	(find-file file)
	(erase-buffer)
	(insert "<html>\n<head>\n<title>Notes on emacs/" dir
		"/</title>\n")
	(insert-file-contents ".head")
	(insert "</head>\n<body>\n<h1>Notes on emacs/" dir
		"/</h1>\n\n")
	(fill-in-files dir "\\.\\(el\\|c\\|h\\)$")
	(insert-file-contents ".tail")
	(insert "</body>\n</html>\n")
	(basic-save-buffer)))))

;;; end of list-all-files.el
