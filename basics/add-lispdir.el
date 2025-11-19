;;; Time-stamp: <2025-11-19 14:41:57 jcgs>
;; add a directory to the emacs load path

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

(provide 'add-lispdir)

(defun add-lispdir (lispdir &optional url unpacker)
  "If it exists, add Lisp directory LISPDIR to `load-path'.
If the directory contains any .info files, add it to Info-directory-list too.

If it does not exist, and an optional second argument URL is given and
names a reachable resource, create the directory and populate it from
the URL target (and then add it to `load-path').  A further optional
argument is a form or function to run to do the unpacking.
Optional argument UNPACKER is how to unpack it."
  (interactive "DAdd directory to path: ")
  (setq lispdir (expand-file-name (substitute-in-file-name lispdir)))
  (if (file-directory-p lispdir)
      (progn
	(add-to-list 'load-path lispdir)
	(when nil (directory-files lispdir nil "\\.info$")
	      (require 'info)
	      (add-to-list 'Info-directory-list lispdir)))
    (if (and (stringp url))
	(let* (file host protocol)
	  (message "Setting up %s from %s" lispdir url)
	  (cond
	   ((string-match "^/\\([-.a-z0-9]+\\):\\(.+\\)$" url)
	    (setq file (match-string 3 url)
		  host (match-string 2 url))
	    (message "Fetching %s using ange-ftp" url))
	   ((string-match "^\\([a-z]+\\)://\\([-a-z0-9.]+\\)/\\(.+\\)$" url)
	    (setq file (match-string 3 url)
		  host (match-string 2 url)
		  protocol (match-string 1 url))
	    (message "Fetching %s from %s using %s" file host protocol))
	   (t
	    (message "Don't know how to fetch %s" url)))
	  )
      (message "Warning: %s is not a directory; not adding to load-path" lispdir))))

(defun add-tree-to-load-path (dir)
  "Add DIR and any directories below it to `load-path'."
  (message "Recursively adding %S to load-path" dir)
  (unless (string= user-emacs-directory dir)
    (add-to-list 'load-path dir))
  (dolist (file (directory-files dir))
    (when (and (file-directory-p file)
	       (not (member file '("." ".." ".git"))))
      (add-tree-to-load-path (expand-file-name file dir)))))

;;; end of add-lispdir.el
