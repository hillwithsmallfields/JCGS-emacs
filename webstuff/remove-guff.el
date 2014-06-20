;;; remove-guff.el --- Remove assorted guff from web page files

;; Copyright (C) 2007  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; remove scripts, etc, from HTML

;;; Code:

(defun html-remove-guff (file)
  "Remove scripts etc from FILE."
  (interactive "fRemove scripts etc from file: ")
  (if (file-directory-p file)
      (mapcar 'html-remove-scripts
	      (directory-files file t "\\.html?$" t))
    (save-excursion
      (find-file file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<script" (point-max) t)
	  (let ((start (match-beginning 0)))
	    (search-forward "</script")
	    (delete-region start (point))))
	(goto-char (point-min))
	(while (re-search-forward "on\\(?:load\\|mouseover\\|mouseout\\)=\"[^\"]+\"" (point-max) t)
	  (replace-match "" t t))
	(goto-char (point-min))
	(while (search-forward "" (point-max) t)
	  (replace-match "" t t))
	(goto-char (point-min))
	(while (search-forward default-directory (point-max) t)
	  (replace-match "" t t))
	(basic-save-buffer)))))

(provide 'remove-guff)
;;; remove-guff.el ends here
