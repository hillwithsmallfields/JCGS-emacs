;;;; mailto-checker.el
;;; Time-stamp: <2005-01-18 19:04:55 jcgs>

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

(provide 'mailto-checker)

;;;###autoload
(defun html-find-bad-mailto ()
  "Look for things that might be malformed mailto hrefs on this page."
  (interactive)
  (message "Checking buffer %s for bad mailtos" (buffer-name))
  (let* ((bad (save-excursion
	   (or
	    (progn
	      (goto-char (point-min))
	      (re-search-forward "href=\"mailto:[^@]+\"" (point-max) t))
	    (progn
	      (goto-char (point-min))
	      (re-search-forward "href=\"[^:]+@.+\"" (point-max) t))))))
    (if bad
	(progn
	  (goto-char bad)
	  (error "Bad mailto found at %d in %s" bad (buffer-name)))
      (message "No bad mailtos found in buffer %s" (buffer-name)))))

;;; end of mailto-checker.el
