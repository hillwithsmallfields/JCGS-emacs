;;;; utf8-to-html.el -- convert UTF-8 characters to HTML special objects
;;; Time-stamp: <2005-10-13 09:50:13 jcgs>

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

(provide 'utf8-to-html)
(require 'cl)

(defvar utf8-to-html-alist
  '((?á . "&aacute;")
    (?ó . "&oacute;")
    (?Ó . "&Oacute;")
    (?ä . "&auml;")
    (?é . "&eacute;")
    ;; (?& . "&amp;")
    ;; (?< . "&lt;")
    ;; (?> . "&gt;")
    )
  "Alist defining how to convert each utf8 special character to HTML.
There are many more than I have entered here.")

(defun utf8-to-html-region (a b)
  "Convert utf8 non-ascii characters between A and B to HTML special objects."
  (interactive "r")
  (save-excursion
    (let ((cp b))
      (while (>= cp a)
	(let ((html-pair (assq (char-after cp) utf8-to-html-alist)))
	  (when html-pair
	    (goto-char cp)
	    (delete-char 1)
	    (insert (cdr html-pair)))
	  (decf cp))))))

;;; end of utf8-to-html.el
