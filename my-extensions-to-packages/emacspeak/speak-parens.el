;;;; speak-parens.el -- tell the user about bracketry
;;; Time-stamp: <2006-04-25 10:13:27 jcgs>

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

(provide 'speak-parens)

(defun current-depth (where &optional msg)
  "Return the depth of nesting of WHERE."
  (interactive "d")
  (let ((depth 0))
    (condition-case evar
	(while where
	  (setq where (scan-lists where -1 1)
		depth (1+ depth)))
      (error (setq where nil)))
    (when (or msg (interactive-p))
      (message "depth %d" depth))
    depth))

;;; end of speak-parens.el
