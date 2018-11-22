;;;; octal-chars.el -- convert escape sequences in a file
;;; Time-stamp: <2018-11-15 19:23:48 jcgs>

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

(provide 'octal-chars)

(defun convert-escape-sequences-region (from to)
  "Convert escape sequences between FROM and TO."
  (interactive "r")
  (save-excursion
  (goto-char to)
  (while (re-search-backward "\\\\\\([0-7][0-7][0-7]\\)" from t)
    (replace-match (char-to-string (string-to-number (match-string 1) 8))
		   t t))))

(defun convert-escape-sequences-buffer ()
  "Convert escape sequences throughout this buffer."
  (interactive)
  (convert-escape-sequences-region (point-min) (point-max)))

;;; end of octal-chars.el
