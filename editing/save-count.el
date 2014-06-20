;;;; save-count.el -- count the number of saves of a file
;;; Time-stamp: <2005-05-27 15:09:57 john>

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

(provide 'save-count)

(defvar save-count-pattern "\\(Save count: \\[\\)\\([0-9]+\\)\\(\\]\\)"
  "Pattern defining the save count for a file.
There should be three sub-patterns: preamble, count, postamble.
This is buffer-local, and you might want to set it in the file variables.")

(make-variable-buffer-local 'save-count-pattern)

(defun save-count-update ()
  "Update the save count of a file.
The syntax recognized is defined by save-count-pattern.
Meant to go on write-file-hook."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward save-count-pattern (point-max) t)
      (message "got %s" (match-string 2))
	(replace-match (int-to-string (1+ (string-to-int (match-string 2))))
		       t t nil 2))
    nil))

(add-hook 'write-file-hooks 'save-count-update)

;;; end of save-count.el
