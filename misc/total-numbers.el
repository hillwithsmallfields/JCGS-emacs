;;;; total-numbers.el -- add up all the numbers found in matches for a given pattern
;;; Time-stamp: <2005-08-12 14:44:40 jcgs>

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

(provide 'total-numbers)

;;;###autoload
(defun total-numbers-region (begin end pattern sub-index)
  "Count all the matches for numbers between BEGIN and END, that are in the sub-pattern of PATTERN given by SUB-INDEX."
  (interactive
   (list (region-beginning) (region-end)
	 (read-from-minibuffer "Pattern: " "\\([-0-9]+\\)")
	 (string-to-int (read-from-minibuffer "Sub-pattern index: " "1"))))
  (save-excursion
    (goto-char begin)
    (let ((total 0)
	  (count 0))
      (while (re-search-forward pattern end t)
	(setq total (+ total (string-to-int (match-string-no-properties sub-index)))
	      count (1+ count)))
      (message "%d numbers totalled %d" count total)
      total)))

;;; end of total-numbers.el
