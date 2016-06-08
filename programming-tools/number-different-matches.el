;;; number-different-matches.el --- replace different matches for a regexp with different numbers

;; Copyright (C) 2016  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: data, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Originally writted to replace all long hex numbers in a file with
;; numbers allocated in frequency order, to make comparing stderr
;; traces from program runs (where pointers will be different but not
;; significant) easier.

;;; Code:

(defun number-different-matches-region (from to regexp fmt)
  "Replace each match between FROM and TO for REGEXP according to FMT."
  (setq to (copy-marker to))
  (let ((matches (make-hash-table :test 'equal))
	(as-list)
	(i 0))
    (goto-char from)
    (while (re-search-forward regexp to t)
      (let ((match (match-string-no-properties 0)))
	(puthash match
		 (1+ (gethash match matches 0))
		 matches)))
    (maphash (lambda (k v)
	       (push (cons v k)
		     as-list))
	     matches)
    (dolist (pair (nreverse (sort as-list 'car-less-than-car)))
      (puthash (cdr pair)
	       i
	       matches)
      (setq i (1+ i)))
    (goto-char from)
    (while (re-search-forward regexp to t)
      (replace-match (format fmt
			     (gethash (match-string-no-properties 0)
				      matches))))
    (move-marker to nil)))

(defun number-different-pointers ()
  "Replace pointers in buffer with tokens."
  (interactive)
  (number-different-matches-region (point-min) (point-max)
				   "0x[0-9a-f]\\{5,\\}" "ptr%d"))

(provide 'number-different-matches)
;;; number-different-matches.el ends here
