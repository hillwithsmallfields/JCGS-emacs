;;;; convert-base.el -- convert number strings between bases
;;; Time-stamp: <2006-03-06 08:27:59 jcgs>

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

(provide 'convert-base)

(defun read-base-string (string base)
  "Convert STRING to a number, treating it as BASE digits."
  (interactive "sNumber: 
NBase: ")
  (let ((number 0)
	(length (length string))
	(i 0))
    (while (< i length)
      (let* ((char (aref string i))
	     (digit (cond
		     ((and (<= ?0 char)
			   (<= char ?9))
		      (- char ?0))
		     ((and (<= ?a char)
			   (<= char ?z))
		      (+ 10 (- char ?a)))
		     ((and (<= ?A char)
			   (<= char ?Z))
		      (+ 10 (- char ?A)))
		     (t (error "Cannot make a digit from %c" char)))))
	(when (>= digit base)
	  (error "%c is not a valid digit in base %d" char base))
	(setq number 
	      (+ (* number base)
		 digit)))
      (setq i (1+ i)))
    (when (interactive-p) (message "%s in base %d is %d in base 10" string base number))
    number))

(defun insert-decimal-number-from-hex (hex)
  "Read a number in hex, and insert it in decimal."
  (interactive "sHex number: ")
  (insert (int-to-string (read-base-string hex 16))))

;;; end of convert-base.el
