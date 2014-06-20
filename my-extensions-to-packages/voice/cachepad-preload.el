;;;; cachepad-preload.el
;;; load the cachepad with words from a given region
;;; Time-stamp: <2004-12-15 09:21:36 jcgs>

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

(defun cachepad-preload (start end)
  "Preload the cachepad with words between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((words nil))
      (while (re-search-forward "\\<\\(\\w+\\)\\>" end t)
	(let ((word (buffer-substring-no-properties (match-beginning 1)
						    (match-end 1))))
	  (pushnew word words :test 'string=)))
      (setq words (sort words 'string<))
      (let* ((nwords (length words))
	     (longestword (apply 'max (mapcar 'length words))))
	(setq cache-num-columns (max 1 (/ (frame-width) longestword))
	      cache-num-rows (1+ (/ nwords cache-num-columns)))
	(message "%d words, longest %d chars, giving %d columns and %d rows"
		 nwords longestword cache-num-columns cache-num-rows)
	)
      (mapcar 'insert-word-into-cache-pad words))))

(defun cachepad-defun ()
  "Load the cachepad from the current function."
  (interactive)
  (save-excursion
    (unless (looking-at "^\\s(") (beginning-of-defun))
    (let ((start (point)))
      (end-of-defun 1)
      (cachepad-preload start (point)))))
