;;; vocab-text-mode.el --- major mode for entering vocab lists

;; Copyright (C) 2008  John Sturdy

;; Author: John Sturdy <john.sturdy@ul.ie>
;; Keywords: i18n, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;

;;; Code:

(defun vocab-text-mode-next-line (&optional arg)
  "Move to the next line, or create one."
  (interactive)
  (if (eobp)
      (newline arg)
    (next-line arg)))

(defvar vocab-text-mode-columns nil
  "Sorted alist of column numbers to input methods.")

(defun vocab-text-mode-next-column ()
  "Move to the next column, selecting the appropriate input method.
Columns are defined by where the input method was changed."
  (interactive)
  (let ((col (current-column))
	(cols vocab-text-mode-columns)
	(last-col (if cols
		      (cdar cols)
		    0))
	(last-col-but-one last-col))
    (while (and cols (< (caar cols) col))
      (setq last-col-but-one last-col
	    last-col (cdar cols)
	    cols (cdr cols)))
    (if (null cols)
	(move-to-column (+ last-col (- last-col last-col-but-one)))
      (move-to-column (caar cols) t)
      (set-input-method (cdar cols)))))

(defun vocab-text-mode-change-input-method-function ()
  "On changing input method, associate it with that column."
  (let ((pair (assoc (current-column) vocab-text-mode-columns)))
    (if pair
	(rplacd pair current-input-method)
      (setq vocab-text-mode-columns
	    (sort (cons (cons (current-column)
			      current-input-method)
			vocab-text-mode-columns)
		  (function
		   (lambda (a b)
		     (< (car a) (car b)))))))))

(defvar vocab-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'vocab-text-mode-next-line)
    (define-key map "\t" 'vocab-text-mode-next-column)
    map))

(define-derived-mode vocab-text-mode nil "Vocab text"
  "Major mode for entering vocabulary lists in columns."
  (add-hook 'input-method-activate-hook 'vocab-text-mode-change-input-method-function nil t)
  )

(provide 'vocab-text-mode)
;;; vocab-text-mode.el ends here
