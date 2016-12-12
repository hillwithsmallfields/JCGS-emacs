;;;; fortune.el -- fortune file handling
;;; Time-stamp: <2006-01-29 18:18:31 jcgs>

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

(provide 'fortune)

(require 'cl)

;; todo: merge with strfile.el

(defvar fortune-strings nil
  "The fortune strings for this file.")

(make-variable-buffer-local 'fortune-strings)

(defun open-fortune-file (file)
  "Open FILE."
  (interactive "fFortune file: ")
  (find-file file)
  (add-hook 'local-write-file-hooks 'fortune-file-write-hook)
  (if (null fortune-strings)
      (let ((strings nil)
	    (prev 1))
	(goto-char (point-min))
	(while (re-search-forward "^%$" (point-max) t)
	  (push (buffer-substring-no-properties prev (- (point) 2))
		strings)
	  (setq prev (1+ (point))))
	(push (buffer-substring-no-properties prev (point-max)) strings)
	(setq fortune-strings (apply 'vector strings)))))

(defun fortune-file-write-hook ()
  "Thing to call on saving a fortune file."
  (setq fortune-string nil))

(defvar fortune-delay 2
  "*How long to wait after each fortune, if not interactive.")

(defun fortune (file &optional format)
  "Get a fortune from FILE. Optionally use FORMAT to display it."
  (interactive "fFortune file: ")
  (let* ((strings-and-buffer
	  (save-window-excursion
	    (open-fortune-file file)
	    (cons fortune-strings (current-buffer))))
	 (strings (car strings-and-buffer))
	 (buffer (cdr strings-and-buffer))
	 (n (random (length strings)))
	 (string (aref strings n)))
    (bury-buffer buffer)
    (message (or format "%s") string)
    (if (not (interactive-p)) (sit-for fortune-delay))
    string))

(defun fortunes-matching (file pattern &optional display)
  "Get the fortunes from FILE that match PATTERN."
  (interactive "fFortune file:
sPattern: ")
  (let* ((strings-and-buffer
	  (save-window-excursion
	    (open-fortune-file file)
	    (cons fortune-strings (current-buffer))))
	 (strings (car strings-and-buffer))
	 (buffer (cdr strings-and-buffer)))
    (bury-buffer buffer)
    (let ((result nil)
	  (case-fold-search t)
	  (i (1- (length strings))))
      (while (>= i 0)
	(message "%s" (aref strings i))
	(if (string-match pattern (aref strings i))
	    (setq result (cons (aref strings i) result)))
	(decf i))
      (if (or display (interactive-p))
	  (with-output-to-temp-buffer "*Fortunes*"
	    (let ((x result))
	      (while x
		(princ (car x)) (princ "\n")
		(setq x (cdr x))))))
      result)))

;;; end of fortune.el
