;;;; words-in-elisp-symbols.el -- Analyze the words used in elisp symbols
;;; Time-stamp: <2005-02-14 11:00:47 john>

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

(provide 'words-in-elisp-symbols)

(defvar elisp-symbol-words nil
  "Obarray for the words used in elisp symbols.
The value of each is the number of times it has appeared.")

(defun words-in-elisp-symbols ()
  "Analyze the words used in elisp symbols."
  (interactive)
  (setq elisp-symbol-words (make-vector 1511 nil))
  (mapatoms
   (lambda (atom)
     (let ((words (split-string (symbol-name atom) "-")))
       (while words
	 ;; (message "word: %s" (car words))
	 (let* ((word-symbol (intern (car words) elisp-symbol-words))
		(word-count (if (boundp word-symbol) (symbol-value word-symbol) 0)))
	   (if (not (eq word-symbol nil))
	       (set word-symbol
		    (if (numberp word-count)
			(1+ word-count)
		      1))))
	 (setq words (cdr words))))))
  (let ((word-list nil))
    (mapatoms (lambda (wordsym)
		(if (and (boundp wordsym)
			 (integerp (symbol-value wordsym)))
		    (setq word-list (cons (cons wordsym (symbol-value wordsym))
					  word-list))
		  ))
	      elisp-symbol-words)
    (setq word-list (sort word-list (lambda (a b) (> (cdr a) (cdr b)))))
    (with-output-to-temp-buffer "*Emacs words*"
      (while word-list
	(princ (format "%s: %d\n" (caar word-list) (cdar word-list)))
	(setq word-list (cdr word-list))))))

;;; end of words-in-elisp-symbols.el
