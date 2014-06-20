;;; tts.el --- Text to speech

;; Copyright (C) 2008  John Sturdy

;; Author: John Sturdy <john.sturdy@ul.ie>
;; Keywords: multimedia, languages

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

(defun tts-sort-patterns (patterns)
  "Sort PATTERNS into the order needed for `tts-string-to-phones'."
  (sort patterns
	(lambda (a b)
	  (< (length (car a))
	     (length (car b))))))

(defun tts-string-to-phones (text patterns)
  "In TEXT, replace things in PATTERNS.
PATTERNS is a list of conses of pattern and replacement.
It should be sorted by length of pattern.
If a pattern contains \"\(...\)\", the replacement for it
replaces just that part."
  (dolist (pattern-pair patterns)
    (let ((pattern (car pattern-pair))
	  (replacement (cdr pattern-pair)))
      (while (string-match pattern text)
	(replace-match replacement t t
		       text
		       (if (match-beginning 1)
			   1
			 nil)))))
  text)

(defun tts-phones-to-files (phones)
  "Convert PHONES (a string) to a list of files."
  (let ((result (format "0%c.%s" (aref phones 0) tts-sound-file-extension)))
    (dotimes (i (1- (length phones)))
      (push (format "%c%c.%s" (aref phones i) (aref (phones (1+ i))) tts-sound-file-extension)
	    result))
    (push (format "%c0.%s" (aref phones (1- (length phones))) tts-sound-file-extension)
	  result)
    (nreverse result)))

(defun tts-play-files (files)
  "Play the sounds in FILES."
  (mapcar (lambda (file)
	    (play-sound :file (expand-file-name file tts-sound-file-directory)))
	  files))

(provide 'tts)
;;; tts.el ends here
