;;;; parts-of-speech.el -- set parts of speech in a vocab file
;;; Time-stamp: <2006-09-26 21:35:55 jcgs>

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

(defvar part-of-speech-letters
  '((?a . "adverb")
    (?n . "noun")
    (?v . "verb")
    (?j . "adjective")
    (?p . "preposition")
    (?z . "probably a mistake"))
  "Alist of letters to parts of speech")

(defun set-parts-of-speech ()
  "Set parts of speech interactively by pressing a key for each entry."
  (interactive)
  (while (re-search-forward "^\\\"\\\"," (point-max) t)
    (let* ((key (save-match-data (read-char)))
	   (pos (cdr (assoc key part-of-speech-letters))))
      (if pos
	  (replace-match
	   (concat "\"" pos "\",")
	   t t)
	
	))))

(provide 'parts-of-speech)

;;; end of parts-of-speech.el
