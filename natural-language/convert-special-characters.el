;;;; convert-special-characters.el -- convert utf8 into HTML, TeX etc
;;; Time-stamp: <2006-01-25 10:39:39 jcgs>

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

(provide 'convert-special-characters)

(defvar utf8-re-encodings
  '(("à" "agrave " (TeX . "\agrave ") (html . "&agrave;"))
    ("á" "a acute" (TeX . "\aacute ") (html . "&aacute;"))
    ("ä" "a umlaut" (TeX . "\aumlaut ") (html . "&auml;"))
    ("å" "a ring" (TeX . "\aring ") (html . "&aring;"))
    ("è" "e grave" (TeX . "\egrave ") (html . "&egrave;"))
    ("é" "e acute" (TeX . "\eacute ") (html . "&eacute;"))
    ("ë" "e umlaut" (TeX . "\eumlaut ") (html . "&euml;"))
    ("ê" "e hat" (TeX . "\ehat ") (html . "&ehat;"))
    ("ì" "i grave" (TeX . "\igrave ") (html . "&igrave;"))
    ("í" "i acute " (TeX . "\iacute ") (html . "&iacute;"))
    ("ò" "o grave" (TeX . "\ograve ") (html . "&ograve;"))
    ("ó" "o acute" (TeX . "\oacute ") (html . "&oacute;"))
    ("ö" "o umlaut" (TeX . "\oumlaut ") (html . "&ouml;"))
    ("ø" "o slash" (TeX . "\oslash ") (html . "&oslash;"))
    ("ù" "u grave" (TeX . "\ugrave ") (html . "&ugrave;"))
    ("ú" "u acute" (TeX . "\uacute ") (html . "&uacute;"))
    ("ü" "u umlaut" (TeX . "\uumlaut ") (html . "&uuml;"))
    ;; ("l" "dark l" (TeX . "\darkl ") (html . "&darkl;"))
    ;; ("" "s caron" (TeX . "\scaron ") (html . "&scaron;"))
    ;; ("" "z acute" (TeX . "\zacute ") (html . "&zacute;"))
    ;; ("" "z dot" (TeX . "\zdot ") (html . "&zdot;"))
    ("þ" "thorn symbol" (TeX . "\thorn ") (html . "&thorn;"))
    ("ð" "biddap symbol" (TeX . "\biddap") (html . "&biddap;"))
    ("æ" "ae lig" (TeX . "\aelig") (html . "&aelig;")))
  "Re-encodings of utf8 characters to markup.")

(defun convert-special-characters (markup)
  "Convert special characters to MARKUP."
  (dolist (character utf8-re-encodings)
    (goto-char (point-min))
    (while (search-forward (car character) (point-max) t)
      (let ((replacement (cdr (assoc markup character))))
	(if replacement
	  (replace-match replacement))))))

;;; end of convert-special-characters.el
