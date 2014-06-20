;;;; romanize.el -- find the romanization for characters e.g. pinyin for Chinese
;;; Time-stamp: <2007-08-25 23:51:18 jcgs>

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


;;; Commentary:
;; Uses the input method data to describe non-western characters in
;; terms of western ones.


;;; History:
;; Started by John C. G. Sturdy 2007-07-07


;;; Code:
(defvar romanization-hash-table nil
  "Hash table of the pinyin for characters.")

(defvar romanization-pinyin-tonal-letters
  (vector nil
	  "ābcdēfghījklmnōpqrstūvwxyz"
	  "ábcdéfghíjklmnópqrstúvwxyz"
	  "ăbcdĕfghĭjklmnŏpqrstŭvwxyz"
	  "àbcdèfghìjklmnòpqrstùvwxyz")
  "Tonal letters for tonepy back-conversion.")

(defun romanize-pinyin-accented-string (string)
  "Return an accented version of pinyin STRING."
  ;; todo: fill this in
  ;; I think the rule is to accent the first vowel unless it is "i" in
  ;; which case accent the next one
  (if (string-match "[1234]$" string)
      (let ((tone-letters (aref romanization-pinyin-tonal-letters
				(- (aref string (1- (length string))) ?0)))
	    (copy-of-string (substring string 0 -1)))
	(cond
	 ((string-match "[aeou][aeiou]$" copy-of-string)
	  (let ((index (- (length copy-of-string) 2)))
	    (aset copy-of-string
		  index
		  (aref tone-letters (- (aref string index) ?a))))
	  copy-of-string)
	 ((string-match "[^aeou][aeiou]$" copy-of-string)
	  (let ((index (1- (length copy-of-string))))
	    (aset copy-of-string
		  index
		  (aref tone-letters (- (aref string index) ?a))))
	  copy-of-string)
	 (t string)))
    string))

(defun romanize-pinyin-analyze-map (map)
  "Analyze the pinyin-map MAP."
  (let ((pinyin-chars-string (concat pinyin-chars-string
				     (char-to-string (car map)))))
    (if (vectorp (cadr map))
	(let ((han-chars (cadr map))
	      (neat-string (romanize-pinyin-accented-string pinyin-chars-string)))
	  (mapc (function
		 (lambda (han-char)
		   (puthash (aref han-char 0)
			    neat-string
			    romanization-hash-table)))
		han-chars))
      (mapcar 'romanize-pinyin-analyze-map (cddr map)))))

(defun romanize-pinyin-initialize ()
  "Add pinyin to the romanization table, if not yet done.
Interactively, or with optional argument from program, do it anyway."
  (interactive)
  (when (or (not (boundp 'quail-package-alist))
	    (not (assoc "chinese-tonepy" quail-package-alist)))
    (load "quail/TONEPY"))
  (let ((pinyin-raw-data (cdr (assoc "chinese-tonepy" quail-package-alist))))
    (if (null pinyin-raw-data)
	;; todo: may be possible to get it loaded?
	(error "Could not find chinese-tonepy data")
      (setq romanization-hash-table (make-hash-table :test 'eq))
      (let ((pinyin-chars-string ""))
	(mapcar 'romanize-pinyin-analyze-map (cdr (cadr pinyin-raw-data)))))))

(defun romanize-kana-initialize ()
  "Add kana to the romanization table, if not yet done.
Interactively, or with optional argument from program, do it anyway."
  (interactive)
  (when (or (not (boundp 'quail-package-alist))
	    (not (assoc "japanese-hiragana" quail-package-alist)))
    (load "quail/japanese"))
  (mapc (function
	 (lambda (rule)
	   (when (stringp (cadr rule))
	     (puthash (aref (cadr rule) 0)
		      (car rule)
		      romanization-hash-table))))
	quail-japanese-transliteration-rules))

(defun romanize-initialize (&optional force)
  "Initialize the romanization data.
Optional argument FORCE to do it even if already done."
  (interactive
   (list t))
  (when (or force (null romanization-hash-table))
    (romanize-pinyin-initialize)
    (romanize-kana-initialize)))

(defun romanize-dump ()
  "Dump the romanization data, for debugging."
  (interactive)
  (with-output-to-temp-buffer "*Pinyin*"
    (romanize-initialize)
    (let ((pinyin nil))
      (maphash (lambda (k v)
		 (push (cons v (char-to-string k)) pinyin))
	       romanization-hash-table)
      (dolist (p (sort pinyin
		       (function
			(lambda (a b)
			  (if (string= (car a) (car b))
			      (string< (cdr a) (cdr b))
			    (string< (car a) (car b)))))))
	(princ (format "%S: %S\n" (car p) (cdr p)))))))

(defun romanize-lookup (char)
  "Find the romanization of CHAR."
  (romanize-pinyin-initialize)
  (if (stringp char)
      (mapconcat 'pinyin-lookup char "")
    (gethash char romanization-hash-table)))

(defun romanize-char-at-point (where)
  "Show the romanization of the character at WHERE (point, interactively)."
  (interactive "d")
  (let* ((char (char-after where))
	 (romanized (romanize-lookup char)))
    (if romanized
	(message "%c is %s" char romanized)
      (message "No romanization found for %c" char))))

(provide 'romanize)

;;; romanize.el ends here
