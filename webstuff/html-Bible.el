;;; html-Bible.el

;;; <title>Insert Bible reference URLs</title>

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

(provide 'html-Bible)

(defun spaces-to-pluses (string)
  "Return STRING with any spaces replaced with plus signs
as used in CGI arguments."
  (let ((i (1- (length string))))
    (while (>= i 0)
      (if (= (aref string i) ? )
	  (aset string i ?+))
      (setq i (1- i)))))

;;;###autoload
(defun Bible-ref-region (a b)
  "Turn the region into an HTML Bible citation.
Takes begin and end of region as arguments."
  (interactive "r")
  (let ((ref (spaces-to-pluses (buffer-substring a b))))
    (goto-char b)
    (insert "</a>")
    (goto-char a)
    (insert "<a href=\"http://www.gospelcom.net/cgi-bin/bible?language=English&version=KJV&passage="
	    ref
	    "\">")))

;;;###autoload
(defun html-insert-Bible-ref (book chapter start-verse
				   &optional end-verse
				   start-marker end-marker)
  "Insert reference to BOOK CHAPTER START-VERSE &OPTIONAL END-VERSE."
  (interactive
   "sBook: 
nChapter: 
sVerse: 
sEnd verse: ")
  (if (stringp chapter) (setq chapter (string-to-number chapter)))
  (let ((endstart (string-match "[-,]\\([0-9]+\\)$" start-verse)))
    (if (and endstart (null end-verse))
	(progn
	  (setq end-verse (substring start-verse (1+ endstart))
		start-verse (substring start-verse 0 endstart)))))
  (setq start-verse
	(if (string= start-verse "")
	    nil
	  (string-to-number start-verse)))
  (if end-verse
      (progn
	(setq end-verse
	      (if (string= end-verse "")
		  nil
		(string-to-number end-verse)))
	(if (or (eq start-verse end-verse)
		(zerop end-verse))
	    (setq end-verse nil))))
  (let ((ref-string
	 (format
	  (if start-verse
	      (if end-verse
		  "%s+%d:%d-%d"
		"%s+%d:%d")
	    "%s+%d")
	  book chapter start-verse end-verse))
	(anchor-string
	 (format
	  (if start-verse
	      (if end-verse "%s %d:%d-%d" "%s %d:%d")
	    "%s %d")
	  book chapter start-verse end-verse)))
    (insert
     (format "<a href=\"http://www.gospelcom.net/cgi-bin/bible?%s\">%s%s%s</a>"
	     ref-string
	     (if start-marker start-marker "")
	     anchor-string
	     (if end-marker end-marker "")))))

;;;###autoload
(defun html-convert-Bible-ref ()
  "Convert reference in [...] around point to a Bible gateway reference."
  (interactive)
  (save-excursion
    (let (start end)
      (if (and (setq start (search-backward "[" (point-min) t))
	       (setq end (re-search-forward
			  "\\([1-3 A-Za-z]+\\) \\([0-9]+\\):\\([-,0-9]+\\)\\]"
			  (point-max) t)))
	  (let ((book (buffer-substring (match-beginning 1)
					(match-end 1)))
		(chapter (buffer-substring (match-beginning 2)
					   (match-end 2)))
		(verses (buffer-substring (match-beginning 3)
					  (match-end 3))))
	    ;; (message "Book=\"%s\" chapter=\"%s\" verses=\"%s\"" book chapter verses) (sit-for 2)
	    (delete-region start end)
	    (html-insert-Bible-ref book chapter verses nil "[" "]"))))))

