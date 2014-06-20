;;; creates lectionaries

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

(require 'cl)

(setq
 book-lengths				; also contains month lengths
 '(("1 Chronicles" . 29)
   ("1 Corinthians" . 16)
   ("1 John" . 5)
   ("1 Kings" . 22)
   ("1 Peter" . 5)
   ("1 Samuel" . 31)
   ("1 Thessalonians" . 5)
   ("1 Timothy" . 6)
   ("2 Chronicles" . 36)
   ("2 Corinthians" . 13)
   ("2 John" . 1)
   ("2 Kings" . 25)
   ("2 Peter" . 3)
   ("2 Samuel" . 24)
   ("2 Thessalonians" . 3)
   ("2 Timothy" . 4)
   ("3 John" . 1)
   ("Acts" . 28)
   ("Amos" . 9)
   ("April" . 30)
   ("August" . 31)
   ("Colossians" . 4)
   ("Daniel" . 12)
   ("December" . 31)
   ("Deuteronomy" . 34)
   ("Ecclesiastes" . 12)
   ("Ephesians" . 6)
   ("Esther" . 10)
   ("Exodus" . 40)
   ("Ezekiel" . 48)
   ("Ezra" . 10)
   ("February" . 28)
   ("Galatians" . 6)
   ("Genesis" . 50)
   ("Habakkuk" . 3)
   ("Haggai" . 2)
   ("Hebrews" . 13)
   ("Hosea" . 14)
   ("Isaiah" . 66)
   ("James" . 5)
   ("January" . 31)
   ("Jeremiah" . 52)
   ("Job" . 42)
   ("Joel" . 3)
   ("John" . 21)
   ("Jonah" . 4)
   ("Joshua" . 24)
   ("Jude" . 1)
   ("Judges" . 21)
   ("July" . 31)
   ("June" . 30)
   ("Lamentations" . 5)
   ("Leviticus" . 27)
   ("Luke" . 24)
   ("Malachi" . 4)
   ("March" . 31)
   ("Mark" . 16)
   ("Matthew" . 28)
   ("May" . 31)
   ("Micah" . 7)
   ("Nahum" . 3)
   ("Nehemiah" . 13)
   ("November" . 30)
   ("Numbers" . 36)
   ("Obadiah" . 1)
   ("October" . 31)
   ("Philemon" . 1)
   ("Philippians" . 4)
   ("Proverbs" . 31)
   ("Psalms" . 150)
   ("Revelation" . 22)
   ("Romans" . 16)
   ("Ruth" . 4)
   ("September" . 30)
   ("Song of Solomon" . 8)
   ("Titus" . 3)
   ("Zechariah" . 14)
   ("Zephaniah" . 3))
 OT-books
 '("Genesis"
   "Exodus"
   "Leviticus"
   "Numbers"
   "Deuteronomy"
   "Joshua"
   "Judges"
   "Ruth"
   "1 Samuel"
   "2 Samuel"
   "1 Kings"
   "2 Kings"
   "1 Chronicles"
   "2 Chronicles"
   "Ezra"
   "Nehemiah"
   "Esther"
   "Proverbs"
   "Job"
   "Ecclesiastes"
   "Song of Solomon"
   "Isaiah"
   "Jeremiah"
   "Lamentations"
   "Ezekiel"
   "Daniel"
   "Hosea"
   "Joel"
   "Amos"
   "Obadiah"
   "Jonah"
   "Micah"
   "Nahum"
   "Habakkuk"
   "Zephaniah"
   "Haggai"
   "Zechariah"
   "Malachi")
  OT-books-except-wisdom
 '("Genesis"
   "Exodus"
   "Leviticus"
   "Numbers"
   "Deuteronomy"
   "Joshua"
   "Judges"
   "Ruth"
   "1 Samuel"
   "2 Samuel"
   "1 Kings"
   "2 Kings"
   "1 Chronicles"
   "2 Chronicles"
   "Ezra"
   "Nehemiah"
   "Esther"
   "Isaiah"
   "Jeremiah"
   "Lamentations"
   "Ezekiel"
   "Daniel"
   "Hosea"
   "Joel"
   "Amos"
   "Obadiah"
   "Jonah"
   "Micah"
   "Nahum"
   "Habakkuk"
   "Zephaniah"
   "Haggai"
   "Zechariah"
   "Malachi")
 Psalms-books
 '("Psalms")
 Wisdom-books
 '("Psalms"
   "Proverbs"
   "Job"
   "Ecclesiastes"
   "Song of Solomon")
 Gospel-books
 '("Matthew"
   "Mark"
   "Luke"
   "John")
 Apostolic-books
 '("Acts"
   "Romans"
   "1 Corinthians"
   "2 Corinthians"
   "Galatians"
   "Ephesians"
   "Philippians"
   "Colossians"
   "1 Thessalonians"
   "2 Thessalonians"
   "1 Timothy"
   "2 Timothy"
   "Titus"
   "Philemon"
   "Hebrews"
   "James"
   "1 Peter"
   "2 Peter"
   "1 John"
   "2 John"
   "3 John"
   "Jude"
   "Revelation")
 NT-all-books
 '("Matthew"
   "Mark"
   "Luke"
   "John"
   "Acts"
   "Romans"
   "1 Corinthians"
   "2 Corinthians"
   "Galatians"
   "Ephesians"
   "Philippians"
   "Colossians"
   "1 Thessalonians"
   "2 Thessalonians"
   "1 Timothy"
   "2 Timothy"
   "Titus"
   "Philemon"
   "Hebrews"
   "James"
   "1 Peter"
   "2 Peter"
   "1 John"
   "2 John"
   "3 John"
   "Jude"
   "Revelation")
 )

(defstruct reading-state
  books
  books-remaining
  chapter
  per-day
  format
  cyclic)

(defun new-lectionary-reading (format per-day book-list cyclic)
  "Make a new lectionary reading state record, for FORMAT, PER-DAY, BOOK-LIST, CYCLIC."
  (make-reading-state
   :books book-list
   :books-remaining book-list
   :chapter 1
   :per-day per-day
   :format format
   :cyclic cyclic))

(defun advance-to-next-book (reading)
  "Move READING on to the start of its next book."
  (let ((try-this (cdr (reading-state-books-remaining reading))))
    (if (and (null try-this)
	     (and (reading-state-cyclic reading)
		  (or (eq (reading-state-cyclic reading) t)
		      (progn
			(decf (reading-state-cyclic reading))
			(> (reading-state-cyclic reading) 0)))))
	(setq try-this (reading-state-books reading)))
    (setf (reading-state-books-remaining reading) try-this
	  (reading-state-chapter reading) 1)
    (if (null try-this)
	nil
      (car try-this))))

(defun step-lectionary-reading (reading)
  "Return the next chapter from READING as a cons of book.chapter,
advancing READING."
  (if (reading-state-books-remaining reading)
      (let* ((current-chapter (reading-state-chapter reading))
	     (current-book (car (reading-state-books-remaining reading)))
	     (current-book-length (cdr (assoc current-book book-lengths)))
	     (book-containing-chapter
	      (if (> current-chapter current-book-length)
		  (progn
		    (setq current-chapter 1)
		    (advance-to-next-book reading))
		current-book)))
	(setf (reading-state-chapter reading) (1+ current-chapter))
	(if (null book-containing-chapter)
	    nil
	  (cons book-containing-chapter current-chapter)))
    nil))

(defun step-and-output-daily-reading (reading)
  "Output the next day's reading from READING."
  (let ((countdown (reading-state-per-day reading))
	(book-and-chapter ))
    (while (and (> countdown 0)
		(setq book-and-chapter (step-lectionary-reading reading)))
      (insert (format (reading-state-format reading)
		      (car book-and-chapter)
		      (cdr book-and-chapter)))
      (decf countdown))
    book-and-chapter))

(defun step-readings (readings pre post)
  "Step all of READINGS once, constructing the relevant text with PRE and POST ambles."
  (insert pre)
  (prog1
      (reduce (function (lambda (a b) (or a b)))
	      (mapcar 'step-and-output-daily-reading readings))
    (insert post)))

;;;###autoload
(defun lectionary (&optional reduced)
  "Make my lectionary."
  (interactive "P")
  (switch-to-buffer (get-buffer-create (format "*%s lectionary*" (if reduced "reduced" "full"))))
  (erase-buffer)
  (setq day-rota (new-lectionary-reading
		  "  <th>%s %d</th> " 1
		  '("January" "February" "March" "April" "May" "June"
		    "July" "August" "September" "October" "November" "December") nil)
	OT-rota (new-lectionary-reading "<td>%s %d</td> " 2 OT-books-except-wisdom nil)
	OT-rota-with-wisdom (new-lectionary-reading "<td>%s %d</td> " 2 OT-books nil)
	Wisdom-rota (new-lectionary-reading "<td>%s %d</td> " 1 Wisdom-books nil)
	Psalm-rota (new-lectionary-reading "<td>%s %d</td> " 1 Psalms-books 2)
	NT-rota (new-lectionary-reading "<td>%s %d</td> " 1 NT-all-books nil)
	Gospel-rota (new-lectionary-reading "<td>%s %d</td> " 1 Gospel-books 4)
	Apostolic-rota (new-lectionary-reading "<td>%s %d</td> " 1 Apostolic-books nil)
	)
  (insert "<table center>\n")
  (let ((the-readings (if reduced
			  (list day-rota
				OT-rota
				Wisdom-rota
				NT-rota)
			(list day-rota
			      OT-rota-with-wisdom
			      Psalm-rota
			      Gospel-rota
			      Apostolic-rota
			      ))))
    (while (and (step-readings the-readings "  <tr>" "</tr>\n")
		(if reduced
		    t
		  (reading-state-books-remaining day-rota)))
      (message "%s %d"
	       (car (reading-state-books-remaining day-rota))
	       (reading-state-chapter day-rota))
      nil))
  (insert "</table>\n"))
