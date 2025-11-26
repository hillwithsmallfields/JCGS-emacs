;;; cyclic-readings.el --- select Bible readings for a day of the month  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Selects psalms and gospel readings by the day of the month

;;; Code:

(defconst old-testament-lengths
  '(("Ecclesiastes" . 12)
    ("Song of Solomon" . 8)
    ("Ecclesiastes" . 12)))

(defconst gospel-lengths
  '(("Matthew" . 28)
    ("Mark" . 16)
    ("Luke" . 24)
    ("John" . 21)))

(defun book-sequence-chapter (book-lengths chapter-number)
  "Return a book chapter by number in a sequence of books.
BOOK-LENGTHS is a list of pairs of books and their numbers of chapters.
Argument CHAPTER-NUMBER is the number in the overall sequence of gospels."
  (let* ((books book-lengths)
         (begin 1)
         (end (cdar books)))
    (catch 'found
      (while books
        (when (and (<= begin chapter-number)
                   (<= chapter-number end))
          (throw 'found (format "%s %d"
                                (caar books)
                                (1+ (- chapter-number begin)))))
        (setq begin (1+ end)
              books (cdr books)
              end (+ end (or (cdar books) 0))))
      nil)))

(defun old-testament-chapter (chapter-number)
  "Return an Old Testament chapter by number."
  (book-sequence-chapter old-testament-lengths chapter-number))

(defun gospel-chapter (chapter-number)
  "Return a gospel chapter by number.
Argument CHAPTER-NUMBER is the number in the overall sequence of gospels."
  (book-sequence-chapter gospel-lengths chapter-number))

(defun proverbs-for-day (day-of-cycle)
  "Return the proverbs for DAY-OF-CYCLE."
  (list (cond
         ((<= day-of-cycle 89) (format "Proverbs %d" (% day-of-cycle 31)))
         ((= day-of-cycle 90) "Proverbs 28")
         (t "Proverbs 30"))))

(defun gospels-for-day (day-of-cycle)
  "Return the gospels for DAY-OF-CYCLE."
  (list (cond ((<= day-of-cycle 89) (gospel-chapter day-of-cycle))
              ((= day-of-cycle 90) "Proverbs 29")
              (t "Proverbs 31"))))

(defun psalms-for-day (day-of-cycle)
  "Return the psalms for DAY-OF-CYCLE."
  (let ((double-day (* day-of-cycle 2)))
    (if (<= double-day 150)
        (list (format "Psalm %d" (1- double-day))
              (format "Psalm %d" double-day))
      (list (old-testament-chapter (- double-day 151))
            (old-testament-chapter (- double-day 150))))))

(defun day-of-year-to-day-of-cycle (day-of-year)
  "Convert a day of the year to a day of 91-day cycle."
  (1+ (% (1- day-of-year) 91)))

(defun readings-for-day (&optional day-of-cycle)
  "Return the readings for DAY-OF-CYCLE, as a list."
  (let* ((day-of-cycle (or day-of-cycle
                           (day-of-year-to-day-of-cycle
                            (time-to-day-in-year (current-time))))))
    (nconc (psalms-for-day day-of-cycle)
           (proverbs-for-day day-of-cycle)
           (gospels-for-day day-of-cycle))))

(defun readings-for-day-string (&optional day-of-cycle)
  "Return the readings for DAY-OF-CYCLE, as a string."
  (mapconcat 'identity (readings-for-day day-of-cycle) ", "))

(provide 'cyclic-readings)
;;; cyclic-readings.el ends here
