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

(defconst gospel-lengths
  '(("Matthew" . 28)
    ("Mark" . 16)
    ("Luke" . 24)
    ("John" . 21)))

(defun gospel-chapter (chapter-number)
  "Return a gospel chapter by number.
Argument CHAPTER-NUMBER is the number in the overall sequence of gospels."
  (let* ((gospels gospel-lengths)
         (begin 1)
         (end (cdar gospels)))
    (catch 'found
      (while gospels
        (when (and (<= begin chapter-number)
                   (<= chapter-number end))
          (throw 'found (format "%s %d"
                                (caar gospels)
                                (1+ (- chapter-number begin)))))
        (setq begin (1+ end)
              gospels (cdr gospels)
              end (+ end (or (cdar gospels) 0))))
      nil)))

(defun proverbs-for-day (day-of-cycle)
  "Return the proverbs for DAY-OF-CYCLE."
    (list (format "Proverbs %d" (% day-of-cycle 31))))

(defun gospels-for-day (day-of-cycle)
  "Return the gospels for DAY-OF-CYCLE."
    (list (gospel-chapter day-of-cycle)))

(defconst favourite_psalms
  [1 4 8 9 11
   14 15 16 17 19
   20 21 23 25 27
   29 30 31 32 33
   34 139 40 41 42
   43 45 46 47 48
   49 51])

(defun psalms-for-day (day-of-cycle)
  "Return the psalms for DAY-OF-CYCLE."
  (let ((double-day (* day-of-cycle 2)))
    (if (<= double-day 150)
        (list (format "Psalm %d" (1- double-day))
              (format "Psalm %d" double-day))
      (list (aref favourite_psalms (- double-day 151))
            (aref favourite_psalms (- double-day 150))))))

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
