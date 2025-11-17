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

(defun gospels-for-day (day-in-month)
  "Return the gospels for DAY-IN-MONTH."
  (let ((base (1+ (* (1- day-in-month) 3))))
    (list (gospel-chapter base)
          (gospel-chapter (1+ base))
          (gospel-chapter (+ 2 base)))))

(defun psalms-for-day (day-in-month)
  "Return the psalms for DAY-IN-MONTH."
  (let ((base (1+ (* (1- day-in-month) 5))))
    (mapcar (lambda (n) (if (<= n 150)
                            (format "Psalm %d" n)
                          nil))
            (number-sequence base (+ base 5)))))

(defun readings-for-day (day-in-month)
  "Return the readings for DAY-IN-MONTH, as a list."
  (delq nil (nconc (psalms-for-day day-in-month)
                   (gospels-for-day day-in-month))))

(defun readings-for-day-string (day-in-month)
  "Return the readings for DAY-IN-MONTH, as a string."
  (mapconcat 'identity (readings-for-day day-in-month) ", "))

(provide 'cyclic-readings)
;;; cyclic-readings.el ends here
