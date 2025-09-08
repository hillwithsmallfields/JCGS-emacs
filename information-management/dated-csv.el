;;; dated-csv.el --- functions for CSV files with dates in the first column  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2025  John Sturdy

;; Author: John Sturdy <jsturdy@ccsl.com>
;; Keywords: convenience, data

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

;; When you start changing a row of the CSV file, this mode makes sure
;; that today's date is at the start of that line.x

;;; Code:

(require 'csv-mode)

(defmacro def-row-timestamper (name time-format &optional time-pattern)
  "Make a row timestamping function called NAME using TIME-FORMAT.
Optional argument TIME-PATTERN is a pattern to see whether this line has already been annotated."
  `(defun ,name (from to)
     "Make the line being changed start with a date string.
This is in ISO format, followed by a comma, suitable for
use in a CSV file.
Arguments FROM and TO mark the area being changed, for use
on `before-change-functions'."
     (when (or (eobp)
               (save-excursion
                 (beginning-of-line 2)
                 (eobp)))
       (let ((date-pattern (format-time-string ,(or time-pattern time-format)))
             (date-string (format-time-string ,time-format)))
         (message "date-pattern=%S date-string=%S" date-pattern date-string)
         (unless (save-excursion
                   (goto-char from)
                   (beginning-of-line)
                   (looking-at date-pattern))
           (beginning-of-line)
           (insert date-string)
           (end-of-line))))))

(def-row-timestamper force-row-datestamp "%F,")
(def-row-timestamper force-row-timestamp "%FT%R,,"
  "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9],,")

(defun dated-csv-normalize ()
  "Normalize the data in a dated CSV file."
  (interactive)
  (let* ((now (decode-time))
         (now-day (nth 3 now))
         (now-month (nth 4 now))
         (now-year (nth 5 now)))
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line 2)
      (while (re-search-forward "^\\(\\([0-9][0-9]\\)-\\)?\\([0-9][0-9]\\)," (point-max) t)
        (let* ((line-day (string-to-number (match-string-no-properties 3)))
               (line-month (match-string-no-properties 2))
               (month-number (if line-month
                                 (string-to-number line-month)
                               (+ now-month (if (>= line-day now-day) 0 1))))
               (year-number (+ now-year (if (>= month-number now-month) 0 1))))
          (beginning-of-line 1)
          (unless line-month
            (insert (format "%02d-" month-number)))
          (beginning-of-line 1)
          (insert (format "%4d-" year-number))))
      (goto-char (point-min))
      (beginning-of-line 2)
      (sort-lines nil (point) (point-max))))
  nil)

(define-derived-mode dated-csv-mode csv-mode "Dated CSV"
  "CSV mode with ISO dates in the first column.
Incomplete dates are filled in, and the entries sorted, when the file is saved."
  (make-local-variable 'write-file-functions)
  (add-hook 'write-file-functions 'dated-csv-normalize)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))
  
(define-derived-mode datestamped-csv-mode csv-mode "Datestamped CSV"
  "CSV mode with ISO dates in the first column.
When you start typing in the last row, today's date is inserted
at the start of the row if it is not already there."
  (make-local-variable 'before-change-functions)
  (add-hook 'before-change-functions 'force-row-datestamp nil t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))

(defun set-overwrite-column (begin end)
  "If in the first 16 columns, overwrite instead of inserting."
  (overwrite-mode (> (current-column) 16)))

(define-derived-mode timestamped-csv-mode csv-mode "Timestamped CSV"
  "CSV mode with ISO timedates in the first column.
When you start typing in the last row, this minute's timedate is
inserted at the start of the row if it is not already there."
  (make-local-variable 'before-change-functions)
  (add-hook 'before-change-functions 'force-row-timestamp nil t)
  ;; (add-hook 'before-change-functions 'set-overwrite-column nil t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))

(defun time-tracking-csv-mode-fill-in-durations ()
  "Fill in durations in a time-tracking CSV file."
  (interactive)
  (save-excursion
    (goto-line 2)
    (while (re-search-forward "^\\([-0-9]+T[:0-9]+\\),," (point-max) t)
      (let ((start-time-string (match-string-no-properties 1))
            (where-to-write-duration (+ (match-end 1) 1)))
        (when (save-excursion
                (beginning-of-line 2)
                (looking-at "^\\([-0-9]+T[:0-9]+\\),"))
          (let* ((end-time (parse-iso8601-time-string (concat (match-string-no-properties 1) ":00")))
                 (start-time (parse-iso8601-time-string (concat start-time-string ":00")))
                 (duration-string (format-seconds "%h:%m" (time-to-seconds (time-subtract end-time start-time)))))
            (goto-char where-to-write-duration)
            (insert duration-string))))))
  nil                                   ; say we haven't written it
  )

(define-derived-mode time-tracking-csv-mode timestamped-csv-mode "Time-tracking CSV"
  "CSV mode with ISO timedates in the first column.
When you start typing in the last row, this minute's timedate is
inserted at the start of the row if it is not already there.
When you save the file, durations are filled in in the second column."
  (make-local-variable 'write-file-functions)
  (add-hook 'write-file-functions 'time-tracking-csv-mode-fill-in-durations))

(provide 'dated-csv)
;;; dated-csv.el ends here
