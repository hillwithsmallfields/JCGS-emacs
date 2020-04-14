;;; dated-csv.el --- functions for CSV files with dates in the first column  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

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
;; that today's date is at the start of that line.

;;; Code:

(require 'csv-mode)

(defun force-row-timestamp (from to)
  "Make the line being changed start with a date string.
This is in ISO format, followed by a comma, suitable for
use in a CSV file.
Arguments FROM and TO mark the area being changed, for use
on `before-change-functions'."
  (when (or (eobp)
            (save-excursion
              (beginning-of-line 2)
              (eobp)))
    (let ((date-string (format-time-string "%F,")))
      (unless (save-excursion
                (goto-char from)
                (beginning-of-line)
                (looking-at date-string))
        (beginning-of-line)
        (insert date-string)
        (end-of-line)))))

(define-derived-mode dated-csv-mode csv-mode "Dated CSV"
  "CSV mode with ISO dates in the first column.
When you start typing in the last row, today's date is inserted
at the start of the row if it is not already there."
  (add-hook 'before-change-functions 'force-row-timestamp nil t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))

(provide 'dated-csv)
;;; dated-csv.el ends here
