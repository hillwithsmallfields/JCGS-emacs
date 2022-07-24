;;; csv-enter-fields.el --- assisted entry of named fields in CSV files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

;; Author: John Sturdy <jsturdy@ccsl.com>
;; Keywords: convenience

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

;; Put data in named columns of a CSV file

;;; Code:

(defvar csv-field-order nil
  "The order of column names in the current buffer.")

(make-variable-buffer-local 'csv-field-order)

(defun csv-field-order ()
  "Get the order of column names in the current buffer.
This is cached, so if you change header line, you'll need
to reload the buffer to update it."
  (if csv-field-order
      csv-field-order
    (save-excursion
      (goto-char (point-min))
      (setq csv-field-order
            (split-string (buffer-substring (point)
                                            (line-end-position))
                          ",")))))

(defun csv-field-index (field-name)
  "Return the index of FIELD-NAME."
  (position field-name (csv-field-order)))

(defun csv-field-add (fields name value)
  "To a vector of FIELDS add NAME VALUE at the appropriate position.
This may return a new vector, so you should normally assign the
result back into the variable that FIELDS came from."
  (let ((index (csv-field-index name))
        (original-length (length fields)))
    (when (> index original-length)
      (let ((new-fields (make-vector (1+ index))))
        (dotimes (i original-length)
          (aset new-fields i (aref fields i)))
        (setq fields new-fields)))
    (aset fields index value)
    fields))

(defun csv-field-read-name (prompt &optional allow-new)
  "Prompt for a field name using PROMPT, with completion.
With optional ALLOW-NEW, allow the user to enter fields not in
the file header."
  (completing-read prompt
                   (csv-field-order)
                   nil (not allow-new)))

(defun csv-field-as-string (fields)
  "Make a string from a fields vector."
  (mapconcat 'prin1-to-string       ; TODO: convert nil to empty string
             fields ","))

(provide 'csv-enter-fields)
;;; csv-enter-fields.el ends here
