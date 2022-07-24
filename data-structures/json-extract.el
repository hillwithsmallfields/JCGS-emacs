;;; json-extract.el --- extract JSON data            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Sturdy (CCS)

;; Author: John Sturdy (CCS) <jsturdy@ccsl.com>
;; Keywords: data, convenience

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

;; Extract chunks of JSON data into separate files.

;;; Code:

(defun json-extract-expression (filename)
  "Extract a JSON expression into a file.
Argument FILENAME is the file to write to.
The expression is re-indented."
  (interactive "FWrite JSON to file: ")
  (let* ((start (point))
         (end (save-excursion
                (forward-sexp 1)
                (point)))
         (buf (save-excursion
                (find-file filename)
                (erase-buffer)
                (current-buffer))))
    (call-process-region start end "jq"
                         nil buf nil ".")
    (pop-to-buffer buf)
    (basic-save-buffer)))

(provide 'json-extract)
;;; json-extract.el ends here
