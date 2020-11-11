;;; gather-vocab.el --- Gather and manage vocabulary lists from text  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

;; Author: John Sturdy <jsturdy@ccsl.com>
;; Keywords: languages

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

;; 

;;; Code:

(defun gather-vocab-buffer ()
  "Gather the vocabulary from a buffer."
  (interactive)
  (gather-vocab-region (point-min) (point-max)))

(defun gather-vocab-region (from to &optional table)
  "Gather the vocabulary from the region between FROM and TO.
The optional TABLE is a hash to gather them into.  The results
are returned as a hash table (the one supplied if one was)."
  (interactive "r")
  (unless table
    (setq table (make-hash-table :test 'string-equal)))
  (goto-char (point-min))
  (while (re-search-forward "\\<\\w+\\>" to t)
    (puthash (match-string-no-properties 0) nil table))
  table)

(provide 'gather-vocab)
;;; gather-vocab.el ends here
