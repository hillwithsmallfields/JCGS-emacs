;;; longest-working-search.el --- Find the longest part of a given regexp that actually finds something  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
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

;; 

;;; Code:


(defun failsafe-re-search-forward (regexp &optional bound noerror count)
  (interactive "sSearch for: ")
  "Like `re-search-forward` but return `nil` if there is an error."
  (condition-case
      errvar
      (re-search-forward regexp bound noerror count)
    (error nil)))

(defun longest-working-search (regexp &optional bound)
  (interactive "sSearch for: ")
  (let ((whole-result (failsafe-re-search-forward regexp (or bound (point-max)))))
    (if (integerp whole-result)
        regexp
      (let* ((full-length (length regexp))
             (longest-working-regexp
             (catch 'matched
               (dotimes (i full-length)
                 (let ((partial-regexp (substring regexp 0 (- full-length i))))
                   (when (integerp (failsafe-re-search-forward partial-regexp (or bound (point-max)) t))
                     (throw 'matched partial-regexp))))
               nil)))
        (if longest-working-regexp
            (progn
              (message "\"%s\" matched" longest-working-regexp)
              longest-working-regexp)
          (message "No substrings matched")
          nil)))))

(provide 'longest-working-search)
;;; longest-working-search.el ends here
