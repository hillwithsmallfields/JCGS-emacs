;;; indent-html.el --- Indent HTML according to its tags  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: hypermedia, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar indent-html-non-enclosing-tags
  '("br" "hr" "input"))

(defun indent-html-region (begin end)
  "Indent the html between BEGIN and END."
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char begin)
  (let ((indentation 0))
    (while (<= (point) end)
      (save-excursion
        (while (re-search-forward "<\\(/?\\)\\([a-z]+\\)" (line-end-position) t)
          (let ((closing (/= (match-beginning 1) (match-end 1)))
                (tag (match-string-no-properties 2)))
            ;; (message "%S %S %d" closing tag indentation)
            (unless (member tag indent-html-non-enclosing-tags)
              (setq indentation (+ indentation (if closing -1 1)))))))
      (beginning-of-line 2)
      (delete-horizontal-space)
      (indent-to (* indentation 2)))))

(provide 'indent-html)
;;; indent-html.el ends here
