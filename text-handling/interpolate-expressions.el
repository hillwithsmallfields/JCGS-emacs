;;; interpolate-expressions.el --- interpolate expression results into text  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: docs

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

(defun interpolate-evaluation-results (begin end)
  "Interpolate evaluation results between BEGIN and END."
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char begin)
  (while (re-search-forward ",(" end  t)
    (backward-char 1)
    (let ((result (eval (read (current-buffer)))))
      (delete-region (match-beginning 0) (point))
      (insert result)))))
       
(provide 'interpolate-expressions)
;;; interpolate-expressions.el ends here
