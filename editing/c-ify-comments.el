;;; c-ify-comments.el --- convert C++ comments to C

;; Copyright (C) 2009  John C G Sturdy

;; Author: John C G Sturdy <john.sturdy@ul.ie>
;; Keywords: convenience, c

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

;; Converts C++ comments to old-style C comments.

;;; Code:

(defun c-ify-comments-region (beg end)
  "Convert C++-style comments to C comments between BEG and END."
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char beg)
  (while (search-forward "//" end t)
    (replace-match "/*")
    (let ((comment-start (match-beginning 0)))
      (beginning-of-line 2)
      (back-to-indentation)
      (while (looking-at "//")
	(replace-match "  ")
	(beginning-of-line 2)
	(back-to-indentation))
      (end-of-line 0)
      (insert " */"))))

(defun c-ify-comments ()
  "Convert C++ comments to C comments throughout the buffer."
  (interactive)
  (c-ify-comments-region (point-min) (point-max)))

(provide 'c-ify-comments)
;;; c-ify-comments.el ends here
