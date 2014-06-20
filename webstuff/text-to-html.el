;;;; text-to-html.el
;;; Time-stamp: <2007-03-12 11:37:51 john>

;;;; convert plain to text to html, heuristically

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'text-to-html)
(require 'replace-regexp-list)

;;;###autoload
(defun text-to-html-string (text)
  "Convert TEXT from plain text to HTML, by guesswork."
  (save-window-excursion
    (set-buffer (get-buffer-create " *text-to-html work buffer*"))
    (erase-buffer)
    (insert text)
    (text-to-html-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun text-to-html-region (start end)
  "Convert TEXT from plain text to HTML, by guesswork."
  (interactive "r")
  (setq end (copy-marker end))
  (if (> (* 30 (count-lines start end))
	 (buffer-size))
      (progn
	;; short lines
	(apply-replace-regexp-alist
	 '(("\n" . "<br>\n"))
	 start end)
	)
    (progn
      ;; long lines
      (apply-replace-regexp-alist
       '(("\n\n" . "</p>\n\n<p>"))
       start end)
      (goto-char start) (insert "<p>")
      (goto-char end) (insert "</p>")
      ))

  )

;;;###autoload
(defun line-to-tag (tagtype)
  "Convert the current line of text to an HTML tagged construct."
  (interactive "sTag: ")
  (beginning-of-line 1)
  (insert "<" tagtype ">")
  (end-of-line 1)
  (insert "</" tagtype ">"))

;;;###autoload
(defun lines-to-tags ()
  "Convert a series of lines to HTML tagged constructs, interactively."
  (interactive)
  (while (re-search-forward "^.+$" (point-max) t)
    (call-interactively 'line-to-tag)))

;;; end of text-to-html.el
