;;; fetch-page-as-text.el --- fetch a page and convert it to text

;; Copyright (C) 2012  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: hypermedia

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

;; Written for use with gnugol

;;; Code:

(defun fetch-page-as-text (url buffer)
  "Fetch URL and convert it to text in BUFFER."
  (call-process "w3m" nil (get-buffer-create buffer) nil "-dump" url))

(defun gnugol-fetch-page-as-text (url)
  "Fetch URL as text, and put it in the gnugol buffer."
  (interactive (list (if (org-in-regexp org-bracket-link-regexp)
			 (progn
			   (let (link)
			     (setq link (org-extract-attributes
					 (org-link-unescape (org-match-string-no-properties 1))))
			     (while (string-match " *\n *" link)
			       (setq link (replace-match " " t t link)))
			     (setq link (org-link-expand-abbrev link))))
		       nil)))
  (save-excursion
    (beginning-of-line 2)
    (message "Starting at %d in buffer %S" (point) (current-buffer))
    (let ((start (point)))
      (fetch-page-as-text url (current-buffer))
      (message "Ending at %d in buffer %S" (point) (current-buffer))
      (indent-rigidly start (point) 3))))

(provide 'fetch-page-as-text)
;;; fetch-page-as-text.el ends here
