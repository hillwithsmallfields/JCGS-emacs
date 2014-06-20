;;; buffer-file-names.el --- do things based on buffer file names

;; Copyright (C) 2012  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun kill-buffers-matching-file-regexp (regexp)
  "Kill buffers whose filenames match REGEXP."
  (interactive "sKill buffers of files matching: ")
  (let ((killed 0))
  (mapcar (lambda (buffer)
	    (when (and (stringp (buffer-file-name buffer))
		       (string-match regexp (buffer-file-name buffer)))
	      (kill-buffer buffer)
	      (setq killed (1+ killed))))
	  (buffer-list))
  (message "Killed %d buffers" killed)))

(provide 'buffer-file-names)
;;; buffer-file-names.el ends here
