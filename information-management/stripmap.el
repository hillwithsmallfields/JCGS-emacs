;;; stripmap.el --- Help editing stripmaps           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: convenience, multimedia

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

(defun stripmap-max-centre-position (from to)
  "Return the maximum necessary left-hand width between FROM and TO."
  (let ((max-pos 0))
    (goto-char from)
    (beginning-of-line 1)
    (while (< (point) to)
      (when (looking-at "^\\s-*\\(\\S-[^|+%]+\\)[|+%]")
	(let ((needed (1+ (- (match-end 1) (match-beginning 1)))))
	  (when (> needed max-pos)
	    (message "%s needs %d" (match-string 1) needed)
	    (setq max-pos needed))))
      (beginning-of-line 2))
    max-pos))

(defun stripmap-realign-block ()
  "Realign the block around point."
  (interactive)
  (save-excursion
    (backward-paragraph 1)
    (let ((start (point)))
      (forward-paragraph 1)
      (let* ((end (point-marker))
	     (spinal-column (stripmap-max-centre-position start end)))
	(message "Aligning to %d" spinal-column)
	(goto-char end)
	(beginning-of-line 1)
	(while (>= (point) start)
	  (when (looking-at "^[^|+%]+[|+%]")
	    (let ((existing (save-excursion
			      (goto-char (match-end 0))
			      (current-column))))
	      (message "on %S, existing is %d" (buffer-substring-no-properties (line-beginning-position) (line-end-position)) existing)
	      (cond
	       ((> existing spinal-column)
		(message "On %S, deleting %d char widths" (buffer-substring-no-properties (line-beginning-position) (line-end-position)) (- existing spinal-column))
		;; (delete-char (- existing spinal-column))
		(move-to-column (- existing spinal-column))
		(delete-region (line-beginning-position) (point))
		)
	       ((< existing spinal-column)
		(message "On %S, adding %d chars" (buffer-substring-no-properties (line-beginning-position) (line-end-position)) (- spinal-column existing))
		(back-to-indentation)
		(insert (make-string (- spinal-column existing) 32))
		;; (indent-to (- spinal-column existing))
		))))
	  (beginning-of-line 0))))))

(defun stripmap-prepare-html ()
  "Convert this stripmap file to HTML."
  (interactive)
  (let* ((this-name (buffer-file-name))
	 (html-name (concat (file-name-sans-extension this-name) ".html")))
    (copy-file this-name html-name t)
    (find-file html-name)
    (goto-char (point-min))
    (while (re-search-forward "\\* \\(.+\\) \\*" (point-max) t)
      (let ((name (match-string 1)))
      (replace-match (concat "</pre><h2><a name=\"" name "\">" name "</a></h2><pre>"))))
    (goto-char (point-min))
    (while (re-search-forward "^# \\(.+\\)$" (point-max) t)
      (replace-match (concat "</pre>\n<p>" (match-string 1) "</p>\n<pre>")))
    (goto-char (point-min))
    (while (re-search-forward "--- \\(.+?\\) ---" (point-max) t)
      (let ((target (match-string 1)))
      (replace-match (concat "--- <a href=\"#" target "\">" target "</a> ---"))))
    (goto-char (point-min))
    (insert "<html><head><meta charset=\"UTF-8\"><title>Stripmap</title></head>\n<body>\n<pre>\n")
    (goto-char (point-max))
    (insert "</pre>\n</body>\n</html>\n")
    (basic-save-buffer)))

(provide 'stripmap)
;;; stripmap.el ends here
