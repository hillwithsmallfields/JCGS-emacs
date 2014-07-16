;;; org-smartwatch.el --- make agendas in outline form

;; Copyright (C) 2014  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, outlines

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

;; Like org-agenda, but producing an outline / org format buffer.
;; Done initially with a view to sending the output to a smartwatch outline display app.

;;; Code:

(defun org-smartwatch-extra-headings (a b)
  "Return a list of headings that have to be added to get to A from B."
  (if (>= (length a) (length b))
      (let ((d 1)
	    (r nil))
	(while (and a b (equal (car a) (car b)))
	  (setq a (cdr a)
		b (cdr b)
		d (1+ d)))
	(message "extra headings: %S" a)
	(while a
	  (push (format "%d %s" d (car a)) r)
	  (message "extra step: %S" (car r))
	  (setq d (1+ d)
		a (cdr a)))
	r)
    nil))

(defun org-smartwatch-today ()
  "Create a smartwatch-friendly buffer for today."
  (interactive)
  (let ((entries (apply 'append
			(mapcar
			 (lambda (file)
			   (org-check-agenda-file file)
			   (when (file-exists-p file)
			     (let ((entries (org-agenda-get-day-entries file (calendar-current-date))))
			       (if entries
				   (mapcar (lambda (e)
					     (let* ((location  (get-text-property 0 'org-hd-marker e))
						    (path (save-excursion
							    (set-buffer (marker-buffer location))
							    (goto-char (marker-position location))
							    (org-get-outline-path))))
					       (message "%s is at %s and has path %S" e location path)
					       ;; todo: strip down to just the state and the action text
					       (set-text-properties 0 (length e) nil e)
					       (cons e
						     (cons (file-name-sans-extension (file-name-nondirectory file))
							   path))))
					   entries)
				 nil))))
			 org-agenda-files))))
    (let ((lines nil)
	  (prev-path nil))
      (dolist (entry entries)
	(message "placing %S" entry)
	(push (format "%d %s"
		      (length (cdr entry))
		      (car entry))
	      lines)
	(message "old path %S, new path %S" prev-path (cdr entry))
	(unless (equal (cdr entry) prev-path)
	  (setq lines (append (org-smartwatch-extra-headings (cdr entry) prev-path)
			      lines)))
	(setq prev-path (cdr entry)))
      (setq lines (nreverse lines))
      (set-buffer (get-buffer-create "*Today's agenda outline*"))
      (erase-buffer)
      (dolist (line lines)
	(insert line "\n"))
      (display-buffer (current-buffer)))))

(provide 'org-smartwatch)

;;; org-smartwatch.el ends here
