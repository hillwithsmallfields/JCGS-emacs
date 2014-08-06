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
	(while a
	  (push (format "%d. %s" d (car a)) r)
	  (setq d (1+ d)
		a (cdr a)))
	r)
    nil))

(defun org-smartwatch-today-entries ()
  "Return a list of today's entries.
Make the list of entries, representing each entry as a list of
strings, of which the first is the entry itself, and the rest the
path of the entry, starting with the filename base and continuing
with the headings from the top level down."
  (apply 'append
	 (mapcar
	  (lambda (file)
	    (org-check-agenda-file file)
	    (when (file-exists-p file)
	      (let ((entries (org-agenda-get-day-entries
			      file
			      (calendar-current-date))))
		(mapcar (lambda (e)
			  (let* ((location (get-text-property
					    0 'org-hd-marker
					    e))
				 (path (save-excursion
					 (set-buffer
					  (marker-buffer location))
					 (goto-char
					  (marker-position location))
					 (org-get-outline-path))))
			    ;; todo: strip down to just the state and the action text --- but probably do that somewhere else
			    (set-text-properties 0 (length e) nil e)
			    (cons e
				  (cons (file-name-sans-extension
					 (file-name-nondirectory file))
					path))))
			entries))))
	  org-agenda-files)))

(defun org-smartwatch-tags-view-entries (matcher)
  "Return a list of entries that match MATCHER."
  (apply 'append
	 (mapcar
	  (lambda (file)
	    (org-check-agenda-file file)
	    (when (file-exists-p file)
	      (save-excursion
		(find-file file)
		(org-scan-tags 'agenda matcher t))))
	  org-agenda-files)))

(defun org-smartwatch-fill-buffer (entries &optional buffer)
  "Run through the list of ENTRIES, putting in headings.
A heading is added whenever the current heading changes.  The
result is a BUFFER of lines where each line begins with a depth
number, followed by a colon \(if an entry) or a dot (if a
heading), then a space, then the entry or heading itself."
  (let ((lines nil)
	(prev-path nil))
    (dolist (entry entries)
      (unless (equal (cdr entry) prev-path)
	(let ((extra  (org-smartwatch-extra-headings (cdr entry) prev-path)))
	  (setq lines (append extra
			      lines))))
      (push (format "%d: %s"
		    (1+ (length (cdr entry)))
		    (car entry))
	    lines)
      (setq prev-path (cdr entry)))
    (setq lines (nreverse lines))
    (set-buffer (get-buffer-create (or buffer "*Today's agenda outline*")))
    (erase-buffer)
    (dolist (line lines)
      (insert line "\n"))
    (current-buffer)))

(defun org-smartwatch-today (&optional buffer)
  "Create a smartwatch-friendly BUFFER for today.
If BUFFER is given, use that, otherwise create one."
  (interactive)
  (display-buffer
   (org-smartwatch-fill-buffer (org-smartwatch-today-entries)
			       buffer)))

(defun org-smartwatch-tags-view (&optional buffer)
  "Create a smartwatch-friendly BUFFER for for matching entries.
Like `org-tags-view', but using the smartwatch for display.
If BUFFER is given, use that, otherwise create one."
  (interactive)
  (let* ((todo-only t)
	 (matcher (org-make-tags-matcher nil)))
    (message "Matcher is %S" matcher)
    (display-buffer
     (org-smartwatch-fill-buffer (org-smartwatch-tags-view-entries matcher)
				 buffer))))

(provide 'org-smartwatch)

;;; org-smartwatch.el ends here
