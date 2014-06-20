;;; config-diary-calendar.el --- configure the diary and calendar systems

;; Copyright (C) 2013  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience, data

;;;; Diary and calendar

(setq diary-file (substitute-in-file-name "$DROPBOX/org/diary")
      calendar-date-style 'iso
      calendar-christian-all-holidays-flag t
      diary-display-function 'diary-fancy-display
      )

(add-to-list 'auto-mode-alist (cons diary-file 'diary-mode))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(require 'parse-time)			; for parse-time-weekdays

(dolist (day '(("domhnach" . 0)
	       ("luan" . 1)
	       ("mháirt" . 2)
	       ("chéadaoin" . 3)
	       ("déardaoin" . 4)
	       ("aoine" . 5)
	       ("satharn" . 6)))
  (add-to-list 'parse-time-weekdays day))

(defvar jcgs/diary-save-remove-duplicates t
  "Whether to purge duplicates from the diary file every time it is saved.")

(defun line-string ()
  "Return the contents of the current line."
  (buffer-substring-no-properties (line-beginning-position)
				  (line-end-position)))

(defun jcgs/diary-mode-save-hook-function ()
  "My diary mode pre-save function."
  ;; todo: stop putting multiple blanks lines between repeating block and dates block
  ;; todo: remove all copies of a duplicate in one go, instead of one each time
  (interactive)				; for debugging
  (delete-matching-lines "^$")
  (let ((column (current-column))	; remember where we were
	(line-text (line-string))	; so we can find it even after sorting
	;; find the first line with a date on it (I'm assuming the
	;; repeating entries come at the start of the file, which is
	;; actually just my convention AFAIK)
	(dated-start (save-excursion
		       (goto-char (point-min))
		       (if (re-search-forward "^[0-9/]+" (point-max) t)
			   (point-at-bol)
			 (point-min)))))
    (sort-lines nil
		dated-start
		(point-max))
    ;; (goto-char (point-min))
    (goto-char dated-start)		; was (point-min)
    (skip-syntax-forward "-")
    (delete-region dated-start (point))
    (when jcgs/diary-save-remove-duplicates
      (goto-char dated-start)
      (let ((old-line (line-string))
	    (old-year-month-string (buffer-substring-no-properties (point) (+ (point) 7))))
	(forward-line 1)
	(while (not (eobp))
	  (let ((new-line (line-string)))
	    (when (>= (length new-line) 7)
	      (let ((new-year-month-string (substring new-line 0 7)))
		(if (string= old-line new-line)
		    (delete-region (line-beginning-position)
				   (line-beginning-position 2))
		  (setq old-line new-line)
		  (unless (string= new-year-month-string old-year-month-string)
		    (setq old-year-month-string new-year-month-string)
		    (insert "\n"))
		  )))
	    (forward-line 1)))))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (search-backward line-text (point-min) t)
    (move-to-column column))
  (delete-trailing-whitespace)
  nil)

(defun jcgs/diary-mode-hook-function ()
  "My diary mode setup."
  (add-hook 'before-save-hook
	    'jcgs/diary-mode-save-hook-function
	    t t))

(add-hook 'diary-mode-hook 'jcgs/diary-mode-hook-function)

(when (boundp 'diary-date-forms)
  (setq diary-date-forms diary-iso-date-forms))

(defun diary-move-to-today (&optional last)
  "Move to the first (optionally, LAST) of today's entries."
  (interactive)
  (let* ((date (decode-time))
	 (date-string (diary-date-string (nth 5 date) (nth 4 date)  (nth 3 date))))
    (if last
	(progn
	  (goto-char (point-max))
	  (if (re-search-backward (format "^%s " date-string) (point-min) t)
	      (beginning-of-line 2)
	    (error "%s not found" date-string)))
      (goto-char (point-min))
      (if (re-search-forward (format "^%s " date-string) (point-max) t)
	  (beginning-of-line)
	(error "%s not found" date-string)))
    (point)))

(defun diary-mark-today ()
  "Set the region to cover today's entries in the diary.
Assumes they are contiguous."
  (interactive)
  (push-mark (diary-move-to-today t))
  (diary-move-to-today))

;;; config-diary-calendar.el ends here
