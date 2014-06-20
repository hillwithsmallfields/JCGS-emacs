;;;; diary-mode.el
;;; Time-stamp: <2006-01-23 16:45:58 john>
;;; smarten up a buffer for use with the emacs diary
;;; originally done mainly to colour things according to day of week

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

(require 'calendar)
(provide 'diary-mode)

(defconst diary-mode-Sunday-face (copy-face 'default 'diary-mode-Sunday-face))
(defconst diary-mode-Monday-face (copy-face 'default 'diary-mode-Monday-face))
(defconst diary-mode-Tuesday-face (copy-face 'default 'diary-mode-Tuesday-face))
(defconst diary-mode-Wednesday-face (copy-face 'default 'diary-mode-Wednesday-face))
(defconst diary-mode-Thursday-face (copy-face 'default 'diary-mode-Thursday-face))
(defconst diary-mode-Friday-face (copy-face 'default 'diary-mode-Friday-face))
(defconst diary-mode-Saturday-face (copy-face 'default 'diary-mode-Saturday-face))

(mapcar 'makunbound '(diary-colour-backgrounds diary-background-colour))

(defvar diary-colour-backgrounds nil
  "Whether to colour the backgrounds in diary mode.")

(defvar diary-background-colour nil ; "black" ; "grey"
  "The background colour in diary mode, or nil.")

(set-face-foreground diary-mode-Sunday-face "red")
(set-face-foreground diary-mode-Monday-face "orange")
(set-face-foreground diary-mode-Tuesday-face "brown")
(set-face-foreground diary-mode-Wednesday-face (if diary-background-colour "green" "darkgreen"))
(set-face-foreground diary-mode-Thursday-face "blue")
(set-face-foreground diary-mode-Friday-face (if diary-background-colour "white" "black"))
(set-face-foreground diary-mode-Saturday-face "purple")

(when diary-colour-backgrounds
  (set-face-background diary-mode-Sunday-face "cyan")
  (set-face-background diary-mode-Monday-face "green")
  (set-face-background diary-mode-Tuesday-face "blue")
  (set-face-background diary-mode-Wednesday-face "magenta")
  (set-face-background diary-mode-Thursday-face "yellow")
  (set-face-background diary-mode-Friday-face "white")
  (set-face-background diary-mode-Saturday-face "green"))

(when diary-background-colour
  (set-face-background diary-mode-Sunday-face diary-background-colour)
  (set-face-background diary-mode-Monday-face diary-background-colour)
  (set-face-background diary-mode-Tuesday-face diary-background-colour)
  (set-face-background diary-mode-Wednesday-face diary-background-colour)
  (set-face-background diary-mode-Thursday-face diary-background-colour)
  (set-face-background diary-mode-Friday-face diary-background-colour)
  (set-face-background diary-mode-Saturday-face diary-background-colour))

(defvar diary-mode-day-faces
  [ diary-mode-Sunday-face
    diary-mode-Monday-face
    diary-mode-Tuesday-face
    diary-mode-Wednesday-face
    diary-mode-Thursday-face
    diary-mode-Friday-face
    diary-mode-Saturday-face ]
  "Faces for each day of the week.")

(defun diary-mode-write-file-function ()
  "Function to call as part of writing a diary file.
This re-does the colouring."
  (diary-mode:colour-buffer)
  nil)

(defvar diary-mode-hook nil
  "Hooks to run on entering diary mode.")

(defvar diary-mode-map (make-sparse-keymap "Diary mode")
  "Keymap to use in diary mode.")

(define-key diary-mode-map "\C-c\C-d" 'diary-find-date)
(define-key diary-mode-map "\C-c\C-t" 'diary-find-today)

;;;###autoload
(defun diary-mode ()
  "Major mode for editing diary files."
  (interactive)
  (kill-all-local-variables)
  ;; (make-variable-buffer-local 'after-change-function)
  (use-local-map diary-mode-map)
  (run-hooks 'diary-mode-hook)
  (add-hook 'after-change-functions 'diary-mode:set-line-colour t t)
  (setq major-mode 'diary-mode
	mode-name "diary file"
 	;; after-change-function 'diary-mode:set-line-colour
	)
  (diary-mode:colour-buffer)
  (add-hook 'local-write-file-hooks 'diary-mode-write-file-function nil t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst weekday-alist
 '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
   ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
   ("Tues" . 2) ("Thurs" . 4)
   ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
   ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defconst full-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))

(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  )

(defconst monthname-alist
  (append monthabbrev-alist
	  full-monthname-alist
	  '(("Sept" . 9))))

(defconst monthname-regexp
  (concat "\\("
	  (mapconcat (function car)
		     monthname-alist
		     "\\|")
	  "\\)\\.?"))

(defconst weekday-regexp
  (concat "\\("
	  (mapconcat (function car)
		     weekday-alist
		     "\\|")
	  "\\)\\.?"))

(defconst monthnumber-regexp "\\(0?[1-9]\\|1[0-2]\\)")
(defconst monthnumber-regexp-two-char "\\(0[1-9]\\|1[0-2]\\)")

(defconst monthday-regexp "\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)")
(defconst monthday-regexp-two-char "\\([0-2][0-9]\\|3[01]\\)")

(defconst full-year-regexp "[0-2][0-9][0-9][0-9]")
(defconst short-year-regexp "[0-9][0-9]")

(defconst year-regexp (concat "\\(" full-year-regexp
			      "\\|" short-year-regexp "\\)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst diary-short-month-name-list
  '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
    "Jun" "May" "Apr" "Mar" "Feb" "Jan")
  "Short names of all the months")

(defvar diary-last-found-day 1
  "The most recent day found by diary-find-date's interactive reader.")

(defun diary-find-date (month day)
  "Move to MONTH DAY."
  (interactive
   (let* ((completion-ignore-case t)
	  (month (completing-read "Month: " full-monthname-alist nil t))
	  (day (if (and (fboundp 'handsfree-read-number)
			(boundp 'journal-month-lengths))
		   (setq diary-last-found-day
			 (handsfree-read-number "Day: " 1 (aref journal-month-lengths month) diary-last-found-day))
		 (read-from-minibuffer "Day: "))))
     (list month day)))
  (if (not (eq major-mode 'diary-mode))
      (find-file diary-file))
  (if (stringp day) (setq day (string-to-int day)))
  (setq month (if (integerp month)
		  (car (rassoc month monthabbrev-alist))
		(substring month 0 3)))
  (goto-char (point-min))
  (if (re-search-forward (format "^%s %02d" month day)
			 (point-max) t)
      (goto-char (match-beginning 0))
    (let* ((day-in-ten (% day 10))
	   (tens-of-days (/ day 10))
	   (remaining-in-ten-days (if (< day-in-ten 9)
				      (format "\\(%d[%d-9]\\)" tens-of-days
					      (1+ day-in-ten))
				    nil))
	   (remaining-tens-of-days (if (< tens-of-days 3)
				       (format "\\([%s][0-9]\\)" (substring "123" tens-of-days))
				     nil))
	   (pattern (if (and remaining-in-ten-days
			     remaining-tens-of-days)
			(format "^%s \\(%s\\|%s\\)"
				month
				remaining-in-ten-days
				remaining-tens-of-days)
		      (format "^%s %s"
			      month
			      (or remaining-in-ten-days
				  remaining-tens-of-days)))))
      (if (re-search-forward pattern (point-max) t)
	  (goto-char (match-beginning 0))
	(let* ((month-number (cdr (assoc month monthabbrev-alist)))
	       (increment (if (= month-number 12) -1 1))
	       (month-name nil))
	  (setq month-number (+ month-number increment))
	  (catch 'found
	    (while (setq month-name (car (rassoc month-number monthabbrev-alist)))
	      (let ((month-pattern (format "^%s [0-3][0-9]" month-name)))
		(if (re-search-forward month-pattern
				       (point-max) t)
		    (progn
		      (goto-char (match-beginning 0))
		      (throw 'found t))))
	      (setq month-number (+ month-number increment)))
	    (message "Could not find date")))))))

(defun diary-find-today ()
  "Move to today."
  (interactive)
  (let ((time (decode-time)))
    (diary-find-date (nth 4 time) (nth 3 time))))

(defun diary-cursor-to-date ()
  "Return the date the diary cursor is on."
  (save-excursion
    (beginning-of-line 1)
    (let* ((month-name-regexp (if european-calendar-style
				  (concat "^[0-3]?[0-9][/ ]+" monthname-regexp)
				(concat "^" monthname-regexp)))
	   (month-number-regexp (if european-calendar-style
				    (concat "^[0-3]?[0-9][/ ]+" monthnumber-regexp)
				  (concat "^" monthnumber-regexp)))
	   (day-regexp (if european-calendar-style
			   "^\\([0-3]?[0-9]\\)[/ ]+"
			 "^[jfmasond1-9][a-z0-2][a-z]*[/ ]*\\([0-3]?[0-9]\\)[/ ]"))
	   (year-regexp "^[0-9a-z]+[ /]+[0-9a-z]+[ /]+\\([0-9][0-9][0-9][0-9]\\)")
	   (the-month (if (looking-at month-name-regexp)
		      (let* ((month-string (substring (buffer-substring-no-properties (match-beginning 1) (match-end 1)) 0 3))
			     (months-remaining-backwards (member month-string diary-short-month-name-list))
			     (month-number (length months-remaining-backwards))
			     )
			 month-number)
		    (if (looking-at month-number-regexp)
			(match-string 1)
		      nil)))
	   (day (if (looking-at day-regexp)
		    (string-to-int (match-string 1))
		  nil))
	   (year (if (looking-at year-regexp)
		     (string-to-int (match-string 1))
		   (third (calendar-current-date)))))
      ;; (message "month=%d day=%d year=%d" the-month day year)
      (if (and the-month day year)
	  (list the-month day year)
	nil))))

(defun diary-mode:day-of-week ()
  "Return the day of week at point."
  (let ((date (diary-cursor-to-date)))
    (if date
	(calendar-day-of-week date)
      (save-excursion
	(beginning-of-line 1)
	(catch 'found-day
	  (dotimes (iday 6)
	    (when (looking-at (aref calendar-day-name-array iday))
	      (throw 'found-day iday)))
	  nil)))))

;;;###autoload
(defun diary-mode:show-day-of-week (where)
  "Show which day of the week WHERE is on."
  (interactive "d")
  (save-excursion
    (goto-char where)
    (message "%S" (diary-mode:day-of-week))))

(defvar diary-mode:in-set-line-colour nil)

;;;###autoload
(defun diary-mode:set-line-colour (a &rest ignore)
  "Set the colour for the line at A."
  (interactive)
  (unless diary-mode:in-set-line-colour
    (save-excursion
      (goto-char a)
      (beginning-of-line 1)
      (let ((start (point))
	    (diary-mode:in-set-line-colour t))
	(let ((day (diary-mode:day-of-week)))
	  (when day
	    (end-of-line 1)
	    (put-text-property start (point)
			       'face (aref diary-mode-day-faces day))))))))

;;;###autoload
(defun diary-mode:colour-buffer ()
  "Colour in the diary buffer."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(diary-mode:set-line-colour (point))
	(beginning-of-line 2)))
    (set-buffer-modified-p modified)))

(defun diary-mode:test ()
  "Test the diary buffer day-of-week code."
  (interactive)
  (with-output-to-temp-buffer "*diary dates*"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (let ((start (point))) (buffer-substring start (point-at-eol))))
	      (date (diary-cursor-to-date))
	      (day (diary-mode:day-of-week)))
	  (princ (format "%14s %s %s\n" date day line)))
	(beginning-of-line 2)))))

;;; end of diary-mode.el
