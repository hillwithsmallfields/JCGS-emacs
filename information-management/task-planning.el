;;;; Stuff for task planning
;;; Time-stamp: <2014-10-15 14:13:47 johstu01>

(defun count-days (key)
  "Count the days matching KEY in a work plan."
  (interactive "sDays matching key: ")
  (let ((pattern (format "%s (\\([0-9]+\\)\\([dw]\\)" key))
	(days 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern (point-max) t)
	(let ((number (string-to-number (match-string 1)))
	      (multiplier (if (string= (match-string 2) "w")
			      5
			    1)))
	  (setq days (+ days (* number multiplier))))))
    (message "%d days (%d weeks %d days)" days (/ days 5) (% days 5))
    days))

;;; task-planning.el ends here
