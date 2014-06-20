;;;; type-break-enhancements.el
;;; Time-stamp: <03/01/25 14:49:59 jcgs>

(provide 'type-break-enhancements)

(defvar type-break-statistics-hook nil
  "A chance for typing break hooks to provide statistics.
Functions on this hook should say their stuff onto stdout,
e.g. with princ")

(defun type-break-statistics ()
  "Print statistics about typing breaks in a temporary buffer.
This includes the last time a typing break was taken, when the next one is
scheduled, the keystroke thresholds and the current keystroke count, etc."
  (interactive)
  (with-output-to-temp-buffer "*Typing Break Statistics*"
    (princ (format "Typing break statistics\n-----------------------\n
Typing break mode is currently %s.
Interactive query for breaks is %s.
Warnings of imminent typing breaks in mode line is %s.

Last typing break ended     : %s
Next scheduled typing break : %s
Interval between breaks     : %s
Length of typing breaks     : %s\n
Minimum keystroke threshold : %s
Maximum keystroke threshold : %d
Current keystroke count     : %s
Peak keystroke count        : %s
Total keystroke count       : %d
Deferments                  : %d
Max consecutive deferments  : %d\n\n"
                   (if type-break-mode "enabled" "disabled")
                   (if (boundp 'type-break-query-mode)
		       (if type-break-query-mode "enabled" "disabled")
		     "Not available in this version")
                   (if (boundp 'type-break-mode-line-message-mode)
		       (if type-break-mode-line-message-mode "enabled" "disabled")
		     "Not available in this version")
                   (if type-break-time-last-break
                       (current-time-string type-break-time-last-break)
                     "never")
                   (if (and type-break-mode type-break-time-next-break)
                       (format "%s\t(%s from now)"
                               (current-time-string type-break-time-next-break)
                               (type-break-format-time
                                (type-break-time-difference
                                (current-time)
                                type-break-time-next-break)))
                     "none scheduled")
		   (type-break-format-time type-break-interval)
		   (type-break-format-time type-break-good-rest-interval)
                   (or (car type-break-keystroke-threshold) "none")
                   (or (cdr type-break-keystroke-threshold) "none")
                   type-break-keystroke-count
		   type-break-max-keystrokes
		   type-break-total-keystroke-count
		   type-break-total-deferments
		   type-break-max-deferments))
    (run-hooks 'type-break-statistics-hook)))

;;; end of type-break-enhancements.el
