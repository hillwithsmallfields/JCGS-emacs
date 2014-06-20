;;;; type-break-extras.el -- extra things for typing breaks
;;; Time-stamp: <2005-02-10 10:59:00 jcgs>

(provide 'type-break-extras)

(defun type-break-light-fire ()
  "Start giving me reminders to check that the coal fire is still alight."
  (interactive)
  (require 'cl)
  (if (or (interactive-p) (yes-or-no-p "Is the coal fire lit? "))
      (push '(nil 0 1200 "Check coal fire")
	    type-break-repetitive-activities))
  (remove-hook 'type-break-start-break-hook 'type-break-light-fire))

;;; end of type-break-extras.el
