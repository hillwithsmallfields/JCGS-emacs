;;;; shells-to-start -- what shells to start during initialization
;;; Time-stamp: <2006-02-14 09:05:44 john>

(provide 'shells-to-start)

(defun start-shells ()
  "Start my usual collection of shell buffers."
  (interactive)
  (make-named-shell (format "=%s=" (system-short-name)) "~")
  )

;;; end of shells-to-start.el
