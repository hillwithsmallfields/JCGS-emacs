;;;; shells-to-start -- what shells to start during initialization
;;; Time-stamp: <2005-11-03 10:11:55 john>

(provide 'shells-to-start)

(defun start-shells ()
  "Start my usual collection of shell buffers."
  (interactive)
  (make-named-shell (format "=%s=" (system-short-name)) "~")
  (make-shell-for-directory-if-present "~/ULDSPACE")
  )

;;; end of shells-to-start.el
