;;;; vr-log.el -- vr log facilities
;;; Time-stamp: <2004-12-10 16:33:35 jcgs>

(provide 'vr-log)

(defun vr-display-log-in-other-frame ()
  (interactive)
  (let ((main-frame (selected-frame)))
    (switch-to-buffer-other-frame " *vr*")
    (let ((vr-frame (selected-frame)))
      ;; (set-frame-font "")
      )
    (select-frame main-frame)))

;;; end of vr-log.el
