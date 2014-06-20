;;;; setup-timeclock.el -- load and configure timeclock
;;; Time-stamp: <2006-12-03 17:21:07 jcgs>

(setq timeclock-file (substitute-in-file-name "$COMMON/var/timelog")
      timeclock-workday (* 6 60 60))

(provide 'setup-timeclock)

;;; end of setup-timeclock.el
