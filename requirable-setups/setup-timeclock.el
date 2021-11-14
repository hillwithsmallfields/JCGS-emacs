;;;; setup-timeclock.el -- load and configure timeclock
;;; Time-stamp: <2021-11-14 18:31:28 jcgs>

(setq timeclock-file (substitute-in-file-name "$SYNCED/var/timelog")
      timeclock-workday (* 6 60 60))

(provide 'setup-timeclock)

;;; end of setup-timeclock.el
