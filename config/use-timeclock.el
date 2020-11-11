;;;; find, load and configure timeclock
;;; Time-stamp: <2020-11-11 20:51:08 jcgs>

(unless (fboundp 'first) (require 'cl))

(jcgs/use-package timeclock
	     t
	     "http://www.planner.org/timeclock.el" ; todo: get the real URL
	     ((timeclock-in "timeclock"
			    "Clock in, recording the time in the timelog."
			    t))
	     (setq timeclock-file (substitute-in-file-name
				   "$COMMON/var/timelog")
		   ;; 6 hours at the computer is probably enough
		   timeclock-workday (* 6 60 60)))

;;; end of use-timeclock.el
