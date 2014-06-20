;;;; use-journal.el -- configure my journal system
;;; Time-stamp: <2013-10-15 12:22:11 johnstu>

(use-package journal
	     (expand-file-name "webstuff" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/webstuff/journal.el"
	     ((journal-new-day "journal"
			       "Start a new day's entry. The arguments are JOURNAL YEAR MONTH MONTHNAME DAY.
An optional extra argument gives where in the buffer the previous day was found."
			       t))
	     (setq journal-dates-directories
		   '(("research" substitute-in-file-name "$COMMON/research/log/")
		     ("asr33 restoration" substitute-in-file-name "$COMMON/www/computing/asr33/")
		     ("talks" substitute-in-file-name "$COMMON/www/talks"))))

;;; end of use-journal.el
