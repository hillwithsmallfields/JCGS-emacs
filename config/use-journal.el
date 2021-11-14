;;;; use-journal.el -- configure my journal system
;;; Time-stamp: <2021-11-14 18:31:09 jcgs>

(require 'jcgs-use-package)

(jcgs/use-package journal
	     (expand-file-name "webstuff" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/lisp/webstuff/journal.el"
	     ((journal-new-day "journal"
			       "Start a new day's entry. The arguments are JOURNAL YEAR MONTH MONTHNAME DAY.
An optional extra argument gives where in the buffer the previous day was found."
			       t))
	     (setq journal-dates-directories
		   '(("research" substitute-in-file-name "$SYNCED/research/log/")
		     ("asr33 restoration" substitute-in-file-name "$SYNCED/www/computing/asr33/")
		     ("talks" substitute-in-file-name "$SYNCED/www/talks"))))

;;; end of use-journal.el
