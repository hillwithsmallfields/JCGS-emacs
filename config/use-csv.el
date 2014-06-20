;;;; use-csv.el --- setup for csv-related things

(use-package csv-mode
	     "$GATHERED/emacs/csv/"
	     "http://centaur.maths.qmul.ac.uk/Emacs/csv-mode.el"
	     ((csv-mode "csv-mode" "Mode for editing CSV files." t)
	      ("\\.csv~?$" . csv-mode)))

;; end of use-csv.el
