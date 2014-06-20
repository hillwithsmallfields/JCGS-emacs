;;;; prefix-voice.el
;;; Time-stamp: <2005-08-02 18:00:30 jcgs>

(provide 'prefix-voice)

(defvar prefix-voice-name-number-pairs
  '((once . 1)
    (twice . 2)
    (thrice . 3)
    (four-times . 4)
    (five-times . 5)
    (six-times . 6)
    (seven-times . 7)
    (dozen . 12)
    (score . 20)
    (gross . 144)
    )
  "Repetitions as commands.")

(mapcar (function (lambda (pair)
		    (fset (car pair)
			  `(lambda ()
			     (interactive)
			     (setq prefix-arg ,(cdr pair))))))
	prefix-voice-name-number-pairs)

(defvar vr-prefix-commands
  (mapcar (lambda (pair)
	    (cons (symbol-name (car pair))
		  (car pair)))
	  prefix-voice-name-number-pairs)
  "Voice commands for prefices")

;;; end of prefix-voice.el
