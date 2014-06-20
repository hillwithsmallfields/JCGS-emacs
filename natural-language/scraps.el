;;;; scraps.el -- scraps taken from other files in this directory
;;; Time-stamp: <2006-03-11 12:40:27 jcgs>

(defun mulvo-test-set-symbol-language-type-meaning ()
  (interactive)
  (let ((mulvo-debug t))
    (mulvo-clear-dictionary)
    (mapcar (lambda (spec)
	      (apply 'mulvo-set-symbol-language-type-meaning spec)
	      (message "Just after setting, result for %S:%S:%S is %S" (second spec) (third spec) (first spec) (mulvo-get-meaning (second spec) (third spec) (first spec)))
	      (mulvo-dump-dict))
	    '(
	      ("_fear" "ENG" noun (("ENG" . "fear") ("GLI" . "eagla") ("DUT". "vrees")))
	      ("_fear" "GLI" noun (("ENG" . "man") ("GLI" . "fear")))
	      ("_bean" "GLI" noun (("ENG" . "woman") ("GLI" . "bean")))
	      ("_fear" "ENG" verb (("ENG" . "fear") ("DUT" . "vrezen")))
	      )
	    )
    (mapcar (lambda (spec)
	      (message "Result for %S is %S" spec (apply 'mulvo-get-meaning spec)))
	    '(("ENG" noun "_fear")
	      ("ENG" verb "_fear")
	      ("GLI" noun "_fear")
	      ("GLI" noun "_bean")))
    (mulvo-list-dictionary "*Small test steam dictionary*")))

(defun mulvo-test ()
  (interactive)
  (switch-to-buffer "*Messages*")
  (erase-buffer)
  (mulvo-clear-dictionary)
  (let ((mulvo-dictionaries-directories
	 (list (substitute-in-file-name "$COMMON/dictionaries")))
	(mulvo-dictionaries-pattern "test.\\.csv$")
	(mulvo-debug t)
	(stack-trace-on-error t))
    (mulvo-setup)))

;;; end of scraps.el
