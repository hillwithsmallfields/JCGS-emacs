;;; Time-stamp: <2006-07-28 10:28:37 jcgs>

;; todo: move out of basics, to anywhere before we start finding files

(defun add-auto-mode (pattern mode)
  "Declare files matching PATTERN to be in MODE."
  (let ((pair (assoc pattern auto-mode-alist)))
    (if pair
	(rplacd pair mode)
      (setq auto-mode-alist
	    (cons (cons pattern mode)
		  auto-mode-alist)))))

(add-auto-mode "/birthdays$" 'diary-move-to-today)
;; (add-auto-mode "/sorted-mail/" 'rmail-input)
;; (add-auto-mode "/calendar$" 'show-today)
;; (add-auto-mode "\\.ros$" 'step-round-buffer)
;; (add-auto-mode "\\.verse$" (function (lambda () (require 'ps-verse))))
(add-auto-mode "\\.[1-8]$" 'nroff-mode) ;manualformats
(add-auto-mode "/[Mm]akefile$" 'makefile-mode)
(add-auto-mode "\\.lap" 'lisp-mode)
(add-auto-mode "\\.cpp" 'c-mode)
(add-auto-mode "\\.i$" 'c-mode)
(add-auto-mode "\\.sml$" 'sml-mode)
(add-auto-mode "research/log/*\.html$" 'html-journal-helper-mode)
(add-auto-mode "personal/journal/dates/*\.html$" 'html-journal-helper-mode)
(add-auto-mode "\\.html$" 'html-helper-mode)
(add-auto-mode "\\.shtml$" 'html-helper-mode)
(add-auto-mode "\\.htm$" 'html-helper-mode)
(add-auto-mode "\\.head" 'html-helper-mode)
(add-auto-mode "\\.tail" 'html-helper-mode)
(add-auto-mode "diary$" 'diary-mode)
(add-auto-mode ".mailrc" 'mailrc-mode)
(add-auto-mode "\\.csv" 'csv-mode)
(add-auto-mode "\\.jpe?g" 'jpeg-mode)
(add-auto-mode "\\.JPE?G" 'jpeg-mode)
(add-auto-mode ".py$" 'python-mode)

;;; end of basics/modes.el

