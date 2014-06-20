;;;; JCGS's configuration for ps-print
;;; Time-stamp: <2013-11-21 11:55:00 johnstu>

(setq ps-paper-type 'a4
      ;; ps-lpr-switches nil
      ps-printer-name "Canon_PS"


      ps-spool-duplex nil

      ps-bold-faces '(font-lock-function-name-face font-lock-keyword-face)
      ps-italic-faces '(font-lock-string-face font-lock-comment-face)
      ps-build-face-reference t

      ps-font-size 5

      ps-landscape-mode t
      ps-number-of-columns 2
      ps-inter-column (/ 72 ps-number-of-columns)
      ps-line-spacing 0

      ps-default-fg '(0.0 0.0 0.0)
      ps-default-bg '(1.0 1.0 1.0)

      ps-print-color-p t

      ps-print-header t
      ps-print-header-frame t
      ps-show-n-of-n t
      ps-header-lines 3
      ps-print-only-one-header t
      ps-left-header '(ps-get-buffer-name)
      ps-right-header '("/pagenumberstring load" ps-time-stamp-yyyy-mm-dd ps-time-stamp-hh:mm:ss)
      ps-header-offset 9
      ;; ps-left-header '(ps-header-dirpart ps-get-buffer-name)
      ;; ps-right-header' ("/pagenumberstring load" time-stamp-yy/mm/dd time-stamp-hh:mm:ss)
      )

(defun ps-print-directory-files (directory pattern)
  "Print files in DIRECTORY with names matching PATTERN."
  (interactive "DPrint files in:
sPrint files matching pattern: ")
  (mapcar (lambda (file)
	    (find-file file)
	    (ps-print-buffer-with-faces))
	  (directory-files directory t pattern)))

(defun ps-print-region-with-faces-larger (&rest options)
  "Like `ps-print-region-with-faces' but with a larger font.
Passes all OPTIONS on."
  (interactive (ps-print-preprint-region current-prefix-arg))
  (let ((ps-font-size 8)
	(ps-landscape-mode nil)
	(ps-number-of-columns 1))
    (apply 'ps-print-region-with-faces options)))

(defun ps-print-set-small ()
  "Set the printing to my usual small size."
  (interactive)
  (setq ps-font-size 5
	ps-landscape-mode t
	ps-number-of-columns 2))

(defun ps-print-set-large ()
  "Set the printing to a larger size."
  (interactive)
  (setq ps-font-size 10
	ps-landscape-mode nil
	ps-number-of-columns 1))

;;; end of config-ps-print.el
