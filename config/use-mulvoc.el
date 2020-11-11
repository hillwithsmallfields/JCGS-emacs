;;;; find, load and configure mulvoc
;;; Time-stamp: <2020-11-11 20:56:07 jcgs>

;; Set these here, because they're needed during the loading of
;; mulvoc, and so eval-after-load is too late
(setq mulvoc-dictionary-files
      (list (substitute-in-file-name
	     "$OPEN_PROJECTS/mulvoc/mulvoc/dictionaries/general.csv"))
      input-method-highlight-flag t
      input-method-verbose-flag t
      mulvoc-use-overlays nil
      ;; sendmail-coding-system "UTF-8"
      )

;; (setq mulvoc-displayed-languages (aref daily-languages (nth 6 (decode-time))))

(when nil
  (require 'jcgs-use-package)
(jcgs-use-package mulvoc
	       "$OPEN_PROJECTS/mulvoc/mulvoc/lisp"
	       "http://sourceforge.net/project/showfiles.php?group_id=165695"
	       ((expand-file-name "natural-language" user-emacs-directory)
		(expand-file-name "file-handling" user-emacs-directory)
		"$GATHERED/emacs/csv"	; for convenience
		(csv-vocab-mode "csv-vocab-mode" nil t)
		("mulvoc/dictionaries/.+\\.csv" . csv-vocab-mode)
		("languages/.+\\.csv" . csv-vocab-mode)
		(mulvoc-mode "mulvoc"
				  "Minor mode to show translated words." t)
		(text-mode-hook . mulvoc-mode))
	       (setq mulvoc-languages "GLI,BSQ,KHK,FIN,PRT,PQL,DUT,GER"))
)

;;; end of use-mulvoc.el
