(setq stack-trace-on-error t)

(setenv "COMMON" "i:/common")
(setenv "GATHERED" "j:/users/jcgs/library")

(mapcar (lambda (dir)
	  (add-to-list 'load-path
		       (substitute-in-file-name dir)))
	'("$OPEN_PROJECTS/mulvoc/mulvoc/lisp/"
	  "$GATHERED/emacs/csv/"
	  (expand-file-name "natural-language/" user-emacs-directory)))

(setq mulvoc-dictionaries-pattern (if t
				     "test.*\\.csv$"
				   "\\(general\\|basic\\).csv$")
      mulvoc-dictionaries-directories '("i:/common/open-projects/mulvoc/mulvoc/dictionaries")
      mulvoc-etc-directory (substitute-in-file-name "$OPEN_PROJECTS/mulvoc/mulvoc/etc/")
      language-codes-file-name (expand-file-name "LanguageCodes.tab" mulvoc-etc-directory)
      wiktionary-cache-directory (substitute-in-file-name "$GATHERED/wiktionary")
      message-log-max t)

(require 'mulvoc)

(defun use-mulvoc ()
  (interactive)
  (mulvoc-mode 1))

(add-hook 'text-mode-hook 'use-mulvoc)
(add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
(add-hook 'write-file-hooks 'time-stamp)

(mulvoc-setup)

(find-file (substitute-in-file-name "$OPEN_PROJECTS/mulvoc/mulvoc/lisp/mulvoc-wiktionary.el"))
