;;;; use-misc.el -- small things to load
;;; Time-stamp: <2013-10-15 12:22:18 johnstu>

(add-to-list 'load-path
	     (expand-file-name "natural-language" user-emacs-directory))

(autoload 'romanize-lookup "romanize")
(autoload 'romanize-initialize "romanize" nil t)
(autoload 'romanize-char-at-point "romanize" nil t)

(add-to-list 'load-path
	     (expand-file-name "my-extensions-to-packages" user-emacs-directory))

;;; end of use-misc.el
