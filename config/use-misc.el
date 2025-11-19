;;;; use-misc.el -- small things to load
;;; Time-stamp: <2025-11-19 17:26:25 jcgs>

(add-lispdir "$MY_ELISP/natural-language")

(autoload 'romanize-lookup "romanize")
(autoload 'romanize-initialize "romanize" nil t)
(autoload 'romanize-char-at-point "romanize" nil t)

(add-lispdir "$MY_ELISP/my-extensions-to-packages")

;;; end of use-misc.el
