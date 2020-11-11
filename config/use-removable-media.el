;;;; use-removable-media.el -- look for USB keys etc
;;; Time-stamp: <2020-11-11 20:56:08 jcgs>

(require 'jcgs-use-package)

(jcgs-use-package 'removable-media
	     (expand-file-name "file-handling" user-emacs-directory)
	     "http://www.cb1.com/~john/computing/emacs/file-handling/removable-media.el"
	     ((expand-file-name "webstuff" user-emacs-directory)
	      (mount-removable-top-level-directory "removable-media" nil t)
	      (html-journal-helper-mode "journal" nil t)
	      ("/personal/journal/dates/[/0-9]+/.+\\.html" .
	       html-journal-helper-mode)))

(mapcar 'mount-removable-top-level-directory
	'("watch" "personal" "usb3" "usb4"))

;;; end of use-removable-media.el
