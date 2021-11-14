;;;; find, load and configure w3
;;; Time-stamp: <2021-11-14 18:31:05 jcgs>

;; (require 'jcgs/use-package)
;; (jcgs/use-package w3
;; 	     "$GATHERED/emacs/w3/w3-4.0pre.47/lisp/"
;; 	     "http://www.cs.indiana.edu/elisp/w3" ; todo: find the full URL
;; 	     ((w3-fetch "w3"
;; 			"Retrieve a document over the World Wide Web.
;; Defaults to URL of the current document, if any.
;; With prefix argument, use the URL of the hyperlink under point instead."
;; 			t))
;; 	     (setq w3-default-homepage
;; 		   (cond
;; 		    (nil "http://www.cb1.com/~john")
;; 		    (t (substitute-in-file-name
;; 			"$SYNCED/local-www/index.html")))
;; 		   w3-configuration-directory
;; 		   (substitute-in-file-name "$SYNCED/var/w3")
;; 		   w3-user-colors-take-precedence t
;; 		   w3-strict-width 77)

;; 	     (when w3-host
;; 	       (define-key menu-bar-tools-menu [w3]
;; 		 (cons "Web browsing" (make-sparse-keymap "Web browsing")))

;; 	       (define-key menu-bar-tools-menu [w3 dove]
;; 		 '("Dove's guide" . dove))

;; 	       (define-key menu-bar-tools-menu [w3 weather]
;; 		 '("Cambridge weather" . weather:page))

;; 	       (define-key menu-bar-tools-menu [w3 bbc-news]
;; 		 '("BBC news" . bbc-news))

;; 	       (define-key menu-bar-tools-menu [w3 oremus]
;; 		 '("Oremus" . oremus))

;; 	       (define-key menu-bar-tools-menu [w3 altavista]
;; 		 '("Altavista" . altavista))

;; 	       (define-key menu-bar-tools-menu [w3 w3]
;; 		 '("Web browser" . w3))

;; 	       (define-key menu-bar-tools-menu [w3 w3-fetch]
;; 		 '("Web fetch" . w3-fetch))))

;;; end of use-w3.el
