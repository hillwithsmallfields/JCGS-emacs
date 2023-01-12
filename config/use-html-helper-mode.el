;;;; find, load and configure html-helper-mode
;;; Time-stamp: <2022-03-15 10:25:47 jcgs>

;; (require 'jcgs-use-package)

;; (defun john-html-helper-write-hook ()
;;   "Fancy stuff on writing an HTML file."
;;   (html-find-bad-mailto)
;;   (update-recent-changes-file (buffer-file-name)))

;; (add-to-list 'load-path (expand-file-name "my-extensions-to-packages/emacspeak"
;; 					  user-emacs-directory))

;; (jcgs/use-package html-helper-mode
;; 	     "$GATHERED/emacs/html-helper-mode"
;; 	     "http://www.santafe.edu/~nelson/tools/html-helper-mode.el"
;; 	     ((expand-file-name "webstuff" user-emacs-directory)
;; 	      (html-helper-mode "html-helper-mode"
;; 				nil t)
;; 	      ("\\.html" . html-helper-mode)
;; 	      (require mailto-checker)
;; 	      (expand-file-name "webstuff" user-emacs-directory)
;; 	      (expand-file-name "webstuff/webmastering" user-emacs-directory)
;; 	      (html-re-tail-page "change-html-head-tail" nil t)
;; 	      (html-re-head-page "change-html-head-tail" nil t)
;; 	      (html-re-tail-directory "change-html-head-tail" nil t)
;; 	      (html-re-head-directory "change-html-head-tail" nil t)
;; 	      )
;; 	     (setq html-helper-address-string
;; 		   (format "<a href=\"%s\">John Sturdy</a>\n"
;; 			   ;; (user-login-name) (system-domain-name)
;; 			   ;; todo: work out what address to give, by which directory we are in
;; 			   ;; "john" "cb1.com"
;; 			   (if (string-match "ul.ie" (system-name))
;; 			       "mailto:john.sturdy@ul.ie"
;; 			     "http://www.cb1.com/~john/contact.html"))
;; 		   html-helper-do-write-file-hooks t
;; 		   html-helper-timestamp-start "<i><!-- timestamp -->"
;; 		   html-helper-timestamp-end "</i>"
;; 		   html-use-font-lock t
;; 		   html-helper-do-write-file-hooks t
;; 		   html-helper-build-new-buffer t
;; 		   tempo-template-html-paragraph '("<p>" (r "Text: ") "</p>\n")
;; 		   ;; todo: work this one out according to which directory I'm in
;; 		   ;; html-helper-new-buffer-template html-helper-new-buffer-template
;; 		   ))

;;; end of use-html-helper-mode.el
