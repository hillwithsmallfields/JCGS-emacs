;;;; find, load and configure muse
;;; Time-stamp: <2013-10-15 12:22:13 johnstu>

;; Copyright (C) 2007, 2008, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

;; (defvar notes-on-emacs-source (substitute-in-file-name "$COMMON/notes/emacs")
;;   "Directory of notes on emacs source.")

(defvar jcgs-muse-files-to-upload nil
  "Files to upload to the server.")

(defun jcgs-muse-mode-hook-function ()
  (add-hook 'local-write-file-hooks
	    '(lambda ()
	       (delete-trailing-whitespace)
	       nil)))

(defun jcgs-muse-get-server-path (project)
  "Return the server path for PROJECT."
  (let ((targets (cddr project)))
    (catch 'found
      (mapcar (lambda (target)
		(let ((server-path (cadr (memq :jcgs-server-path target))))
		  (when server-path
		    (throw 'found server-path))))
	      targets)
      nil)))

(defun jcgs-muse-before-project-publish (project)
  "Function to run before publishing a project."
  (message "(jcgs-muse-before-project-publish %S)" project)
  (setq jcgs-muse-files-to-upload nil)
  (let ((server-path (jcgs-muse-get-server-path project)))
    (when (stringp server-path)
      (let* ((host (and (string-match "//\\([^/]+\\)/" server-path)
			(match-string 1 server-path)))
	     (user (ange-ftp-get-user host))
	     (password (ange-ftp-lookup-passwd host user)))
	(message "Want to set password for %s@%s to %s" user host password)
	;; (eldav-process-set-authinfo url realm username password)
	))))

(defun jcgs-muse-after-project-publish (project)
  "Function to run after publishing a project."
  (message "(jcgs-muse-after-project-publish %S)" project)
  (mapcar (lambda (output)
	    (message "Output %S" output)
	    (when (memq :jcgs-after-function output)
	      (let ((source (caadr project))
		    (command (cadr (memq :jcgs-after-function output)))
		    (internal (cadr (memq :path output)))
		    (external (cadr (memq :jcgs-server-path output))))
		(message "%S on %S-->%S-->%S" command source internal external)
		(mapcar (lambda (file)
			  (message "Uploading %S to %S using %S" file external command)
			  (funcall command file (expand-file-name (file-name-nondirectory file)
								  external)))
			jcgs-muse-files-to-upload))))
	  (cddr project)))

(defun jcgs-muse-after-publish ()
  "Function to run after publishing a buffer."
  (message "jcgs-muse-after-publish in %S: %S(%S) --> %S"
	   (current-buffer)
	   muse-current-project
	   muse-publishing-current-file
	   muse-publishing-current-output-path)
  (setq jcgs-muse-files-to-upload (cons muse-publishing-current-output-path jcgs-muse-files-to-upload)))

(add-hook 'after-init-hook
	  (lambda ()
	    (message "muse-mode after-init function")
	    (delete-other-windows)
	    ;; bury-buffers-matching is in config-elisp-devel.el
	    (bury-buffers-matching "\\.muse")))

(add-hook 'desktop-after-read-hook
	  (lambda ()
	    (message "muse-mode after-desktop-read function")
	    (delete-other-windows)
	    ;; bury-buffers-matching is in config-elisp-devel.el
	    (bury-buffers-matching "\\.muse")))

(use-package muse
	     "$GATHERED/emacs/muse/lisp/"
	     "http://www.mwolson.org/projects/EmacsMuse.html" ; todo: get real URL
	     ((expand-file-name "my-extensions-to-packages/muse/" user-emacs-directory)
	      (muse-before-project-publish-hook . jcgs-muse-before-project-publish)
	      (muse-after-project-publish-hook . jcgs-muse-after-project-publish)
	      (muse-after-publish-hook . jcgs-muse-after-publish)
	      (require muse-mode
		       muse-colors
		       muse-wiki
		       muse-html
		       muse-latex
		       muse-journal)
	      ("\\.muse~?$" . muse-mode)
	      (muse-mode "muse" nil t))

	     (setq
	      ;; muse-file-regexp
	      ;; "[/?]\\|\\.\\(html?\\|pdf\\|mp3\\|el\\|zip\\|txt\\|tar\\|doc\\)\\(\\.\\(gz\\|bz2\\)\\)?\\'"
	      muse-html-style-sheet (concat "<link rel=\"stylesheet\""
					    " type=\"text/css\""
					    " charset=\"utf-8\""
					    " media=\"all\""
					    " href=\"this.css\">"))

	     (add-to-list 'muse-project-alist
			  `("Ideas"
			    (,(substitute-in-file-name "$COMMON/jottings")
			     :default "Index"
			     :major-mode muse-mode
			     ;; :visit-link muse-visit-link
			     )
			    (:base "html"
				   :path ,(substitute-in-file-name
					   "$COMMON/www/jottings"))))

	     (add-to-list 'muse-project-alist
			  `("Notes"
			    (,(substitute-in-file-name "$COMMON/notes")
			     :default "Index"
			     :major-mode muse-mode
			     ;; :visit-link muse-visit-link
			     )
			    (:base "html"
				   :path ,(substitute-in-file-name
					   "$COMMON/www/notes"))))

	     (add-to-list 'muse-project-alist
			  `("JCGS emacs"
			    (,(substitute-in-file-name "$COMMON/emacs")
			     :default "jcgs-emacs"
			     :major-mode muse-mode
			     ;; :visit-link muse-visit-link
			     )
			    (:base "html"
				   :path ,(substitute-in-file-name
					   "$COMMON/www/emacs"))))

	     (add-to-list 'muse-project-alist
			  `("Journal"
			    (,(substitute-in-file-name "$COMMON/journal")
			     :default "Index"
			     :major-mode muse-mode
			     ;; :visit-link muse-visit-link
			     )
			    (:base "journal-html"
				   :path ,(substitute-in-file-name "$COMMON/www-journal")
				   :muse-html-footer "<hr><a href=\"../contact.html\">Contact me</a>"
				   ;; :jcgs-after-function copy-file
				   ;; :jcgs-server-path ":http://www.cb1.com/~john/journal"
				   ))))

;;; end of use-muse.el
