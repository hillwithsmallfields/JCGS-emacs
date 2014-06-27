;;;; config-elisp-devel.el -- set up my emacs-lisp development
;;; Time-stamp: <2014-06-23 11:47:31 johstu01>

;; Copyright (C) 2007, 2014, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

(setq downloaded-emacs-directory (substitute-in-file-name "~/downloaded/emacs/")
      source-annotations-directory (file-truename
				    (substitute-in-file-name "$COMMON/www/computing/emacs")))

(add-lispdir (expand-file-name "file-handling" user-emacs-directory))

(autoload 'annotation-mode "source-annotation"
    "Major mode for annotation of source code." t)

;; (add-hook 'find-file-hook 'find-accompanying-annotation-file)

(autoload 'find-accompanying-annotation-file "source-annotation"
    "Find the annotation file accompanying the file of the current buffer.
If the buffer is not visiting a file in a directory in muse-source-annotations-alist,
this does nothing.
It is meant for use as a find-file-hook, but can also be used interactively." t)

(defun bury-buffers-matching (pattern)
  "Bury all buffers matching PATTERN."
  (interactive "sBury buffers matching: ")
  (mapcar (lambda (buffer)
	    ;; (message "bury %S?" buffer)
	    (when (string-match pattern
				(buffer-name buffer))
	      ;; (message "burying %S" buffer)
	      (bury-buffer buffer)))
	  (buffer-list)))

(add-hook 'after-init-hook
	  (lambda ()
	    (message "annotation after-init function")
	    (delete-other-windows)
	    (bury-buffers-matching "-notes.html")))

(add-hook 'desktop-after-read-hook
	  (lambda ()
	    (message "annotation after-desktop-read function")
	    (delete-other-windows)
	    (bury-buffers-matching "-notes.html")))

(defun find-function-or-library ()
  (interactive)
  (if (save-excursion
	(beginning-of-defun)
	(looking-at "(require '\\(\\(?:\\sw\\|\\s_\\)+\\))"))
      (find-library (read-library-name "Find library: "
				       (match-string-no-properties 1)))
    (find-function (car (find-function-read)))))

(define-key emacs-lisp-mode-map "\M-." 'find-function-or-library)
(define-key emacs-lisp-mode-map "\C-x4." 'find-function-other-window)
(define-key emacs-lisp-mode-map "\C-x5." 'find-function-other-frame)

(setq print-length nil
      eval-expression-print-length nil
      print-level nil
      eval-expression-print-level nil
      read-quoted-char-radix 16)

(defun jcgs-edebug-setup ()
  ;; middle pedal for right foot (generally means "next")
  (define-key edebug-mode-map [ kp-divide ] 'edebug-step-mode))

(add-hook 'edebug-setup-hook 'jcgs-edebug-setup)

;;;; lisp-mnt etc

(require 'lisp-mnt)
(require 'copyright)

(setq copyright-names-regexp "J\\(ohn\\)? \\(C\\. G\\. \\)?Sturdy")

(add-hook 'before-save-hook 'copyright-update)

(require 'checkdoc)

(setq checkdoc-ispell-lisp-words (append '("versor" "mulvoc")
					 checkdoc-ispell-lisp-words))

(push "versor" checkdoc-proper-noun-list)
(push "mulvoc" checkdoc-proper-noun-list)

(add-hook 'emacs-lisp-mode-hook
	  '(lambda () (checkdoc-minor-mode 1)))

(add-lispdir (expand-file-name "elisp-dev-tools" user-emacs-directory))

(require 'elisp-admin)
(require 'auto-show-doc)
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (add-hook 'post-command-hook 'auto-show-doc t t)))

;; (add-hook 'emacs-lisp-mode-hook 'jcgs-check-boilerplate)

;;;; Literate Lisp Programming

(autoload 'llp-extract "llp-extract"
  "Extract Lisp code from FILE." t)

;;;; Byte-compilation

(setq byte-compile-generate-call-tree t)

;;; end of config-elisp-devel.el
