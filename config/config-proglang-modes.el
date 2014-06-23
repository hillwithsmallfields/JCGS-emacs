;;;; Configuration for programming language modes and related things
;;; Time-stamp: <2014-06-23 11:47:31 johstu01>

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;
;; c-mode ;;
;;;;;;;;;;;;

(defun jcgs-c-mode-hook ()
  "My hook for setting up C mode."
  (define-key c-mode-map "\C-cc" 'compile)
  (define-key c-mode-map "\C-cf" 'cflow-file)
  (jcgs-xc-build-setup))

(add-hook 'c-mode-hook 'jcgs-c-mode-hook)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'c++-mode-hook
	  (lambda ()
	    (define-key c++-mode-map "\C-cc" 'compile)
	    (when (file-exists-p "../Makefile")
	      (make-local-variable 'compile-command)
	      (setq compile-command "cd ..; make -k "))))

;;;;;;;;;;;;;;;
;; perl-mode ;;
;;;;;;;;;;;;;;;

(defun jcgs/perl-mode-hook ()
  "My hook for setting up perl mode."
  (setq beginning-of-defun-function 'perl-beginning-of-function
	end-of-defun-function 'perl-end-of-function))

(add-hook 'perl-mode-hook 'jcgs/perl-mode-hook)

;; get round problems with flymake in read-only directories
(let* ((improved-flymake-directory (substitute-in-file-name "$GATHERED/emacs/flymake/emacs-flymake"))
       (elc (expand-file-name "flymake.elc" improved-flymake-directory))
       (source (expand-file-name "flymake.el" improved-flymake-directory)))
  ;; install from https://github.com/illusori/emacs-flymake
  (message "improved flymake directory is nominally %S" improved-flymake-directory)
  (when (file-directory-p improved-flymake-directory)
    (message "improved flymake in %S" improved-flymake-directory)
    (add-to-list 'load-path improved-flymake-directory)
    (let ((improved (if (file-exists-p elc)
			elc
		      source)))
      (message "loading improved %S" improved)
      (load-file improved))))

;;;;;;;;;;;;;;;;;
;; python-mode ;;
;;;;;;;;;;;;;;;;;

(defun jcgs/python-mode-hook ()
  "My hook for setting up python mode."
  (message "In jcgs/python-mode-hook for buffer=%S buffer-read-only=%S file=%S directory=%S" (current-buffer) buffer-read-only (buffer-file-name) default-directory)
  (unless buffer-read-only
    ;; (setq flymake-log-level 3)
    (message "Setting flymake mode for buffer %S" (current-buffer))
    (flymake-find-file-hook)
    (message "Set flymake mode for buffer %S" (current-buffer))))

(defun flymake-pylint-init ()
  "Set up pyling for flymake."
  ;; from http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs#toc13
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(eval-after-load "flymake"
  '(add-to-list 'flymake-allowed-file-name-masks
		;; from http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs#toc13
		'("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook 'jcgs/python-mode-hook)

(defun shell-find-python-line (where)
  "Find the python source mentioned on the line containing WHERE."
  (interactive "d")
  (let ((location
         (save-excursion
           (beginning-of-line)
           (if (re-search-forward "File \"\\([^\"]+\\)\", line \\([0-9]+\\), in" (point-max) t)
               (cons (match-string-no-properties 1)
                     (string-to-number (match-string-no-properties 2)))
             nil))))
    (if location
        (progn
          (find-file-other-window (car location))
          (goto-line (cdr location)))
      (message "Could not find a place description"))))

;;;; Tags

(add-lispdir "$EMACS/file-handling/")

(autoload 'find-tag-with-hooks "tags-with-hooks")

(setq tags-file-name nil
      tags-table-list (list (substitute-in-file-name "$EMACS/tags")
			    (substitute-in-file-name "$GATHERED/emacs/tags")
			    (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/tags")))

(let ((build-dir "~/build/emacs"))
  (if (file-readable-p (expand-file-name "TAGS" build-dir))
      (push build-dir tags-table-list)
    (if (file-readable-p (expand-file-name "tags" source-directory))
	(push (expand-file-name "tags" source-directory) tags-table-list))))

;;;; Change logs

(setq change-log-default-name "../ChangeLog")
