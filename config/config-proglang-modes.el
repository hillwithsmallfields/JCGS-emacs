;;;; Configuration for programming language modes and related things
;;; Time-stamp: <2021-10-30 15:31:23 jcgs>

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

(defvar jcgs/leave-multiple-blank-lines nil
  "Suppress my usual tidying of multiple blank lines.")

(defun jcgs/leave-multiple-blank-lines (arg)
  "Set suppression of blank line tidying according to ARG."
  (interactive "p")
  (setq jcgs/leave-multiple-blank-lines (and (numberp arg) (> arg 0))))

(make-variable-buffer-local 'jcgs/leave-multiple-blank-lines)

(defvar jcgs/leave-tabs-alone nil)

(defun jcgs/regularize-whitespace ()
  "Regularize whitespace, typically before saving a file."
  (let* ((tabs (count-matches "	" (point-min) (point-max)))
	 (lines (count-lines (point-min) (point-max)))
	 (delete-trailing-lines t))
    ;; untabify only if there are just a few tabs
    (when (and (not jcgs/leave-tabs-alone)
               (> (* tabs 24) lines))
      (untabify (point-min) (point-max)))
    (unless jcgs/leave-multiple-blank-lines
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "\n\n\n" (point-max) t)
          (forward-line -1)
          (delete-blank-lines))))
    (delete-trailing-whitespace)))

;;;;;;;;;;;;
;; c-mode ;;
;;;;;;;;;;;;

(message "Setting up c-mode")

(defun jcgs-c-mode-hook ()
  "My hook for setting up C mode."
  (define-key c-mode-map "\C-cc" 'compile)
  (define-key c-mode-map "\C-cf" 'cflow-file)
  (setq c-basic-offset 4)
  (add-hook 'before-save-hook 'jcgs/regularize-whitespace nil t))

(add-hook 'c-mode-hook 'jcgs-c-mode-hook)

(defun jcgs-c++-mode-hook ()
  "My hook for setting up C++ mode."
  (define-key c++-mode-map "\C-cc" 'compile)
  (when (file-exists-p "../Makefile")
    (make-local-variable 'compile-command)
    (setq compile-command "cd ..; make -k "))
  (jcgs-c-mode-hook))

(add-hook 'c++-mode-hook 'jcgs-c++-mode-hook)

;;;; cflow-mode

(defun use-latest-version (project-directory pattern file)
  "Use the latest version in PROJECT-DIRECTORY/PATTERN containing FILE."
  (if (file-directory-p project-directory)
      (catch 'found
        (dolist (version (reverse (directory-files project-directory
					           t
					           pattern
					           nil)))
          (let ((dir (expand-file-name "elisp" version)))
	    (when (file-exists-p (expand-file-name file dir))
	      (message "Using %s for %s" dir file)
	      (add-to-list 'load-path dir)
	      (throw 'found version))))
        (message "Could not find a version in %s that contains %s"
	         project-directory file)
        nil)
    nil))

(when (use-latest-version (substitute-in-file-name "$OPEN_PROJECTS/cflow")
		          "cflow-[0-9]+\\.[0-9]+"
		          "cflow-mode.el")
  (autoload 'cflow-mode "cflow-mode")
  (add-to-list 'auto-mode-alist (cons "\\.cflow$" 'cflow-mode))
  (require 'cflow-mode)
  (setq cflow-source-beside-callgraph t))

;;;;;;;;;;;;;;;
;; perl-mode ;;
;;;;;;;;;;;;;;;

(message "Setting up perl-mode")

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

(message "Setting up python-mode")

(defvar jcgs/use-python-flymake nil
  "Whether to use flymake for python.")

(defun jcgs/pylint-available ()
  "Return whether pylint is available."
  (not (zerop (length (shell-command-to-string "which epylint")))))

(defun jcgs/python-mode-hook ()
  "My hook for setting up python mode."
  (message "In jcgs/python-mode-hook for buffer=%S buffer-read-only=%S file=%S directory=%S" (current-buffer) buffer-read-only (buffer-file-name) default-directory)
  (add-hook 'before-save-hook 'jcgs/regularize-whitespace nil t)
  (when (and jcgs/use-python-flymake (jcgs/pylint-available))
    (unless buffer-read-only
      (require 'flymake)
      ;; (setq flymake-log-level 3)
      (message "Setting flymake mode for buffer %S" (current-buffer))
      (flymake-find-file-hook)
      (message "Set flymake mode for buffer %S" (current-buffer)))))

(defun flymake-pylint-init ()
  "Set up epylint for flymake."
  ;; from http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs#toc13
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(when jcgs/use-python-flymake
  (eval-after-load "flymake"
    '(add-to-list 'flymake-allowed-file-name-masks
		  ;; from http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs#toc13
		  '("\\.py\\'" flymake-pylint-init))))

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

(add-to-list 'auto-mode-alist (cons "scons\\(crip\\|truc\\)t" 'python-mode))

;;;;;;;;;;;;;
;; go-mode ;;
;;;;;;;;;;;;;

(message "Setting up go-mode")

(when (catch 'found-go
	(dolist (dir (list "/usr/local/go/misc/emacs/"
			   (substitute-in-file-name "$GATHERED/emacs/go-mode")))
	  (when (file-directory-p dir)
	    (add-to-list 'load-path dir)
	    (throw 'found-go t)))
	nil)
  (add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))
  (require 'go-mode))

(defun jcgs/go-mode-hook-function ()
  "My setup function for golang buffers."
  (local-set-key "\M-." 'godef-jump)
  (local-set-key "\C-c\C-c" 'compile)
  (make-local-variable 'jcgs/leave-tabs-alone)
  (setq jcgs/leave-tabs-alone t)
  (add-hook 'before-save-hook 'jcgs/regularize-whitespace nil t))

(add-hook 'go-mode-hook 'jcgs/go-mode-hook-function)

;;;;;;;;;;;;;;;;;;
;; arduino-mode ;;
;;;;;;;;;;;;;;;;;;

(let ((dir (substitute-in-file-name "$OPEN_PROJECTS/arduino-mode")))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (autoload 'arduino-mode "arduino-mode"
        "Major mode for editing Arduino code."
	t)
    (add-to-list 'auto-mode-alist (cons "\\.ino\\'" 'arduino-mode))))

;;;;;;;;;;
;; Tags ;;
;;;;;;;;;;

(message "Setting up tags")

(add-lispdir (expand-file-name "file-handling" user-emacs-directory))

(autoload 'find-tag-with-hooks "tags-with-hooks")

(setq tags-file-name nil
      tags-table-list (list (substitute-in-file-name (expand-file-name "TAGS" user-emacs-directory))
			    (substitute-in-file-name "$GATHERED/emacs/TAGS")
			    (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/TAGS")))

(let ((build-dir "~/build/emacs"))
  (if (file-readable-p (expand-file-name "TAGS" build-dir))
      (push build-dir tags-table-list)
    (if (file-readable-p (expand-file-name "tags" source-directory))
	(push (expand-file-name "tags" source-directory) tags-table-list))))

;;;;;;;;;;;;;;;;
;; scala-mode ;;
;;;;;;;;;;;;;;;;

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/scala-mode2"))

(autoload 'scala-mode "scala-mode2"
    "Major mode for editing scala code.

When started, runs `scala-mode-hook'." t)

(add-to-list 'auto-mode-alist (cons "\\.scala\\'" 'scala-mode))

;;;;;;;;;;;;;;;;;;
;; clojure-mode ;;
;;;;;;;;;;;;;;;;;;

;; fetch from https://github.com/clojure-emacs/clojure-mode.git and put in $GATHERED/clojure-mode

(let* ((open-projects-clojure-mode-directory ; from https://github.com/clojure-emacs/clojure-mode.git
	(substitute-in-file-name "$OPEN_PROJECTS/clojure-mode"))
       (clojure-mode-directory (if (file-directory-p open-projects-clojure-mode-directory)
				   open-projects-clojure-mode-directory
				 (substitute-in-file-name "$GATHERED/emacs/clojure-mode"))))
  (when (file-directory-p clojure-mode-directory)
    (add-to-list 'load-path clojure-mode-directory)
    (add-to-list 'auto-mode-alist (cons "\\.clj" 'clojure-mode))
    (autoload 'clojure-mode "clojure-mode"
      "Major mode for editing Clojure code." t)
    (let ((open-projects-cider-directory ; https://github.com/clojure-emacs/cider.git
	   (substitute-in-file-name "$OPEN_PROJECTS/cider")))
      (when (file-directory-p open-projects-cider-directory)
	(add-to-list 'load-path open-projects-cider-directory)))))

;;;;;;;;;;;;;;;;;
;; Change logs ;;
;;;;;;;;;;;;;;;;;

(message "Setting up change logs")

(setq change-log-default-name "../ChangeLog")

;;;;;;;;;;;;;;;
;; scad-mode ;;
;;;;;;;;;;;;;;;

(let ((scad-dir (substitute-in-file-name "$GATHERED/emacs/openscad")))
  (when (file-directory-p scad-dir)
    (add-to-list 'load-path scad-dir)
    (autoload 'scad-mode "scad-mode" "Major mode for editing scad files." t)
    (add-to-list 'auto-mode-alist (cons "\\.scad\\'" 'scad-mode))))

;;;;;;;;;;;;;;;
;; blueprint ;;
;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist (cons "\\.bp" 'javascript-mode))

;;;;;;;;;;;;;
;; haskell ;;
;;;;;;;;;;;;;

(defun jcgs/haskell-mode-setup ()
  "Set up my haskell mode parameters."
  (set (make-local-variable 'compile-command) "cabal build "))

(add-hook 'haskell-mode-hook 'jcgs/haskell-mode-setup)

;;;;;;;;;;
;; rust ;;
;;;;;;;;;;

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/github.com/rust-lang/rust-mode"))

(autoload 'rust-mode "rust-mode"
  "Major mode for Rust code.

\\{rust-mode-map}" t)

(add-to-list 'auto-mode-alist (cons "\\.rs\\'" 'rust-mode))

;;;; config-proglang-modes.el ends here
