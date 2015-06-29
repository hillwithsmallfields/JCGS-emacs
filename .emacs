;;;; My .emacs file, started Sat Jun 23 12:11:53 2007
;;; Time-stamp: <2015-06-29 15:37:41 johstu01>

;; Copyright (C) 2007, 2008, 2013, 2014, 2015, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

;;; I decided it was time to re-organize, and perhaps strip down, my
;;; .emacs, and to do so using a new principle, a package
;;; configuration package that I'd started to write. My previous
;;; re-organization was to use load-directory, and I'll still use
;;; that.

;;; The triple-hash cookies are processed by
;;; `bundle-emacs-initialization' which packages up all your emacs
;;; initialization into a single file, which uses use-package to load
;;; things for which you've given use-package definitions.  This way,
;;; you can carry around a single file which will haul the rest of
;;; your setup over the net on demand.

(message "Loading setup from %S" load-file-name)

;;;; First things first:
(setq stack-trace-on-error t
      message-log-max t
      max-lisp-eval-depth 4096
      print-length 2048 eval-expression-print-length 2048
      print-level 128 eval-expression-print-level 128
      inhibit-startup-message t
      use-package-verbose t)

(unless (and (getenv "EHOME")
	     (getenv "OPEN_PROJECTS")
	     (getenv "GATHERED"))
  (error "Environment not set up --- probably need to source .bash_profile"))

(unless (and (stringp user-emacs-directory)
	     (file-directory-p user-emacs-directory)
	     (file-directory-p
	      (expand-file-name "basics" user-emacs-directory)))
  (setq user-emacs-directory
	(catch 'found
	  (dolist (raw-dir '("$HOME/JCGS-emacs" "/work/johstu01/JCGS-emacs"))
	    (let ((dir (substitute-in-file-name raw-dir)))
	      (when (and (stringp dir)
			 (file-directory-p dir)
			 (file-directory-p
			  (expand-file-name "basics" dir)))
		(throw 'found dir)))))))

(add-to-list 'load-path user-emacs-directory)

(message "user-emacs-directory is %S" user-emacs-directory)

(random t)

(defun count-commands (&optional format)
  "Return the number of commands in Emacs.
Optionally, a FORMAT for outputting the result can be given."
  (interactive)
  (let ((count 0))
    (mapatoms (lambda (atom)
		(when (commandp atom)
		  (setq count (1+ count)))))
    (when (or format (interactive-p))
      (message (or format "%d commands") count))
    count))

(count-commands "Out of the box, this emacs has %d commands")

(defvar speaking (boundp 'emacspeak-codename)
  "Whether we appear to be on emacspeak.")

;;;; First, some taken straight from my old setup, that is needed to
;;;; find the rest of the files:

;;; The environment variable COMMON is used in substitute-in-file-name
;;; in lots of my elisp, it points to the stuff I have in common, and
;;; edit, and try to keep synchronized, on all the machines I use
;;; regularly. For simplicity, I just copy everything in a directory
;;; called "common".  The master copy lives on a USB flash drive. To
;;; reduce the number of writes to flash memory, I normally copy it
;;; onto each host, rather than using it in-place, although it should
;;; also work in-place.  Likewise, the environment variable GATHERED
;;; points to my collection of downloaded material.

(defun find-main-directory (dirdesc)
  "Find a directory matching DIRDESC.
This should be a list of three parts:
  An environment variable
    that might be pointing to the directory already, and should be
    left pointing to it the directory when found;
  A filename
    that is known to be in the directory, by which the directory
    can be recognized when found;
  A list of places
    where the directory might be."
  (let ((dirname (car dirdesc)))
    (if (stringp (getenv dirname))
	(message "%s already defined as %s" dirname (getenv dirname))
      (let* ((distinctive-file (cadr dirdesc))
	     (possibles (cadr (cdr dirdesc)))
	     (this-try (expand-file-name (car possibles))))
	(message "Trying %s for %s" this-try dirname)
	(while (and (not (null possibles))
		    (or (not (stringp this-try))
			(not (file-exists-p
			      (expand-file-name distinctive-file
						this-try)))))
	  (setq this-try (expand-file-name (car possibles))
		possibles (cdr possibles))
	  (message "Trying %s for %s" this-try dirname))
	(if (stringp this-try)
	    (progn
	      (message "Using %s as %s directory" this-try dirname)
	      (setenv dirname this-try)
	      t)
	  (message "Could not find directory for %S" dirname)
	  nil)))))

(unless
    (find-main-directory
     '("GATHERED" "library-files.el" ("~/library"
				      "/mnt/library"
				      "/work/johstu01/library"
				      "/media/disk/library"
				      "h:/library"
				      "i:/library"
				      "j:/users/jcgs/library"
				      "i:"
				      "h:"
				      "/library"
				      ;; this one for Oralux on my home machine
				      "/mnt/hda6/users/jcgs/library"
				      ;; this one for Fedora on my home machine
				      "/mnt/share1/users/jcgs/library"
				      )))
  (unless (file-directory-p "~/library")
    (make-directory "~/library"))
  (setenv "GATHERED" (expand-file-name "~/library")))

;; (setq source-directory (substitute-in-file-name "~/build/emacs/source/emacs/emacs")
;;       downloaded-emacs-directory (substitute-in-file-name "~/downloaded/"))

;;;; Load experimental patches (do them now, in case they patch the
;;;; loading of other things):

(let ((patches-file (expand-file-name "early-patches.el" user-emacs-directory)))
  (when (file-exists-p patches-file)
    (load-file patches-file)))

;; I would use load-directory here, except that here is where I load
;; load-directory

;;;###include basics/host.el
;;;###include basics/add-lispdir.el
;;;###include basics/use-package.el
;;;###include basics/load-directory.el

;;;###if nil
(let ((basics (expand-file-name "basics" user-emacs-directory)))
  (mapcar (function (lambda (basic)
		      (message "before loading %s, load-path=%S and user-emacs-directory=%S" basic load-path user-emacs-directory)
		      (load-file
		       (expand-file-name (concat basic ".el")
					 basics))
		      (message "after loading %s, load-path=%S and user-emacs-directory=%S" basic load-path user-emacs-directory)))
          '( ;; "version-patches.el"
	    "host"
	    "jcgs-common-setup"
            "add-lispdir"
	    "use-package"
            ;; "modes"
            "load-directory"
            ;; "startup-messages"
            )))
;;;###endif

(add-tree-to-load-path user-emacs-directory)

(setq use-package-skip-these nil)

;; (add-to-list 'use-package-skip-these 'vr-mode)
;; (add-to-list 'use-package-skip-these 'icicles)
;; (add-to-list 'use-package-skip-these 'mulvoc)

;; set some variables used from several of the config files

(defvar work-agenda-file (expand-file-name "/work/johstu01/work-org/work-tasks.org")
  "The name of the file containing my work agenda.")

(defvar work-log-file (if (at-work)
			  (expand-file-name "/work/johstu01/work-org/work.org-log")
			(expand-file-name "~/Dropbox/notes/hackery.org-log"))
  "The name of the file containing my work log.")

(message "before loading config, load-path=%S and user-emacs-directory=%S" load-path user-emacs-directory)

;;;###include config
;;;###if nil
(if t
    (load-directory (expand-file-name "config" user-emacs-directory) t)
  (let ((config-dir (expand-file-name "config" user-emacs-directory)))
    (dolist (elfile '("config-calendar-diary"
		      "config-distribution"
		      "config-elisp-devel"
		      "config-from-emacswiki"
		      "config-gud"
		      "config-international"
		      "config-laptop"
		      "config-misc"
		      "config-org-mode"
		      "config-personal"
		      "config-proglang-modes"
		      "config-projects"
		      "config-ps-print"
		      "config-ratpoison"
		      "config-slime"
		      "config-windows"
		      "jcgs-bindings"
		      "new-files"
		      "use-auctex"
		      "use-bbdb"
		      "use-color-theme"
		      "use-contexts"
		      "use-csv"
		      "use-doremi"
		      ;; "use-duinnin"
		      "use-emms"
		      "use-flashcard"
		      "use-generic-text"
		      "use-html-helper-mode"
		      "use-http-get"
		      "use-icicles"
		      "use-journal"
		      "use-magit"
		      "use-misc"
		      "use-mulvoc"
		      "use-muse"
		      "use-planner"
		      "use-psgml"
		      "use-ratpoison"
		      "use-removable-media"
		      "use-timeclock"
		      "use-type-break"
		      "use-versor"
		      "use-vm"
		      "use-voice-input"
		      "use-w3"
		      "values-vm-archive"))
      (message "Loading config files individually: %s" elfile)
      (load-file (expand-file-name (format "%s.el" elfile)
				   config-dir))
      (if (boundp 'work-log-file)
	  (message "work-log-file is now %S" work-log-file)
	(message "work-log-file is not bound"))
      (message "Loaded config file %s individually" elfile)
      )))
;;;###endif

(message "after loading config, load-path=%S" load-path)

(add-hook 'after-init-hook
	  (lambda ()
	    (message "deleting other windows from after-init-hook")
	    (delete-other-windows))
	  t)

(count-commands "After initialization, this emacs has %d commands")

;; (add-hook 'desktop-after-read-hook 'debug-org-agenda-file-setting-file-post-desktop)

(if (boundp 'org-agenda-files)
    (message "At end of .emacs, org-agenda-files is %S" org-agenda-files)
  (message "At end of .emacs, org-agenda-files is unbound"))

(let ((patches-file (expand-file-name "late-patches.el" user-emacs-directory)))
  (when (file-exists-p patches-file)
    (load-file patches-file)))

;;; end of .emacs
