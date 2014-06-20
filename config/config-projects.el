;;;; Configuration for project-specific things
;;; Time-stamp: <2014-05-27 12:02:34 jcgs>

;; Copyright (C) 2007, 2008, 2009, 2010, 2012, 2013, 2014, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

(add-lispdir (expand-file-name "external-programs" user-emacs-directory))

(require 'multiple-shells)

(make-named-shell (format "=%s=" (system-name)) "~")

(unless (or (string-match "ezra" (system-name))
	    (string-match "xci-test" (system-name)))
  (make-named-shell "-grevo-"
		    "$OPEN_PROJECTS/GrEvo/trunk/"
		    ;; "make clean; ./configure\n"
		    )
  (make-named-shell "-examples-for-grevo-"
		    "$OPEN_PROJECTS/GrEvo/trunk/examples/")
  (make-named-shell "-ulga-"
		    "$OPEN_PROJECTS/ULGA/trunk/"
		    ;; "make clean; ./configure; make\n"
		    )
  (make-named-shell "-libGE-"
		    "$OPEN_PROJECTS/libGE/trunk/"
		    ;; "make clean; ./configure; make\n"
		    )

  (make-named-shell "-rtl-"
		    "$OPEN_PROJECTS/libRTL/trunk/"
		    ;; "make clean; ./configure; make\n"
		    )
  (when nil
    (make-named-shell "-ephemerals-"
		      "$COMMON/research/bds/grevo/ephemerals/"
		      )))

(make-named-shell "-muesli-"
		  "$OPEN_PROJECTS/muesli/"
		  ;; "make clean; ./configure; make\n"
		  )

(when (string-match "hosea\\|ezra" (system-name))
  (make-named-shell "-gos-"
		    "$OPEN_PROJECTS/gos/"
		    ;; "make clean; ./configure; make\n"
		    )
  (make-named-shell "-mulvoc-"
		    "$OPEN_PROJECTS/mulvoc/mulvoc/"
		    ;; "make clean; ./configure; make\n"
		    )
  (when t
    (make-named-shell "-khanate-"
		      "$WRITING/fiction/last-khan/"
		      ;; "latex last-khan.tex\n"
		      ))
  )

;; (make-variable-buffer-local 'tracking-org-file)

;; (defun jcgs-select-work-log ()
;;   "This function is meant to go on `find-file-hook'."
;;   (cond
;;    ((string-match (substitute-in-file-name "$COMMON/Marmalade") default-directory)
;;     (setq tracking-org-file (substitute-in-file-name "$COMMON/Marmalade/Marmalade-work.log")))))

;; (add-hook 'find-file-hook 'jcgs-select-work-log)

;;; config-projects.el ends here
