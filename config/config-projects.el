;;;; Configuration for project-specific things
;;; Time-stamp: <2014-11-04 14:18:13 johstu01>

;; Copyright (C) 2007, 2008, 2009, 2010, 2012, 2013, 2014, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

(add-lispdir (expand-file-name "external-programs" user-emacs-directory))

(require 'multiple-shells)

(make-named-shell (format "=%s=" (system-name)) "~")

(unless (or (at-work)
	    (on-small-machine))
  (dolist (pair '(("-grevo-" . "$OPEN_PROJECTS/GrEvo/trunk/")
		  ("-examples-for-grevo-" . "$OPEN_PROJECTS/GrEvo/trunk/examples/")
		  ("-ulga-" . "$OPEN_PROJECTS/ULGA/trunk/")
		  ("-libGE-" . "$OPEN_PROJECTS/libGE/trunk/")))
    (make-named-shell (car pair) (cdr pair)))
  ( "-rtl-" . "$OPEN_PROJECTS/libRTL/trunk/" )
  (when nil
    (make-named-shell "-ephemerals-"
		      "$COMMON/research/bds/grevo/ephemerals/"
		      )))

(make-named-shell "-muesli-"
		  "$OPEN_PROJECTS/muesli/"
		  ;; "make clean; ./configure; make\n"
		  )

(make-named-shell "-emacs-"
		  "$EMACS")

(when (at-work)
  (make-named-shell "-src-"
		    "/work/johstu01/build/trunk/work/src")
  (make-named-shell "-monit-"
		    "/work/johstu01/build/trunk/work/src/arm.com/uniSched/overseer/")
  (make-named-shell "-dripfeed-"
		    "/work/johstu01/build/trunk/work/src/arm.com/uniSched/dryrun"))

(unless (at-work)
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

(when (and nil (at-work))
  (let ((login-host (format "login%d.euhpc.arm.com" (1+ (random 7)))))
    (make-named-shell (format "=%s=" login-host)
		      "~"
		      (format "echo ssh %s" login-host))))

;;; config-projects.el ends here
