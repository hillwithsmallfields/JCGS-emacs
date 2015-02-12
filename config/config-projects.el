;;;; Configuration for project-specific things
;;; Time-stamp: <2015-02-12 11:25:35 johstu01>

;; Copyright (C) 2007, 2008, 2009, 2010, 2012, 2013, 2014, 2015, John C. G. Sturdy

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
  ;; ( "-rtl-" . "$OPEN_PROJECTS/libRTL/trunk/" )
  (when nil
    (make-named-shell "-ephemerals-"
		      "$COMMON/research/bds/grevo/ephemerals/"
		      )))

(make-shell-for-directory-if-present "-muesli-"
				     "$OPEN_PROJECTS/muesli/"
				     ;; "make clean; ./configure; make\n"
				     )

(make-shell-for-directory-if-present "-emacs-"
				     "$EMACS")

 (when (at-work)
  (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src" "-src-")
  (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/uniSched" "-uniSched-")
  (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/gocommons" "-gocommons-")
  (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/uniSched/overseer" "-monit-")
  (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/uniSched/dryrun" "-dripfeed-"))

(unless (at-work)
  (make-shell-for-directory-if-present "-gos-"
				       "$OPEN_PROJECTS/gos/"
				       ;; "make clean; ./configure; make\n"
				       )
  (make-shell-for-directory-if-present "-mulvoc-"
				       "$OPEN_PROJECTS/mulvoc/mulvoc/"
				       ;; "make clean; ./configure; make\n"
				       )
  (when t
    (make-shell-for-directory-if-present "-khanate-"
					 "$WRITING/fiction/last-khan/"
					 ;; "latex last-khan.tex\n"
					 ))
  )

(when (and nil (at-work))
  (let ((login-host (format "login%d.euhpc.arm.com" (1+ (random 7)))))
    (make-shell-for-directory-if-present (format "=%s=" login-host)
					 "~"
					 (format "echo ssh %s" login-host))))

;;; config-projects.el ends here
