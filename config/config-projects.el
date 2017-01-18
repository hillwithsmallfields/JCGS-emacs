;;;; Configuration for project-specific things
;;; Time-stamp: <2017-01-18 16:21:33 johstu01>

;; Copyright (C) 2007, 2008, 2009, 2010, 2012, 2013, 2014, 2015, 2016, 2017, John C. G. Sturdy

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
  ;; (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src" "-src-")
  ;; (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/uniSched" "-uniSched-")
  ;; (make-shell-for-directory-if-present "/nethome/johstu01/fpga/trunk/work/farms" "-farms-")
  ;; (make-shell-for-directory-if-present "/nethome/johstu01/fpga/trunk/work/hapsfarm/" "-hapsfarm-" 
  ;; 				       ". ~/.bash_profile\n" ; ". ~/.bash_profile; module load eda synopsys/xactors/2015.03"
  ;; 				       )
  ;; (make-shell-for-directory-if-present "/nethome/johstu01/fpga/trunk/work/profpgafarm" "-profpgafarm-"
  ;; 				       ". ~/.bash_profile\n" ;". ~/.bash_profile; module load eda prodesign/proFPGA/2015C"
  ;; 				       )
  ;; (make-shell-for-directory-if-present "/nethome/johstu01/fpga/trunk/work/autotesting" "-autotesting-")
  ;; (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/gocommons" "-gocommons-")
  ;; (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/uniSched/overseer" "-monit-")
  ;; (make-shell-for-directory-if-present "/work/johstu01/build/trunk/work/src/arm.com/uniSched/dryrun" "-dripfeed-")
  )

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

(defun run-command-in-shell-buffer (command buffer)
  "Run COMMAND in BUFFER and return the result."
  (interactive "sCommand: bRun %s in buffer: \n")
  (set-buffer buffer)
  (goto-char (point-max))
  (let ((start (point)))
    (comint-send-string (get-buffer-process buffer)
	(concat command "\n"))
    (sit-for 1)
    (goto-char (point-max))
    ;; (re-search-backward comint-prompt-regexp)
    ;; (beginning-of-line -1)
    (buffer-substring-no-properties start (point))))

(defun get-shell-cwd (buffer)
  "Get the directory of the shell in BUFFER."
  (interactive "bBuffer: ")
  (car (split-string (run-command-in-shell-buffer "pwd" buffer) "\n")))

(defun get-shell-host (buffer)
  "Get the host of the shell in BUFFER."
  (interactive "bBuffer: ")
  (car (split-string (run-command-in-shell-buffer "hostname -f" buffer) "\n")))

(defun get-shell-locations ()
  "Return a list of the shell buffers and their hosts and directories."
  (save-window-excursion
    (let ((result nil))
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (and (eq major-mode 'shell-mode)
		   (get-buffer-process buf))
	  (push (list (buffer-name buf)
		      (get-shell-host buf)
		      (get-shell-cwd buf))
		result)))
      result)))

(defun list-shell-locations ()
  "List the shell locations."
  (interactive)
  (find-file "~/emacs-shells.el")
  (erase-buffer)
  (dolist (buf (get-shell-locations))
    (insert (format "(make-named-shell \"%s\" \"%s\" \"%s\")\n"
		    (first buf)
		    (third buf)
		    (second buf))))
  (basic-save-buffer))

;;; config-projects.el ends here
