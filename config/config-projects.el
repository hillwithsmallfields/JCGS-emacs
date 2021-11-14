;;;; Configuration for project-specific things
;; Copyright (C) 2007, 2008, 2009, 2010, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

(add-lispdir (expand-file-name "external-programs" user-emacs-directory))

(require 'multiple-shells)

(make-named-shell (format "=%s=" (system-name)) "~")

(unless (or (at-work-p)
	    (on-small-machine))
  (dolist (pair '(("/grevo/" . "$MY_PROJECTS/GrEvo/trunk/")
		  ("/examples-for-grevo/" . "$MY_PROJECTS/GrEvo/trunk/examples/")
		  ("/ulga/" . "$MY_PROJECTS/ULGA/trunk/")
		  ("/libGE/" . "$MY_PROJECTS/libGE/trunk/")))
    (make-named-shell (car pair) (cdr pair)))
  ;; ( "/rtl/" . "$MY_PROJECTS/libRTL/trunk/" )
  (when nil
    (make-named-shell "/ephemerals/"
		      "$SYNCED/research/bds/grevo/ephemerals/"
		      )))

(make-shell-for-directory-if-present "$MY_PROJECTS/muesli/"
				     "/muesli/"
				     ;; "make clean; ./configure; make\n"
				     )

(make-shell-for-directory-if-present "$SYNCED/music/"
				     "/music/"
				     ;; "make clean; ./configure; make\n"
				     )

(make-shell-for-directory-if-present "$MY_ELISP"
				     "/gnuemacs/")

(make-shell-for-directory-if-present "$MY_PROJECTS/makers/"
                                     "/makers/")

(make-shell-for-directory-if-present "$MY_PROJECTS/Simple_client_server/"
                                     "/server/")

(make-shell-for-directory-if-present "$MY_PROJECTS/Simple_client_server/"
                                     "/client/")

(unless (at-work-p)
  (make-shell-for-directory-if-present "$MY_PROJECTS/gos/"
				       "/gos/"
				       ;; "make clean; ./configure; make\n"
				       )
  (make-shell-for-directory-if-present "$MY_PROJECTS/mulvoc/mulvoc/"
				       "/mulvoc/"
				       ;; "make clean; ./configure; make\n"
				       )
  (when t
    (make-shell-for-directory-if-present "$WRITING/fiction/last-khan/"
					 "/khanate/"
					 ;; "latex last-khan.tex\n"
					 ))
  (make-shell-for-directory-if-present "$MY_PROJECTS/qs"
				       "/qs/")
  (make-shell-for-directory-if-present "$MY_PROJECTS/noticeboard"
				       "/noticeboard/")
  (make-shell-for-directory-if-present "$MY_PROJECTS/coimealta"
				       "/coimealta/")
  (make-shell-for-directory-if-present "$MY_PROJECTS/textualosm"
				       "/osm/")
  (make-shell-for-directory-if-present "$MY_PROJECTS/boxes"
				       "/boxes/")
  (make-shell-for-directory-if-present "$MY_PROJECTS/land"
				       "/land/"))

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
