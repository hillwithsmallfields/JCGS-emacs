;; ;;;; find, load and configure planner
;; ;;; Time-stamp: <2014-07-04 16:17:42 johstu01>

;; ;; Copyright (C) 2007, 2014, John C. G. Sturdy

;; ;; Author: John C. G. Sturdy <john@cb1.com>
;; ;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; ;; Created: 2007
;; ;; Keywords: setup

;; ;; This file is NOT part of GNU Emacs.

;; (defvar jcgs-planner-map (make-sparse-keymap)
;;   "Map for JCGS' planner actions.")

;; (fset 'jcgs-planner-map jcgs-planner-map)

;; (define-key jcgs-planner-map "h" 'jcgs-planner-key-help)
;; (define-key jcgs-planner-map "t" 'planner-create-task-from-buffer)
;; (define-key jcgs-planner-map "T" 'planner-create-task)
;; (define-key jcgs-planner-map "p" 'plan)
;; (define-key jcgs-planner-map "g" 'planner-goto-today-and-current-task)
;; (define-key jcgs-planner-map "u" 'planner-update-task)
;; (define-key jcgs-planner-map "U" 'planner-id-update-tasks-on-page)
;; (define-key jcgs-planner-map "d" 'planner-task-done)
;; (define-key jcgs-planner-map [ up ] 'planner-raise-task-priority)
;; (define-key jcgs-planner-map [ down ] 'planner-lower-task-priority)
;; (define-key jcgs-planner-map "m" 'planner-replan-task)
;; (define-key jcgs-planner-map "r" 'planner-copy-or-move-task)
;; (define-key jcgs-planner-map "?" 'planner-show-current-task)
;; (define-key jcgs-planner-map [ f4 ] 'planner-dwim)

;; ;; Do these early, in case of finding a planner file when muse is
;; ;; already loaded, but planner isn't yes loaded.

;; (setq planner-project "WikiPlanner"
;;       muse-project-alist (cons `("WikiPlanner"
;; 				 (,(substitute-in-file-name "$COMMON/var/plans")
;; 				  :default "TaskPool"
;; 				  :major-mode planner-mode
;; 				  :visit-link planner-visit-link)
;; 				 (:base "planner-xhtml"
;; 				  :path ,(substitute-in-file-name
;; 					  "$COMMON/local-www/plans")
;; 				  :jcgs-before-function 'jcgs-planner-pre-publish-function))
;; 			       (when (boundp 'muse-project-alist)
;; 				 muse-project-alist
;; 				 nil))
;;       timeclock-file (substitute-in-file-name "$COMMON/var/timelog"))

;; (use-package planner
;; 	     "$GATHERED/emacs/planner/planner-3.40"
;; 	     "http://www.plannerlove.com/" ; todo: get real URL
;; 	     ((expand-file-name "information-management" user-emacs-directory)
;; 	      (require muse-mode
;; 		       jcgs-planner-stuff
;; 		       planner-publish
;; 		       ;; planner-vm
;; 		       planner-diary
;; 		       ;; planner-w3m
;; 		       planner-bibtex planner-bookmark planner-log-edit
;; 		       planner-timeclock planner-timeclock-summary
;; 		       planner-accomplishments
;; 		       ;; planner-bbdb
;; 		       ;; planner-check-directory
;; 		       planner-cyclic planner-trunk)
;; 	      ([ f4 ] . jcgs-planner-map)
;; 	      (plan "planner" "Set up the plan for today" t)
;; 	      (planner-create-task-from-buffer "planner" nil t)
;; 	      (planner-create-task "planner" nil t)
;; 	      (planner-goto-today "planner" nil t)
;; 	      (planner-mode "planner" nil t)
;; 	      (bury-previous-buffer-if-planner "jcgs-planner-stuff"
;; 					       "Function to go on buffer-selection-hook to keep planner buffers out of the way.")
;; 	      (planner-mark-task-hook . planner-highlight-current-task)
;; 	      (type-break-start-break-hook . planner-start-typing-break)
;; 	      (type-break-end-break-hook . planner-end-typing-break)
;; 	      (planner-mode-hook . planner-trunk-tasks)
;; 	      (planner-mode-hook . turn-off-auto-fill)
;; 	      (planner-mode-hook . start-deleting-whitespace-on-save)
;; 	      (planner-mode-hook . use-utf-8)
;; 	      (buffer-selection-hook . bury-previous-buffer-if-planner))

;; 	     (mapcar 'add-lispdir
;; 		     '("$GATHERED/emacs/muse/lisp"
;; 		       "$GATHERED/emacs/planner/planner-3.40"
;; 		       "$GATHERED/emacs/remember"
;; 		       (expand-file-name "my-extensions-to-packages/planner" user-emacs-directory)))

;; 	     (setq planner-day-page-template
;; 		   "* Aims\n\n* Tasks\n\n** Done\n\n* Diary\n\n<lisp>(planner-diary-entries-here)</lisp>
;;           \n\n* Notes\n\n* Accomplishments\n\n* Timeclock\n\n* Report\n\n<timeclock-report>\n"
;; 		   planner-plan-page-template "* Aims\n\n\n* Tasks\n\n\n* Notes\n\n\n"
;; 		   planner-diary-number-of-days 3
;; 		   planner-diary-file diary-file
;; 		   planner-tasks-file-behavior 'save
;; 		   remember-data-file (substitute-in-file-name "$COMMON/var/notes")
;; 		   remember-handler-functions '(remember-planner-append)
;; 		   remember-annotation-functions planner-annotation-functions
;; 		   jcgs-use-planner-id t
;; 		   planner-cyclic-diary-file (substitute-in-file-name "$COMMON/var/diary-cyclic-tasks")
;; 		   planner-cyclic-diary-nag nil
;; 		   planner-trunk-rule-list
;; 		   '(("." "Done" ("Focloir\\|Cursa-Scriobh-Ilmhean"
;; 				  "PlannerControl\\|Emacs\\|EmacsComprehension\\|EmacsLearning"
;; 				  "Versor\\|ProgrammerInteraction"
;; 				  "Sidebrain\\|Mulvoc\\|WikiRoads"
;; 				  "ProgLangResearch"
;; 				  "Web\\|FictionWriting"
;; 				  "download\\|Searches"
;; 				  "Marmalade\\|ASR-33\\|Trike\\|House\\|Mechanical"
;; 				  "TaskPool\\|Admin\\|Personal")))

;; 		   ;; avoid getting extra spaces put in when changing priority
;; 		   planner-task-format "#%s%-0s %s %s%s"

;; 		   planner-annotation-use-relative-file (function
;; 							 (lambda (file)
;; 							   (let ((common (getenv "COMMON")))
;; 							     (string= common
;; 								      (substring file 0 (length common))))))
;; 		   planner-annotation-strip-directory t

;; 		   ;; put these off by default until I have them working well
;; 		   planner-remind-directory-change 3
;; 		   planner-prompt-directory-change nil
;; 		   planner-bury-buffers t)

;; 	     (message "set timeclock-file to %S" timeclock-file)

;; 	     (planner-accomplishments-insinuate)
;; 	     (planner-timeclock-summary-insinuate)

;; 	     (make-variable-buffer-local 'planner-current-task-overlay)

;; 	     (unless (memq 'planner-timeclock-current-task-short mode-line-format)
;; 	       (add-to-end-of-mode-line " -- ")
;; 	       (add-to-end-of-mode-line 'planner-timeclock-current-task-short))

;; 	     (set-face-background 'planner-high-priority-task-mode-line-face "yellow")
;; 	     ;; (set-face-background 'planner-medium-priority-task-mode-line-face "black")
;; 	     ;; (set-face-background 'planner-low-priority-task-mode-line-face "black")

;; 	     (autoload 'remember "remember" nil t)
;; 	     (autoload 'remember-region "remember" nil t)

;; 	     (require 'remember-planner)

;; 	     (when jcgs-use-planner-id
;; 	       (require 'planner-id)
;; 	       (setq planner-id-tracking-file (substitute-in-file-name "$COMMON/var/planner-ids"))
;; 	       (set-face-foreground 'planner-id-face "darkgray")
;; 	       (setq planner-id-update-automatically nil)
;; 	       (make-variable-buffer-local 'planner-id-update-automatically)
;; 	       (add-hook 'planner-mode-hook
;; 			 (lambda ()
;; 			   (setq goal-column 5
;; 				 planner-id-update-automatically nil ; not sure, may be causing quite a delay
;; 				 )
;; 			   (message "planner-mode-hook in %S" (current-buffer))))
;; 	       )

;; 	     (define-key planner-mode-map "\C-c\C-d" 'planner-delete-task)
;; 	     (define-key planner-mode-map "\r" 'planner-dwim)
;; 	     (add-hook 'versor-dynamic-menu-add-items-at-top-hook
;; 		       (lambda (menu)
;; 			 (when (eq major-mode 'planner-mode)
;; 			   (versor-add-menu-item menu "Start/stop task" 'planner-dwim))))

;; 	     )

;; ;;; end of use-planner.el
