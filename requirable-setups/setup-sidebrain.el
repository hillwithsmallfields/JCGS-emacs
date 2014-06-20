;;;; setup-sidebrain.el -- setup for my use of sidebrain
;;; Time-stamp: <2013-10-15 12:02:47 johnstu>

(message "Loading sidebrain setup")

(add-lispdir "$OPEN_PROJECTS/sidebrain/lisp/")

(provide 'setup-sidebrain)
(require 'sidebrain)
(require 'filenames-in-env)
(require 'sidebrain-planner)

(setq sidebrain-file-name (substitute-in-file-name "$COMMON/var/sidebrain.xml")
      sidebrain-history-file-name (substitute-in-file-name "$COMMON/var/sidebrain-history.xml")
      )

(defun sidebrain-buffer-persistence-hook-function ()
  (lambda ()
    (insert "   (sidebrain-use-default-project t)\n")))

(add-hook 'buffer-persistence-variables-hook
	  ' sidebrain-buffer-persistence-hook-function)

(if (if t
	(on-voice-system)
      (string= (downcase (system-name)) "hosea"))
    ;; if on a voice system (at least on my home one), do not set up
    ;; the pop-up frame, because if it happens to be active when the
    ;; voice system initialises, it becomes the only window for which
    ;; voice is valid
    (progn
      (setq sidebrain-popup-frame nil)
      (when nil
	(add-hook 'vr-mode-startup-hook (lambda ()
					  (setq sidebrain-popup-frame t)
					  (sidebrain-display))))
      )
  (progn
    (setq sidebrain-popup-frame t)))

(defvar screen-setup-sidebrain-frame-font-choices
  '(
    "normal"				; comment out when on large screen
    "tiny" "compact")
  "The font names, in my naming scheme from screen-setup.el, to use for the sidebrain frame.
The first one of these found to exist is used.")

(add-to-list 'sidebrain-frame-parameters
	     (cons 'font
		   (second (get-screen-setup
			    screen-setup-sidebrain-frame-font-choices))))

(add-hook 'sidebrain-filename-save-hooks 'unsubstitute-in-file-name)
(add-hook 'sidebrain-filename-load-hooks 'substitute-in-file-name)

(setq sidebrain-file-projects
      '(("\\.doc" nil nil)
	("\\.log" nil nil)
	("sidebrain.*\\.el" "research programming" "sidebrain")
	("sidebrain/papers" "research writing" "sidebrain")
	("mulvoc.*\\.el" "fun programming" "mulvo")
	("projects/focloir" "riomhaireolaíocht Gaeilge" "focloir")
	("cs4145" "teaching" "cs4145")
	("proglang" "research programming" "programming languages")
	("papers/.+\\.tex" "research writing" "other papers")
	("papers/.+\\.bib" "research writing" "other papers")
	("teaching/lectures" "teaching" "lectures")
	("teaching/exams" "teaching" "exams")
	("csis-www/research-elisp" "research programming" "miscellaneous")
	("csis-www" "research writing" "work web site")
	("downloaded" nil nil)
	("cospa" "eu" "cospa")
	("fyps" nil nil)
	("open-projects/emacs-versor" "research programming" "versor")
	("open-projects/sidebrain" "research programming" "sidebrain")
	("open-projects/WikiRoads" "fun programming" "wikiroads")
	;; ("projects/w5" "fun programming" "web map")
	("gather/code" "research programming" "data hoarding and mining")
	("research/.+\\.tex" "research writing" "miscellaneous")
	("ULDSPACE" "eu" "cospa-kb")
	("emacs/" "programming" "emacs")
	("www" "personal writing" "web site")
	("hat-trick" "personal writing" "hat trick")
	("writing" "personal writing" "other writing")
	("/home/dspace" nil nil)
	("/usr/" nil nil)
	("/etc/" nil nil)
	("\\.html" nil nil)))

(defvar sidebrain-type-break-string "Typing break"
  "*String to say what we are doing, during typing breaks.")

(defvar task-before-typing-break nil
  "The task our typing break interrupted.")

(defun sidebrain-start-type-break ()
  "Record that we are starting a typing break."
  (sidebrain-save-to-file)
  (let* ((top-task (car (sidebrain-task-stack)))
	 (got-task (sidebrain-task-p top-task))
	 (top-task-text (and got-task
			     (sidebrain-task-text top-task))))
    ;; (message "Starting typing break; top-task=%S top-task-text=%S" top-task top-task-text)
    (unless (string= top-task-text sidebrain-type-break-string)
      (when got-task
	(setf (sidebrain-task-file top-task) buffer-file-name
	      (sidebrain-task-line-number top-task) (count-lines (point-min) (point))))
      (setq task-before-typing-break
	    (sidebrain-set-task-triplet (list "special" "fixed" sidebrain-type-break-string)))
      (message "Task before typing break was %S" task-before-typing-break))))

(defun sidebrain-end-type-break ()
  "Record that we are finishing a typing break."
  (let* ((top-task (car (sidebrain-task-stack)))
	 (top-task-text (and (sidebrain-task-p top-task)
			     (sidebrain-task-text top-task))))
    (message "comparing %S and %S to see if type break is on task stack" top-task-text sidebrain-type-break-string)
    (when (equal top-task-text sidebrain-type-break-string)
      (message "Typing break is on task stack, suspending it")
      ;; (sidebrain-suspend-task 'no-edit)
      ;; (sidebrain-resume-task )
      (sidebrain-set-task-triplet task-before-typing-break)      
      )
    ;; (message "Reloading file in case other machine has changed it")
    ;;     (sidebrain-load-from-file)
    ;;     (message "Trying day-structure activities")
    ;; (unless (day-structure-update-current-activity)
    ;;       (message "Did not update activity, looking at reinstating old task, which was %S" task-before-typing-break)
    ;;       (unless (null task-before-typing-break)
    ;; 	(message "Reverting to task-before-typing-break=%S" task-before-typing-break)
    ;; 	(sidebrain-set-task-triplet task-before-typing-break)))
    ))

(add-hook 'type-break-start-break-hook 'sidebrain-start-type-break)
(add-hook 'type-break-end-break-hook 'sidebrain-end-type-break)

(sidebrain-load-from-file)
;; (sidebrain-display) ; doing this here seems to upset the voice setup, perhaps something to do with frame titles?

(define-key global-map [menu-bar sidebrain] '(menu-item "Sidebrain" sidebrain-menu))

;;; end of sidebrain-setup.el
