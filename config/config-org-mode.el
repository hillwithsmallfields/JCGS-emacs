;;; config-org-mode.el --- set up JCGS' org mode
;;; Time-stamp: <2015-03-14 19:54:53 jcgs>

(require 'org)

(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs"))

(add-to-list 'org-modules 'org-timer)
(add-to-list 'org-modules 'org-clock)
(add-to-list 'org-modules 'org-drill)
(add-to-list 'org-modules 'org-mobile)

(org-load-modules-maybe t)

(message "At start of config-org-mode.el, org-agenda-files is %S" org-agenda-files)

(add-to-list 'load-path (expand-file-name "information-management" user-emacs-directory))
(require 'work-tasks)
(require 'work-log)
(add-to-list 'auto-mode-alist (cons "\\.org\\.txt" 'org-mode))

(message "Near start of config-org-mode.el, org-agenda-files is %S" org-agenda-files)

(autoload 'post-sync-catchup "post-sync-catchup"
  "Reload files that might have been changed by syncing with my phone.
This analyzes the script for the command run by
`post-sync-catchup-sync-command' to find what might have
changed." t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "CURRENT(c)" "OPEN(o)"
		  "|"
		  "DONE(d)" "CANCELLED(x)"))
      org-clock-in-switch-to-state "CURRENT"
      org-use-fast-todo-selection nil
      org-log-done 'time
      org-log-into-drawer t
      org-agenda-include-diary t
      org-agenda-compact-blocks t
      org-agenda-start-with-follow-mode t
      org-agenda-start-with-clockreport-mode t
      org-agenda-start-on-weekday 0
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-default-appointment-duration 60 ; minutes
      ;; org-agenda-overriding-columns-format ; should probably set this
      org-directory (substitute-in-file-name "$ORG/")
      foo (message "org dir is %s" org-directory)
      org-default-notes-file (expand-file-name "new.org" org-directory)
      org-archive-location (substitute-in-file-name "$ORG/archive/%s::")
      org-agenda-files (append (mapcar (function
					(lambda (file)
					  (expand-file-name (format "%s.org" file) org-directory)))
				       '("general" "shopping" "eating" "research" "work"
					 "projects" "learning" "improvement"))
			       (list (substitute-in-file-name "$VEHICLES/Marmalade/Marmalade-work.org")
				     (substitute-in-file-name "$VEHICLES/Marmalade/170.org"))
			       (mapcar (function
					(lambda (file)
					  (expand-file-name (format "Cat-Imp-%s.org" file)
							    (substitute-in-file-name "$DROPBOX/Categorical_Imperative"))))
				       '("work" "purchasing")))
      bar (message "agenda files %s" org-agenda-files)
      org-capture-templates '(("p" "Personal todo" entry
			       (file+headline
				(substitute-in-file-name "$ORG/general.org")
				"Incoming"
				"** TODO"))
			      ("b" "Buy" entry (file+headline
						(substitute-in-file-name "$ORG/shopping.org")
						"Incoming"
						"** BUY")))
      org-refile-use-outline-path 'full-file-path
      org-outline-path-complete-in-steps t
      org-timer-default-timer 25
      org-clock-idle-time 26
      org-enforce-todo-dependencies t
      org-agenda-dim-blocked-tasks t
      org-enforce-todo-checkbox-dependencies t
      org-M-RET-may-split-line nil
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-mobile-directory)
      )

(defun jcgs/org-agenda-make-extra-matcher ()
  "Make some extra matcher types for my custom agenda."
  (let ((result nil))
    (when (member (calendar-day-of-week
	 (calendar-gregorian-from-absolute (org-today)))
	org-agenda-weekend-days)
      (push '(tags-todo "weekend") result))
    ;; todo: if I can get the location... add @home, @garden, @work, @Makespace accordingly
    ;;       This probably wants a package of its own!
    ;; todo: if I can get the weather... add dryday accordingly
    ;;       try http://www.metoffice.gov.uk/datapoint
    ;;           http://www.metoffice.gov.uk/datapoint/product/uk-daily-site-specific-forecast/detailed-documentation
    ;;           http://www.metoffice.gov.uk/datapoint/support/documentation/uk-locations-site-list-detailed-documentation
    ;;       This probably wants a package of its own!
    result))

(setq jcgs/org-agenda-current-matcher `("c" "Agenda and upcoming tasks"
					((agenda "")
					 (tags-todo "urgent")
					 (tags-todo "soon")
					 (tags-todo "next")
					 ,@(jcgs/org-agenda-make-extra-matcher)
					 )))

(add-to-list 'org-agenda-custom-commands jcgs/org-agenda-current-matcher)

(when (and (boundp 'work-agenda-file)
	   (stringp work-agenda-file)
	   (file-exists-p work-agenda-file))
  (setq org-capture-templates (cons '("w" "Work todo" entry
				      (file+headline work-agenda-file "Incoming"
						     "** TODO"))
				    org-capture-templates)))

(defun jcgs/org-clock-in-prepare-function ()
  "My customization of task clock-in."
  (save-excursion
    (while (> (funcall outline-level) 1)
      (outline-up-heading 1)
      (when (looking-at org-complex-heading-regexp)
	(let ((state (match-string-no-properties 2)))
	  (when (equal state "TODO")
	    (org-todo "OPEN")))))))

(add-hook 'org-clock-in-prepare-hook 'jcgs/org-clock-in-prepare-function)

(defun jcgs/org-after-todo-state-change-propagate-upwards ()
  "When the last of a set of sibling tasks is marked as DONE,
mark the ancestral tasks as DONE."
  (while (> (funcall outline-level) 1)
    (outline-up-heading 1)
    (let ((not-all-done nil)
	  (on-first t))
      (org-map-entries
       (lambda ()
	 (when (and (not on-first)
		    (org-entry-is-todo-p))
	   (setq not-all-done t))
	 (setq on-first nil))
       nil
       'tree)
      (unless not-all-done
	(org-todo "DONE")))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-propagate-upwards t)

(defun jcgs/org-after-todo-state-change-move-next-marker ()
  "If this task is being marked as done, and has a :next: tag, move the tag."
  (when (and (org-entry-is-done-p)
	     (member "next" (org-get-tags)))
    (org-toggle-tag "next" 'off)
    (beginning-of-line 1)
    (let ((started-at (point)))
      (org-forward-heading-same-level 1)
      (if (/= (point) started-at)
	  (org-toggle-tag "next" 'on)
	(when (y-or-n-p "Move :next: marker to next subtree? ")
	  (outline-next-heading)
	  (org-toggle-tag "next" 'on))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-move-next-marker)

;; TODO: make this stop my pomodoro timer
;; TODO: probably it should be in the pomodoro code rather than this hook
;; (defun jcgs/org-clock-out-function ()
;;   "My customization of task clock-out."
;;   (org-timer-stop))

;; (add-hook 'org-clock-out-hook 'jcgs/org-clock-out-function)

(global-set-key "\C-cn" 'org-capture)

(defun org-tags-view-todo-only ()
  "Call `org-tags-view' with a prefix."
  (interactive)
  (org-tags-view t))

(global-set-key "\C-cm" 'org-tags-view-todo-only)

(defvar jcgs/org-clocking-out-for-type-break nil
  "Whether the current clocking-out is because of a typing break.
Should be nil unless bound in a typing break hook function.")

(defun jcgs/org-timer-setup ()
  "Customizer the org timer to suit me, for pomodoro use."
  (add-hook 'org-clock-in-hook
	    (function
	     (lambda ()
	       (unless org-timer-current-timer
		 (org-timer-set-timer '(16)))
	       (org-todo "CURRENT"))))
  (add-hook 'org-clock-out-hook
	    (function
	     (lambda ()
	       (when org-timer-current-timer
		 (org-timer-cancel-timer))
	       (org-timer-stop)
	       (org-todo (if jcgs/org-clocking-out-for-type-break
			     "OPEN"
			   (if (y-or-n-p "Finished task? ")
			       "DONE"
			     "OPEN")))))))

;;;; JIRA links

(defvar jcgs/org-jira-task-format "http://jira.arm.com/browse/EMUF-%s"
  "Format of links for jira tasks.
The task identifier is substituted in as a string.")

(defun jcgs/org-follow-jira-link (task)
  "Show jira TASK in a browser."
  (interactive "sTask: ")
  (browse-url (format jcgs/org-jira-task-format task)))

(org-add-link-type "jira" 'jcgs/org-follow-jira-link)

;;;; Switch colour themes

;;; Use nicer ones when I'm clocked in to some task, to encourage me
;;; to be clocked in more of the time.

;; (add-lispdir "$GATHERED/emacs/color-theme")

;; (require 'color-theme)

(load-file (substitute-in-file-name "$GATHERED/emacs/color-theme/color-theme.el"))

(defvar jcgs/org-task-color-themes
  [color-theme-wheat			; quite nice
   color-theme-whateveryouwant 		; a bit odd
   ;; color-theme-katester ; too pastel
   ;; color-theme-vellum ; todo: change the pale blue bit
   ;; color-theme-mistyday ; doesn't cancel old dark theme well enough
   ;; color-theme-pierson
   color-theme-robin-hood		; the dark green one
   color-theme-high-contrast
   ;; color-theme-emacs-nw
   ;; color-theme-scintilla
   ]
  "Colour themes I prefer.")

(defvar jcgs/org-no-task-color-themes
  [;; color-theme-euphoria ; doesn't change correctly from some
   ;; color-theme-calm-forest
   color-theme-blue-mood
   ;; color-theme-billw
   color-theme-jonadabian		; dark blue, I think it may leave traces afterwards
   ;; color-theme-lethe
   color-theme-kingsajz
   color-theme-retro-orange
   color-theme-retro-green
   color-theme-resolve
   ]
  "Colour themes I can endure but don't like much.")

(add-hook 'org-clock-in-hook
	  (function
	   (lambda ()
	     (condition-case problem
		 (let ((theme (aref jcgs/org-task-color-themes
				    (random (length jcgs/org-task-color-themes)))))
		   (message "Using %s as clocked-in theme" theme)
		   (funcall theme))
	       (message "Got error %S in changing colour theme" problem)))))

(add-hook 'org-clock-out-hook
	  (function
	   (lambda ()
	     (condition-case problem
		 (let ((theme (aref jcgs/org-no-task-color-themes
				    (random (length jcgs/org-no-task-color-themes)))))
		   (message "Using %s as clocked-out theme" theme)
		   (funcall theme))
	       (message "Got error %S in changing colour theme" problem)))))

;;;; Pomodoros

(defvar jcgs/org-timer-pomodoros-done-count 0
  "Count of the pomodoros I have done.
Reset to zero whenever `jcgs/org-timer-log-pomodoro-done' decides to log
the end of a day.")

(defvar jcgs/pomodoro-log-file
  (cond
   ((file-directory-p "/work/johstu01/work-org")
    (expand-file-name "pomodoro-log.org" "/work/johstu01/work-org/"))
   ((getenv "ORG")
    (substitute-in-file-name "$ORG/pomodoro-log.org"))
   ((file-directory-p "~/Dropbox")
    (expand-file-name "~/Dropbox/pomodoro-log.org"))
   (t (expand-file-name "~/pomodoro-log.org")))
  "Where I log my pomodoro completion.")

(defun jcgs/pomodoro-log-show ()
  "Show my pomodoro log."
  (interactive)
  (find-file-other-window jcgs/pomodoro-log-file)
  (goto-char (point-max )))

(defun jcgs/org-timer-last-pomodoro-completion-day-in-file (file)
  "Find the last completion day in FILE."
  (if (file-readable-p file)
      (save-window-excursion
	(save-excursion
	  (find-file file)
	  (save-excursion
	    (goto-char (point-max))
	    (if (re-search-backward "\\*\\*\\* Date \\([_0-9]+\\)" (point-min) t)
		(match-string-no-properties 1)
	      nil))))
    nil))

(defvar jcgs/org-timer-last-pomodoro-completion-day
  (jcgs/org-timer-last-pomodoro-completion-day-in-file
   jcgs/pomodoro-log-file)
  "The day I last completed a pomodoro.
Used to reset the counter daily.")

(defvar jcgs/org-timer-pomodoros-done-log nil
  "Log of the pomodoros I have done.")

(defun jcgs/date-string (&optional time)
  "Decode date into a format I use.
Optional argument TIME is passed on."
  (format-time-string "%Y_%m_%d"))

(defvar jcgs/org-strip-timer-stuff-regexp
  "\\(.+\\)\\(: time out\\)"
  "Regexp to identify what part of a string from the timer system to keep.")

(defun jcgs/org-strip-timer-stuff (string)
  "Remove timing system related text from STRING."
  (if (string-match jcgs/org-strip-timer-stuff-regexp
		    string)
      (match-string 1 string)
    string))

(defun jcgs/org-timer-log-pomodoro-done (string)
  "Log that I have completed a timed activity slot.
Argument STRING is the log entry."
  (let* ((now (current-time))
	 (day (jcgs/date-string now))
	 (pomodoro-string (jcgs/org-strip-timer-stuff string)))
    (save-window-excursion
      (save-excursion
	(find-file jcgs/pomodoro-log-file)
	(goto-char (point-max))
	(message "Recording pomodoro: day now %S, last completion day %S" day jcgs/org-timer-last-pomodoro-completion-day)
	(when (not (equal day jcgs/org-timer-last-pomodoro-completion-day))
	  (message "%d pomodoros in previous day"
		   jcgs/org-timer-pomodoros-done-count)
	  (save-excursion
	    ;; this should have been inserted when a pomodoro was done
	    ;; on the previous day when any pomodoros were done:
	    (when (search-backward
		   (format "*** Date %s"
			   jcgs/org-timer-last-pomodoro-completion-day)
		   (point-min) t)
	      (end-of-line)
	      (insert (format " (%d pomodoros done)"
			      jcgs/org-timer-pomodoros-done-count))))
	  (setq jcgs/org-timer-pomodoros-done-count 0)
	  (jcgs/org-open-hierarchical-date day))
	(insert "**** " pomodoro-string "\n")
	(basic-save-buffer)
	(setq
	 jcgs/org-timer-last-pomodoro-completion-day day
	 jcgs/org-timer-pomodoros-done-log (cons (cons (current-time-string)
						       pomodoro-string)
						 jcgs/org-timer-pomodoros-done-log)
	 ;; todo: put this in the mode line
	 jcgs/org-timer-pomodoros-done-count (1+
					      jcgs/org-timer-pomodoros-done-count))))))

;;;; Write clocked-in tasks into my work log file

(defun jcgs/org-find-ancestral-jira-task ()
  "Find the jira task covering the current task."
  (save-excursion
    (let* ((pattern "\\[jira:\\([0-9]+\\)\\]")
	   (heading (nth 4 (org-heading-components))))
      (if (string-match pattern heading)
	  (match-string-no-properties 1 heading)
	(catch 'found
	  (while (> (funcall outline-level) 1)
	    (outline-up-heading 1)
	    (setq heading (nth 4 (org-heading-components)))
	    (when (string-match pattern heading)
	      (throw 'found (match-string-no-properties 1 heading))))
	  nil)))))

(defvar jcgs/org-last-clocked-task-added-to-log nil
  "The last clocked task added to the log.")

(defvar jcgs/org-last-clocked-task-date-added-to-log nil
  "The date of the last clocked task added to the log.")

(defun jcgs/org-add-clocked-task-to-log ()
  "Add to your log the task you're currently clocking in to.
For use in `org-clock-in-hook'."
  (let* ((task (nth 4 (org-heading-components)))
	 (jira (if (string-match "\\[jira:[0-9]+\\]" task)
		   nil
		 (jcgs/org-find-ancestral-jira-task)))
	 (date (format-time-string "%Y-%m-%d")))
    (when (or (not (equal task jcgs/org-last-clocked-task-added-to-log))
	      (not (equal date jcgs/org-last-clocked-task-date-added-to-log)))
      (save-window-excursion
	(find-file work-log-file)
	(goto-char (point-max))
	;; todo: regulate this to one blank line
	(insert (format "\n\n**** Clocked in to \"%s%s\"\n" task
			(if jira
			    (format " (jira %s)" jira)
			  "")))
	(setq jcgs/org-last-clocked-task-added-to-log task
	      jcgs/org-last-clocked-task-date-added-to-log date)
	(basic-save-buffer)))))

(add-hook 'org-clock-in-hook 'jcgs/org-add-clocked-task-to-log) 

;;;; Timer notification

(defvar jcgs/background-images-directory (substitute-in-file-name "$HOME/backgrounds")
  "My directory of background images.")

(defun jcgs/random-background-image ()
  "Pick a background image at random."
  (let ((images (directory-files jcgs/background-images-directory t "\\.jpg" t)))
    (nth (random (length images)) images)))

(defun jcgs/org-timer-notifier (notification)
  "Display NOTIFICATION in an arresting manner."
  (let ((overrun nil))
    (jcgs/org-timer-log-pomodoro-done notification)
    (require 'notify-via-browse-url)
    (notify-via-browse-url
     (format "<STYLE type=\"text/css\"> BODY { background: url(\"file:%s\") } </STYLE>"
	     (jcgs/random-background-image))
     (format "Pomodoro completed at %s" (current-time-string))
     notification)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create "*Org timer notification*"))
      (erase-buffer)
      (insert notification (substitute-command-keys "\n\n\\[exit-recursive-edit] to continue\n\n"))
      (let ((start-of-overrun-keystrokes (point)))
	(recursive-edit)
	;; avoid wasting any keystrokes the user was typing at the time
	(when (> (point-max)
		 start-of-overrun-keystrokes)
	  (setq overrun  (buffer-substring start-of-overrun-keystrokes
					   (point-max)))
	  (kill-new overrun))))
    (message "%s saved in kill ring" overrun)))

(setq org-show-notification-handler 'jcgs/org-timer-notifier)

(eval-after-load "org"
  '(jcgs/org-timer-setup))

(defvar jcgs/org-dull-theme 'tango
  "The theme to use for making things look dull.
This is for use when not clocked into any task, to remind or
encourage me to clock in as much as possible.")

(defvar jcgs/org-nice-theme nil
  "The theme to use for making things look dull.
This is for use when not clocked into any task, to remind or
encourage me to clock in as much as possible.
If nil, it just reverts to the default appearance.")

(defun jcgs/org-dull-appearance ()
  "Make Emacs look dull.
This is for use when not clocked into any task, to remind or
encourage me to clock in as much as possible."
  (if (memq jcgs/org-dull-theme custom-enabled-themes)
      (enable-theme jcgs/org-dull-theme)
    (when (and jcgs/org-nice-theme
	       (memq jcgs/org-nice-theme custom-enabled-themes))
      (disable-theme jcgs/org-nice-theme))
    (load-theme jcgs/org-dull-theme)))

(defun jcgs/org-nice-appearance ()
  "Make Emacs look nice.
This is for use when clocked into a task, to remind or encourage
me to clock in as much as possible."
  (when (memq jcgs/org-dull-theme custom-enabled-themes)
    (disable-theme jcgs/org-dull-theme))
  (when jcgs/org-nice-theme
    (if (memq jcgs/org-nice-theme custom-enabled-themes)
	(enable-theme jcgs/org-nice-theme)
      (load-theme jcgs/org-nice-theme))))

(defun jcgs/org-clock-in-or-out ()
  "Clock in (if out) or out (if in)."
  (interactive)
  (if (org-clocking-p)
      (progn
	(org-clock-out)
	;; todo: use org-clock-out-hook instead
	(jcgs/org-dull-appearance))
    (org-clock-in)
    ;; todo: use org-clock-in-hook instead
    (jcgs/org-nice-appearance)))

(define-key org-mode-map [ f5 ] 'jcgs/org-clock-in-or-out)

(defun jcgs/org-clock-out-on-typing-break-function ()
  "Clock out of the current task, as a typing break is starting."
  (message "jcgs/org-clock-out-on-typing-break-function: %S %S %S"
	   org-clock-goto-may-find-recent-task
	   (car org-clock-history)
	   (if (car org-clock-history)
	       (marker-buffer (car org-clock-history))
	     "<none>"))
  (when (and org-clock-goto-may-find-recent-task
	     (car org-clock-history)
	     (marker-buffer (car org-clock-history)))
    (let ((jcgs/org-clocking-out-for-type-break t))
      (org-clock-goto)
      (message "clocking out at %d in %S" (point) (current-buffer))
      (org-clock-out))
    ;; we did not actually do the typing break:
    nil))

(defvar jcgs/type-break-start-break-hook nil
  "Hooks for starting a typing break.
You may want to turn voice input off at this point; and suspend task timers.")

(eval-after-load "type-break"
  '(progn
     (defadvice type-break (before jcgs/type-break-hook-runner activate)
       (run-hooks 'jcgs/type-break-start-break-hook))
     (add-hook 'jcgs/type-break-start-break-hook 'jcgs/org-clock-out-on-typing-break-function)))

(defun jcgs/org-open-hierarchical-date (date)
  "Ensure there is a hierarchical record for DATE."
  ;; TODO: I think there is an existing library I could use for this
  ;; TODO: make this check whether the date is already there
  ;; TODO: make it find the right place in a file even if DATE is not the last date
  ;; TODO: make it use a common date format for the argument
  (goto-char (point-max))
  (beginning-of-line 1)
  (when (looking-at "^\\s-+$")
    (delete-region (point) (point-max)))
  (unless (save-excursion
	    (beginning-of-line 0)
	    (looking-at "^$"))
    (insert "\n"))
  (unless (re-search-backward (concat "* Year " (substring date 0 -6) "$") (point-min) t)
    (goto-char (point-max))
    (insert "* Year " (substring date 0 -6) "\n"))
  (goto-char (point-max))
  (unless (re-search-backward (concat "** Month " (substring date 0 -3) "$") (point-min) t)
    (goto-char (point-max))
    (insert "** Month " (substring date 0 -3) "\n"))
  (goto-char (point-max))
  (unless (re-search-backward (concat "*** Date " date "$") (point-min) t)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert "*** Date " date "\n\n")))

(when (and (boundp 'work-agenda-file)
	   (stringp work-agenda-file)
	   (file-readable-p work-agenda-file)
	   (not (member work-agenda-file org-agenda-files)))
  (push work-agenda-file org-agenda-files))

(add-to-list 'load-path (expand-file-name "information-management/" user-emacs-directory))
(require 'org-mouse-extras)
(add-hook 'org-mode-hook 'jcgs-org-mouse-stuff)

(defun planner-to-org ()
  "Convert text from planner format to org format."
  (interactive)
  (goto-char (point-min)) (replace-regexp "^#[ABC] " "")
  (goto-char (point-min)) (replace-regexp " {{Tasks:[0-9]+}}" "")
  (goto-char (point-min)) (replace-regexp " (\\[\\[[0-9.]+\\]\\])$" "")
  (goto-char (point-min)) (replace-string "pos:" "file:")
  (goto-char (point-min)) (replace-regexp "#[0-9]+" "")
  (goto-char (point-min)) (replace-regexp "^o" "CURRENT")
  (goto-char (point-min)) (replace-regexp "^_" "TODO")
  (goto-char (point-min)) (replace-regexp "^X" "DONE")
  (goto-char (point-min)) (replace-regexp "^" "** "))

(defvar source-file-names-pattern "\\(.el\\|\\.c\\|\\.h\\)$"
  "Pattern describing the files to record.")

(defun make-source-org-tree (dir &optional prefix)
  "Make an org tree for DIR, at level PREFIX."
  (interactive "DDirectory: ")
  (if (null prefix) (setq prefix "*"))
  (let ((pending nil))
    (dolist (file (directory-files dir t))
      (if (and (file-directory-p file)
	       (not (string-match "\\.$" file)))
	  (push file pending)
	(if (string-match source-file-names-pattern file)
	    (insert prefix " TODO read " (file-name-nondirectory file) "\n"))))
    (dolist (pended (nreverse pending))
      (insert prefix " " (file-name-nondirectory pended) "\n")
      (make-source-org-tree pended (concat "*" prefix)))))

(define-key org-mode-map "\C-z" 'org-todo)
(define-key org-mode-map "\C-cw" 'org-agenda-list)
(global-set-key "\C-ca" 'org-agenda)
(define-key org-agenda-mode-map "\C-z" 'org-agenda-todo)

(defun org-global-close-property-drawers ()
  "Close all property drawers in this buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-property-start-re (point-max) t)
      (org-flag-drawer t))))

;;;; Patch the table export to use vertical rules:

(defvar org-export-latex-table-column-separator "|"
  "String to put between columns in exported tables.")

(defvar org-export-latex-table-left-margin "|"
  "String to put before the first column in exported tables.")

(defvar org-export-latex-table-right-margin "|"
  "String to put after the last column in exported tables.")

(setq org-export-latex-tables-column-borders t)

(eval-after-load "org-export-latex"
  '(defun org-export-latex-tables (insert)
     "Convert tables to LaTeX and INSERT it.
This is John Sturdy's modified version."
     (goto-char (point-min))
     (while (re-search-forward "^\\([ \t]*\\)|" nil t)
       ;; FIXME really need to save-excursion?
       (save-excursion (org-table-align))
       (let* ((beg (org-table-begin))
	      (end (org-table-end))
	      (raw-table (buffer-substring beg end))
	      fnum fields line lines olines gr colgropen line-fmt align
	      caption label attr floatp longtblp)
	 (if org-export-latex-tables-verbatim
	     (let* ((tbl (concat "\\begin{verbatim}\n" raw-table
				 "\\end{verbatim}\n")))
	       (apply 'delete-region (list beg end))
	       (insert (org-export-latex-protect-string tbl)))
	   (progn
	     (setq caption (org-find-text-property-in-string
			    'org-caption raw-table)
		   attr (org-find-text-property-in-string
			 'org-attributes raw-table)
		   label (org-find-text-property-in-string
			  'org-label raw-table)
		   longtblp (and attr (stringp attr)
				 (string-match "\\<longtable\\>" attr))
		   align (and attr (stringp attr)
			      (string-match "\\<align=\\([^ \t\n\r,]+\\)" attr)
			      (match-string 1 attr))
		   floatp (or caption label))
	     (setq lines (split-string raw-table "\n" t))
	     (apply 'delete-region (list beg end))
	     (when org-export-table-remove-special-lines
	       (setq lines (org-table-clean-before-export lines 'maybe-quoted)))
	     ;; make a formatting string to reflect aligment
	     (setq olines lines)
	     (while (and (not line-fmt) (setq line (pop olines)))
	       (unless (string-match "^[ \t]*|-" line)
		 (setq fields (org-split-string line "[ \t]*|[ \t]*"))
		 (setq fnum (make-vector (length fields) 0))
		 (setq line-fmt
		       (concat
			org-export-latex-table-left-margin
			(mapconcat
			 (lambda (x)
			   (setq gr (pop org-table-colgroup-info))
			   (format "%s%%s%s"
				   (cond ((eq gr ':start)
					  (prog1 (if colgropen "|" "")
					    (setq colgropen t)))
					 ((eq gr ':startend)
					  (prog1 (if colgropen "|" "|")
					    (setq colgropen nil)))
					 (t ""))
				   (if (memq gr '(:end :startend))
				       (progn (setq colgropen nil) "|")
				     "")))
			 fnum org-export-latex-table-column-separator)
			org-export-latex-table-right-margin))))
	     ;; fix double || in line-fmt
	     (message "line-fmt raw = %S" line-fmt)
	     (setq line-fmt (replace-regexp-in-string "||" "|" line-fmt))
	     ;; maybe remove the first and last "|"
	     (when (and (not org-export-latex-tables-column-borders)
			(string-match "^\\(|\\)?\\(.+\\)|$" line-fmt))
	       (message "line-fmt chomped = %S" line-fmt)
	       (setq line-fmt (match-string 2 line-fmt)))
	     (message "line-fmt now = %S" line-fmt)
	     ;; format alignment
	     (unless align
	       (setq align (apply 'format
				  (cons line-fmt
					(mapcar (lambda (x) (if x "r" "l"))
						org-table-last-alignment)))))
	     ;; prepare the table to send to orgtbl-to-latex
	     (setq lines
		   (mapcar
		    (lambda(elem)
		      (or (and (string-match "[ \t]*|-+" elem) 'hline)
			  (split-string (org-trim elem) "|" t)))
		    lines))
	     (when insert
	       (insert (org-export-latex-protect-string
			(concat
			 (if longtblp
			     (concat "\\begin{longtable}{" align "}\n")
			   (if floatp "\\begin{table}[htb]\n"))
			 (if (or floatp longtblp)
			     (format
			      "\\caption{%s%s}"
			      (if label (concat "\\\label{" label "}") "")
			      (or caption "")))
			 (if longtblp "\\\\\n" "\n")
			 (if (not longtblp) "\\begin{center}\n")
			 (if (not longtblp) (concat "\\begin{tabular}{" align "}\n"))
			 (orgtbl-to-latex
			  lines
			  `(:tstart nil :tend nil
				    :hlend ,(if longtblp
						(format "\\\\
\\hline
\\endhead
\\hline\\multicolumn{%d}{r}{Continued on next page}\\
\\endfoot
\\endlastfoot" (length org-table-last-alignment))
					      nil)))
			 (if (not longtblp) (concat "\n\\end{tabular}"))
			 (if longtblp "\n" "\n\\end{center}\n")
			 (if longtblp
			     "\\end{longtable}"
			   (if floatp "\\end{table}"))))
		       "\n\n"))))))))

;;;; archive all individual DONE tasks:

(defun jcgs/org-archive-done-tasks-buffer ()
  "Archive all DONE entries in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-heading-regexp (point-max) t)
      (beginning-of-line 1)
      (org-map-entries (function
			(lambda ()
			  (let ((started-at (point)))
			    (org-archive-subtree)
			    (setq org-map-continue-from started-at))))
		       "/DONE|CANCELLED|BOUGHT" 'file))))

(defun jcgs/org-archive-done-tasks-file (file)
  "Archive all DONE entries in FILE."
  (interactive "fArchive tasks in file: ")
  (find-file file)
  (jcgs/org-archive-done-tasks-buffer))

(defun jcgs/org-archive-done-tasks ()
  "Archive all DONE entries in variable `org-agenda-files'."
  (interactive)
  (save-window-excursion
    (mapcar 'jcgs/org-archive-done-tasks-file
	    (org-agenda-files))))

;;;; sort entries by stage

(defun jcgs/org-todo-sort-entries-by-stage ()
  "Sort entries by their stage of progress."
  (interactive)
  (org-sort-entries nil ?f
		    'jcgs/todo-keyword-sort-key
		    '<))

(defun jcgs/org-todo-keyword-sort-key ()
  "Return the sort key of the current entry.
For use with `org-sort-entries'."
  (save-excursion
    (when (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
    (if (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
	    (looking-at "\\(?: *\\|[ \t]*$\\)"))
	(position (match-string-no-properties 1) org-todo-keywords-1 :test 'equal)
      999)))

;;; change task dates

(defun jcgs/org-task-today (&optional no-move offset)
  "Mark the task on the current line as to be done today.
Unless optional NO-MOVE, move to the next entry.
With optional OFFSET, add that number of days."
  (interactive "P")
  (let ((today-string (format-time-string "<%Y-%m-%d %a>"
					  (if offset
					      (time-add (current-time)
							(days-to-time offset))
					    nil)))
	(eol (line-end-position)))
    (save-excursion
      (beginning-of-line)
      ;; todo: probably some org-mode functions for positions and changes in the line
      (if (re-search-forward "<[0-9]+-[0-9]+-[0-9]+ [a-z]+>" eol t)
	  (replace-match today-string)
	(let* ((tag-start (save-excursion
			    (and (re-search-forward "[:@a-z0-9_]+:$" eol t)
				 (match-beginning 0))))
	       (text-end (and tag-start
			      (save-excursion
				(goto-char tag-start)
				(skip-syntax-backward "s")
				(point)))))
	  (goto-char (or text-end eol))
	  (just-one-space)
	  (insert today-string)
	  (unless (eolp)
	    (just-one-space)))))
    ;; todo: probably some org-mode or outline-mode command for this
    (forward-line)))

(defun jcgs/org-task-tomorrow (&optional extra-days)
  "Mark the task on the current line as to be done tomorrow.
Then move to the next entry.
An argument can change the number of days ahead, 1 being tomorrow."
  (interactive "p")
  (jcgs/org-task-today nil extra-days))

(define-key org-mode-map [ f8 ] 'jcgs/org-task-today)
(define-key org-mode-map [ f9 ] 'jcgs/org-task-tomorrow)

(defun jcgs/org-agenda-task-today (&optional no-move)
  "Like jcgs/org-task-today, but from the agenda buffer.
Unless optional NO-MOVE, move to the next entry."
  (interactive "P")
  (save-window-excursion
    (other-window 1)
    (jcgs/org-task-today t))
  (unless no-move
    (org-agenda-next-line)))

(defun jcgs/org-agenda-task-tomorrow (&optional extra-days)
  "Like jcgs/org-task-tomorrow, but from the agenda buffer.
Then move to the next entry.
An argument can change the number of days ahead, 1 being tomorrow."
  (interactive "p")
  (save-window-excursion
    (other-window 1)
    (jcgs/org-task-tomorrow extra-days))
  (org-agenda-next-line))

(define-key org-agenda-mode-map [ f8 ] 'jcgs/org-agenda-task-today)
(define-key org-agenda-mode-map [ f9 ] 'jcgs/org-agenda-task-tomorrow)

;;;;;;;;;;;;;;;;;;;;;;;;
;; separate log files ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-variable-buffer-local 'tracking-org-file)

;; (defun jcgs-select-work-log ()
;;   "This function is meant to go on `find-file-hook'."
;;   (cond
;;    ((string-match (substitute-in-file-name "$COMMON/Marmalade") default-directory)
;;     (setq tracking-org-file (substitute-in-file-name "$COMMON/Marmalade/Marmalade-work.log")))))

;; (add-hook 'find-file-hook 'jcgs-select-work-log)

;;; some debugging

(message "At end of config-org-mode.el, org-agenda-files is %S" org-agenda-files)

;;; config-org-mode.el ends here
