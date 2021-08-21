;;; config-org-mode.el --- set up JCGS' org mode
;;; Time-stamp: <2021-08-21 17:38:08 jcgs>


(require 'org)

(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs"))
(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs/information-management"))
(add-to-list 'load-path (substitute-in-file-name "$ORGLISP"))
(let ((dir "/usr/share/emacs/site-lisp/emacs-goodies-el")) ;for htmlize
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))
(add-to-list 'load-path (expand-file-name "information-management" user-emacs-directory))

(add-to-list 'org-modules 'org-agenda)
(add-to-list 'org-modules 'org-timer)
(add-to-list 'org-modules 'org-clock)
(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-ql)

(let ((omd (substitute-in-file-name "$EHOME/Dropbox/MobileOrg")))
  (when (file-directory-p omd)
    (add-to-list 'org-modules 'org-mobile)
    (setq
     org-mobile-directory omd
     org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-mobile-directory))))
(org-load-modules-maybe t)

;; so I can exchange files with non-emacs users and still have their systems pick a text editor:
(add-to-list 'auto-mode-alist (cons "\\.org\\.txt" 'org-mode))

(setq org-todo-keywords
      '((sequence "SOMETIME(s)" "TODO(t)" "BLOCKED(b)" "CURRENT(c)" "OPEN(o)"
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
      org-agenda-skip-scheduled-if-done t
      org-agenda-columns-add-appointments-to-effort-sum t
      org-agenda-default-appointment-duration 60 ; minutes
      ;; org-agenda-overriding-columns-format ; should probably set this
      org-enable-table-editor t
      org-special-ctrl-k t
      org-yank-adjusted-subtrees t
      org-directory (substitute-in-file-name "$ORG/")
      foo (message "org dir is %s" org-directory)
      org-default-notes-file (expand-file-name "new.org" org-directory)
      org-archive-location (substitute-in-file-name "$ORG/archive/%s::")
      org-agenda-files (append (mapcar (function
					(lambda (file)
					  (expand-file-name (format "%s.org" file) org-directory)))
				       '(;; "habits"
					 "general"
					 "shopping"
					 ;; "eating"
					 ;; "research"
					 ;; "work"
					 "projects"
					 "learning"
					 "improvement"
					 "goals"))
			       (list (substitute-in-file-name "$VEHICLES/Marmalade/Marmalade-work.org")))
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
      org-edit-timestamp-down-means-later t
      org-timer-default-timer 25
      org-clock-idle-time 26
      org-enforce-todo-dependencies t
      org-agenda-dim-blocked-tasks t
      org-enforce-todo-checkbox-dependencies t
      org-todo-state-tags-triggers '((done ("soon" . nil) ("urgent" . nil) ("today" . nil)))
      org-M-RET-may-split-line nil
      )

(defvar jcgs/org-ssid-tag-alist
  '(("BTHomeHub2-8GHW" . "@home")
    ;; todo: add one for @office
    ("Makespace" . "@Makespace")
    )
  "Alist mapping wireless networks to tags.")

(require 'metoffice)

(defvar weather-loadable (and (file-readable-p metoffice-config-file)
			      (condition-case weather-problem
				  (load-file metoffice-config-file)
				(error (message "Could not load weather config")
				       nil)))
  "Whether we have a chance of getting the weather data.")

(require 'calendar)

(defun jcgs/org-agenda-make-early-extra-matcher ()
  "Make some extra matcher types for my custom agenda, to go early in the list."
  (let ((result nil))
    (when (member (calendar-day-of-week
		   (calendar-gregorian-from-absolute (org-today)))
		  org-agenda-weekend-days)
      (push '(tags-todo "weekend") result))
    (let ((wifi-command "/sbin/iwgetid"))
      (when (file-executable-p wifi-command)
	(let* ((network (car (split-string
			      (shell-command-to-string
			       (concat wifi-command " --raw")))))
	       (tag (cdr (assoc network jcgs/org-ssid-tag-alist))))
	  (when (stringp tag)
	    (push `(tags-todo ,tag) result)))))
    result))

(defun jcgs/org-agenda-make-late-extra-matcher (early-matches)
  "Make some extra matcher types for my custom agenda, to go late in the list.
EARLY-MATCHES shows what we've already found to go earlier in the list."
  (let ((todo-home '(tags-todo "@home"))
	(result nil))
    (when (string-match "isaiah" (system-name))
      (push todo-home result))
    (when (and weather-loadable (or (member todo-home result)
				    (member todo-home early-matches))) ; could be there because of hostname, or ssid
      (condition-case weather-problem
	  (let* ((day-weather (metoffice-get-site-period-weather nil 0 'day))
		 (temperature (metoffice-weather-aspect day-weather 'feels-like-day-maximum-temperature))
		 (rain (metoffice-weather-aspect day-weather 'precipitation-probability-day))
		 (wind (metoffice-weather-aspect day-weather 'wind-speed)))
	    (when (and (>= temperature 10)
		       (<= rain 6)
		       (<= wind 6))
	      (push '(tags-todo "outdoor|@garden") result)))
	(error (message "Problem %s in adding weather-dependent items" weather-problem))))))

(defvar jcgs/org-agenda-store-directory (or (getenv "WWW_AGENDA_DIR")
					    "/tmp")
  "Where to store agenda views.")

(defun jcgs/org-make-custom-agenda-file-names (description)
  "Make a saved agenda file list for DESCRIPTION."
  (let ((name-base (subst-char-in-string ?  ?_ (downcase description) t)))
    (mapcar (lambda (extension)
	      (expand-file-name (concat name-base extension)
				jcgs/org-agenda-store-directory))
	    '(""
              ".html"
              ".org"
              ;; ".ps"
              )
            )))

(setq jcgs/org-agenda-current-matcher
      (let* ((earlies (jcgs/org-agenda-make-early-extra-matcher))
	     (lates (jcgs/org-agenda-make-late-extra-matcher earlies)))
	`("c" "Agenda and upcoming tasks"
	  ((tags-todo "urgent")
	   (tags-todo "PRIORITY=\"A\"")
	   (tags-todo "today")
	   (tags-todo "soon/OPEN")
	   (tags-todo "soon/TODO")
	   ,@earlies
	   (agenda "")
	   (tags-todo "next")
	   ,@lates
	   (tags-todo "PRIORITY=\"B\"")
	   )
	  nil
	  ,(jcgs/org-make-custom-agenda-file-names "current"))))

(add-to-list 'org-agenda-custom-commands jcgs/org-agenda-current-matcher)

(defun jcgs/def-org-agenda-custom-command (description key type &optional match)
  "Define a custom agenda command with DESCRIPTION, KEY, TYPE, MATCH.
See `org-agenda-custom-commands' for what these mean.
The filenames to save in are added by this function"
  (org-add-agenda-custom-command
   (list key description type (or match "") nil
	 (jcgs/org-make-custom-agenda-file-names description))))

(jcgs/def-org-agenda-custom-command "mackaYs shopping" "y" 'tags-todo "Mackays")
(jcgs/def-org-agenda-custom-command "supermarKet shopping" "k" 'tags-todo "supermarket")
(jcgs/def-org-agenda-custom-command "Daily Bread" "d" 'tags-todo "daily_bread")
(jcgs/def-org-agenda-custom-command "Online" "o" 'tags-todo "online")
(jcgs/def-org-agenda-custom-command "Ordered" "O" '((todo "ORDERED")))
(jcgs/def-org-agenda-custom-command "At home" "h" 'tags-todo "@home")
(jcgs/def-org-agenda-custom-command "Hacking" "H" '((tags-todo "hacking")
							  (tags-todo "programming")
							  (tags-todo "@Makespace")
							  (tags-todo "soldering")
							  (tags-todo "woodwork")
							  (tags-todo "sewing")
							  (tags-todo "epoxy")
							  (tags-todo "hotglue")
							  (tags-todo "electronics")))
(jcgs/def-org-agenda-custom-command "Writing" "W" 'tags-todo "writing")
(jcgs/def-org-agenda-custom-command "At work" "w" 'tags-todo "@office")
(jcgs/def-org-agenda-custom-command "weekEnd" "E" 'tags-todo "weekend")
(jcgs/def-org-agenda-custom-command "Urgent" "u" '((tags-todo "PRIORITY=\"A\"")
						   (tags-todo "urgent")))
(jcgs/def-org-agenda-custom-command "Soon" "s" 'tags-todo "soon")
(jcgs/def-org-agenda-custom-command "B Priority" "B" 'tags-todo "PRIORITY=\"B\"")
(jcgs/def-org-agenda-custom-command "Phone" "p" 'tags-todo "phone")
(jcgs/def-org-agenda-custom-command "maKespace" "K" 'tags-todo "@Makespace")
(jcgs/def-org-agenda-custom-command "Next" "x" 'tags-todo "next")

(when (and (boundp 'work-agenda-file)
	   (stringp work-agenda-file)
	   (file-exists-p work-agenda-file))
  (setq org-capture-templates (cons '("w" "Work todo" entry
				      (file+headline work-agenda-file "Incoming"
						     "** TODO"))
				    org-capture-templates)))

(global-set-key "\C-cn" 'org-capture)

(defun org-tags-view-todo-only ()
  "Call `org-tags-view' with a prefix."
  (interactive)
  (org-tags-view t))

(global-set-key "\C-cm" 'org-tags-view-todo-only)

;; todo: make at least some of these into autoloads
;; (require 'work-tasks)
(require 'org-jcgs-journal)
(require 'org-upwards-propagation)
(add-hook 'org-clock-in-prepare-hook 'jcgs/org-propagate-openness-upward)
(add-hook 'org-after-todo-state-change-hook 'jcgs/org-propagate-doneness-upwards t)
(require 'org-moving-tags)
(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-move-next-marker)
(add-hook 'org-after-todo-state-change-hook 'jcgs/org-maybe-chain-task)
(require 'org-linked-tasks)
(require 'org-task-colours)
(add-hook 'org-clock-in-hook 'jcgs/org-nice-appearance)
(add-hook 'org-clock-out-hook 'jcgs/org-dull-appearance)
(jcgs/org-dull-appearance)
(require 'org-pomodoros)
(require 'org-jira)
(require 'org-log-tasks)
(require 'org-agenda-notify)
(require 'org-agenda-count)
(require 'org-agenda-server)
(require 'org-mi3)
(require 'org-mouse-extras)
(add-hook 'org-mode-hook 'jcgs-org-mouse-stuff)
(require 'org-timestamps)
(add-hook 'org-after-todo-state-change-hook 'jcgs/org-update-timestamp)
(require 'org-parcels)

(when (and (boundp 'work-agenda-file)
	   (stringp work-agenda-file)
	   (file-readable-p work-agenda-file)
	   (not (member work-agenda-file org-agenda-files)))
  (push work-agenda-file org-agenda-files))

(let ((myself-org (substitute-in-file-name "$EHOME/myself/org")))
  (when (file-directory-p myself-org)
    (setq org-agenda-files (append org-agenda-files
				   (directory-files myself-org
						    t
						    ".org$")))))

(defvar jcgs/shell-mode-accumulated-command-history-file
  (substitute-in-file-name "$ORG/shell-command-history.org")
  "My accumulated command history.")

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
(define-key org-mode-map "\C-c\M-l" 'org-toggle-link-display)
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
		       "/DONE|CANCELLED|BOUGHT|EATEN|KEPT|BROKEN|ALMOSTKEPT" 'file))))

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
	    (just-one-space))
	  (org-set-tags nil t))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transfer from and to mobile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (boundp 'org-mobile-directory)
	   (stringp org-mobile-directory)
	   (file-directory-p org-mobile-directory))
  (org-mobile-pull)

  (defun jcgs/org-maybe-push-to-mobile ()
    "Offer to push the agenda to mobile."
    (when (and (string-match "isaiah" (system-name))
	       (y-or-n-p "Push to mobile? "))
      (org-mobile-push))
    t)

  (add-hook
   ;; would be on kill-emacs-hook, but that's not suitable for functions
   ;; that interact with the user --- see its docstring
   'kill-emacs-query-functions
   'jcgs/org-maybe-push-to-mobile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list entries with a given tag ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-tag-list (tag)
  "Return a list of the occurrences of TAG."
  (let ((result nil))
    (org-map-entries
     (lambda ()
       (push (cons (org-get-heading t t)
		   (cons (org-get-todo-state)
			 (org-get-tags)))
	     result))
     tag			  ; maybe add "+TODO=\"TODO\"" or "/!"
     'agenda)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sync with google calendar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'org-gcal)

(let ((gcal-settings-file (substitute-in-file-name "$HOME/.org-gcal.el")))
  (when (file-readable-p gcal-settings-file)
    (load-file gcal-settings-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make exported links open in new tabs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-export-html-open-in-new-tabs (text back-end channel)
  (if (eq back-end 'html)
      (mapconcat 'identity (split-string text "<a href") "<a target=\"_blank\" href")
    text))

(eval-after-load "ox"
  '(add-hook 'org-export-filter-final-output-functions
	     'jcgs/org-export-html-open-in-new-tabs))

;;;;;;;;;;;;;;;;;;;;;;;
;; new export system ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-generate-soon-list ()
  "Generate a list of tasks to do soon."
  (interactive)
  (let* ((header "Things to do soon")
         (items (org-ql (org-agenda-files)
                  (and (or (todo "TODO") (todo "OPEN") (todo "CURRENT") (todo "BUY"))
                       (or (priority >= "B") (tags "soon")))
                  :action (cons (org-get-heading 'no-tags) (org-entry-get (point) "ID")))))
    (find-file "/tmp/soon.org")
    (erase-buffer)
    (dolist (item items)
      (message "Adding %S" item)
      (org-insert-heading)
      (insert (car item))
      (goto-char (point-max))
      (when (cdr item)
        (org-set-property "ID" (cdr item))))
    (basic-save-buffer)))

;;;;;;;;;;;;
;; org-ql ;;
;;;;;;;;;;;;

(defun jcgs/org-ql-defview (name &rest definition)
  "Define a view calleed NAME with &DEFINITION."
  (map-put org-ql-views (if (symbolp name)
                            (symbol-name name)
                          name)
           definition #'equal)
  (customize-set-variable 'org-ql-views org-ql-views)
  (customize-mark-to-save 'org-ql-views))

(when (boundp 'org-ql-views)
  (jcgs/org-ql-defview "Current"
                       :title "Current"
                       :sort 'todo
                       :super-groups '((:auto-parent))
                       ;; :groups '(tags)
                       :buffers-files 'org-agenda-files
                       :query '(or (and
                                    (not (done))
                                    (tags "soon"))
                                   (habit)
                                   (deadline auto)
                                   (scheduled :to today)
                                   (ts-active :on today))
                       :sort '(todo priority date))
  (jcgs/org-ql-defview "Supermarket"
                       :title "Supermarket"
                       :buffers-files (mapcar 'substitute-in-file-name
                                              '("$COMMON/org/shopping.org"))
                       :query '(and (tags "supermarket")
                                    (todo "BUY")))
  (jcgs/org-ql-defview "Online"
                       :title "Online"
                       :buffers-files (mapcar 'substitute-in-file-name
                                              '("$COMMON/org/shopping.org"))
                       :query '(and (tags "online")
                                    (todo "BUY")))
  (jcgs/org-ql-defview "Physical making"
                       :title "Physical making"
                       :buffers-files (mapcar 'substitute-in-file-name
                                              '("$COMMON/org/projects.org"))
                       :query '(and (tags "physical_making")
                                    (or (todo "TODO")
                                        (todo "OPEN")))
                       :sort '(todo priority date))
  (jcgs/org-ql-defview "Programming"
                       :title "Programming"
                       :buffers-files (mapcar 'substitute-in-file-name
                                              '("$COMMON/org/projects.org"))
                       :query '(and (tags "programming")
                                    (or (todo "TODO")
                                        (todo "OPEN")))
                       :sort '(todo priority date))
  (jcgs/org-ql-defview "Mending"
                       :title "Mending"
                       :buffers-files (mapcar 'substitute-in-file-name
                                              '("$COMMON/org/projects.org"
                                                "$COMMON/org/general.org"
                                                "$COMMON/vehicles/Marmalade/Marmalade-work.org"))
                       :query '(and (or (tags "physical_fixing")
                                        (tags "mending"))
                                    (or (todo "TODO")
                                        (todo "OPEN")))))

;; (defun jcgs/org-super-agenda-defgroup (name &rest definition)
;;   "Define a super-agenda group NAME with DEFINITION."
;;   (setq org-super-agenda-groups
;;         (map-put org-super-agenda-groups
;;                  name
;;                  definition
;;                  (lambda (a b) (message "a %s b %s" a b) (eql a b)))))

(setq org-super-agenda-groups
      '((:name "Important"
                :priority "A")
        (:name "In post"
               :todo "ORDERED")))

;;; config-org-mode.el ends here
