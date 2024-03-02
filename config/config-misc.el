;;;; config-misc.el -- small setup stuff
;;; Time-stamp: <2024-01-29 21:47:58 jcgs>

(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs/"))

(defun use-utf-8 ()
  "Set the coding system of the current buffer to UTF-8."
  (interactive)
  (set-buffer-file-coding-system 'utf-8))

(add-to-list 'load-path
	     (expand-file-name "elisp-dev-tools" user-emacs-directory))
(require 'misc-elisp-tools)

(add-to-list 'load-path
	     (expand-file-name "editing" user-emacs-directory))
(require 'skip-initial-comments)

(defun kill-matching-buffers-no-ask (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP.
The optional second argument INTERNAL-TOO indicates whether to kill
internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ? ))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defun write-region-and-find-it (start end file)
  "Write region from START to END into FILE and find it in a buffer."
  (interactive "r
FWrite region to file: ")
  (if (or (not (file-exists-p file))
	  (yes-or-no-p (format "File %s already exists -- overwrite? " file)))
      (progn
	(write-region start end file)
	(find-file file))))

;;;; Genetic Programming setup

(add-to-list 'load-path
	     (substitute-in-file-name "$OPEN_PROJECTS/libRTL/src/"))

(autoload 'rtl-mode "rtl-mode"
  "Major mode for editing RTL files."
  t)

(add-to-list 'auto-mode-alist
	     (cons "\\.rtl$" 'rtl-mode))

(add-to-list 'load-path
	     (substitute-in-file-name "$OPEN_PROJECTS/GrEvo/trunk/utilsrc/"))

(autoload 'grevo-trace-mode "grevo-trace-mode"
    "Mode for looking at GrEvo traces."
    t)

(add-to-list 'auto-mode-alist
	     (cons "\\.trace$" 'grevo-trace-mode))

(add-to-list 'auto-mode-alist
	     (cons "\\.result$" 'grevo-trace-mode))

(defun jcgs-grevo-trace-mode-setup ()
  "Set up grevo mode for me."
  (define-key
    grevo-mode-parent-number-keymap
    [ Trigger-up ]
    'grevo-jump-individual))

(add-hook 'grevo-trace-mode-hook 'jcgs-grevo-trace-mode-setup)

;; temporary for renaming things

(defun rename-throughout (here)
  "Rename the symbol starting HERE wherever it appears."
  (interactive "d")
  (let* ((there (save-excursion
		  (forward-sexp 1)
		  (point)))
	 (old-text (buffer-substring-no-properties here there)))
    (tags-query-replace old-text (concat "grevo_" old-text))))

;;;; Screen appearance

(add-to-list 'load-path
	     (expand-file-name "appearance" user-emacs-directory))

(require 'screen-setups)

;;;; lua-mode

(add-to-list 'auto-mode-alist
	     (cons "\\.lua$" 'lua-mode))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(add-hook 'lua-mode-hook 'turn-on-font-lock)

(add-hook 'lua-mode-hook 'hs-minor-mode)

;;;; haskell-mode

(add-to-list 'load-path
	     (substitute-in-file-name "$GATHERED/emacs/haskell/haskell-mode-2.1"))

(autoload 'haskell-mode "haskell-mode" "Haskell editing mode." t)

(add-to-list 'auto-mode-alist
	     (cons "\\.hs" 'haskell-mode))

;;;; source code annotation

(message "about to load annotation")

(add-to-list 'load-path
	     (expand-file-name "information-management" user-emacs-directory))

(autoload 'annotation-open "annotation"
  "Open an annotation for PROJECT FILE DEFUN."
  t)

(autoload 'annotation-decorate-file "annotation"
  "Find the annotations for the current file, and display them."
  t)

(message "loaded annotation")

;; (add-hook 'c-mode-hook 'annotation-decorate-file)
;; (add-hook 'emacs-lisp-mode-hook 'annotation-decorate-file)

;;;; ratpoison setup

(defvar frame-for-work-log nil
  "A frame for displaying my work log.")

(defvar frame-for-work-org nil
  "A frame for displaying my work org.")

(defvar frame-for-work-erc nil
  "A frame for displaying my work erc.")

(defun get-frame (name)
  "Find the frame named NAME."
  (catch 'done
    (dolist (frame (frame-list))
      (when (equal (frame-parameter frame 'name) name)
	(throw 'done frame)))
    nil))

(defun ratpoison-usual ()
  "Set up my usual ratpoison arrangement."
  (interactive)
  (require 'ratpoison-cmd)
  (delete-other-windows)
  (jcgs/frame-setup)
  (add-to-list 'load-path (expand-file-name "appearance" user-emacs-directory))
  (require 'split-window-multi)
  (split-to-80-columns)
  (when (> (length (split-string (shell-command-to-string "ratpoison -c sdump") ",")) 1)
    (message "setting up ratpoison frames")
    (ratpoison-sfrestore
     (mapconcat 'identity '("(frame :number 0 :x 0 :y 0 :width 1920 :height 1200 :screenw 1920 :screenh 1200 :window 14680228 :last-access 334 :dedicated 0) 0"
			    "(frame :number 1 :x 0 :y 0 :width 1400 :height 1200 :screenw 1920 :screenh 1200 :window 16783739 :last-access 335 :dedicated 0) 1"
			    "(frame :number 2 :x 1400 :y 0 :width 520 :height 430 :screenw 1920 :screenh 1200 :window 16777385 :last-access 311 :dedicated 0) 1"
			    "(frame :number 3 :x 1400 :y 430 :width 520 :height 380 :screenw 1920 :screenh 1200 :window 16777710 :last-access 314 :dedicated 0) 1"
			    "(frame :number 4 :x 1400 :y 810 :width 520 :height 390 :screenw 1920 :screenh 1200 :window 20971618 :last-access 315 :dedicated 0) 1"
			    "")
		","))
    ;; (let ((frame-name (file-name-nondirectory
    ;;     	       (file-name-sans-extension (car jcgs/org-journal-files)))))
    ;;   (message "setting up journalling frame")
    ;;   (ratpoison-fselect 1)
    ;;   (find-file (car jcgs/org-journal-files))
    ;;   (setq frame-for-work-log
    ;;         (or (get-frame frame-name)
    ;;     	(make-frame-on-display ":0.1"
    ;;     			       '((title . "Work log")
    ;;     				 (name . "Work log")))))
    ;;   (display-buffer (find-buffer-visiting (car jcgs/org-journal-files))
    ;;     	      nil
    ;;     	      frame-for-work-log))

    ;; (when (and (boundp 'work-agenda-file)
    ;;            (stringp work-agenda-file)
    ;;            (file-exists-p work-agenda-file))
    ;;   (let ((frame-name (file-name-nondirectory
    ;;     		 (file-name-sans-extension work-agenda-file))))
    ;;     (message "setting up work.org frame")
    ;;     (ratpoison-fselect 2)
    ;;     (find-file work-agenda-file)
    ;;     (setq frame-for-work-org
    ;;           (or (get-frame frame-name)
    ;;     	  (make-frame-on-display ":0.1"
    ;;     				 '((title . "Tasks")
    ;;     				   (name . "Work agenda")))))
    ;;     (display-buffer (find-buffer-visiting work-agenda-file)
    ;;     		nil
    ;;     		frame-for-work-org)))
    
    (message "setting up erc frame")
    (ratpoison-fselect 3)
    (setq frame-for-work-erc
	  (or (get-frame "#xc")
	      (make-frame-on-display ":0.1"
				     '((title . "xc")
				       (name . "#xc")))))
    (erc :server "irc-int.xci-test.com"
	 :port 1494
	 :nick "jcgs"
	 :password "aVRu3naK"
	 :full-name user-full-name)
    (ratpoison-fselect 0)))

;; These are the within-frame equivalents of the ratpoison commands
;; that I've put onto C-f1 and M-f1 for going between frames and
;; windows:
(global-set-key [ C-f2 ] 'other-window)
(global-set-key [ M-f2 ] 'next-buffer)

(message "loaded ratpoison")

;;;; compile with snapshotting

;; (add-to-list 'load-path (expand-file-name "external-programs" user-emacs-directory))
;; (require 'tracked-compile)
;; (require 'laptop-tests)

;; (message "loaded tracked-compile")

(setq pxe-handle "jcgs")

;;;; grep my buffers

(defun grep-buffers (search-pattern files-pattern)
  "Grep the files I have loaded into emacs.
Argument SEARCH-PATTERN is what to search for.
Argument FILES-PATTERN is which files to search."
  (interactive "sGrep for:
sGrep in files matching: ")
  (let ((files nil))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer)))
	(when (and file (string-match files-pattern file))
	  (push file files))))
    (message "Files are %d:%S" (length files) files)
    (shell-command (format "grep -H %s %s" search-pattern (mapconcat 'identity files " ")))))

;;;; look at web pages for a set time

(defun distraction-minutes-osm-mapping (minutes)
  "Do some mapping for MINUTES."
  (interactive "nMinutes of mapping: ")
  (web-page-for-time "http://www.osm.org/"
		     (format "%d mins" minutes)
		     "Stop mapping now"))

(defun distraction-minutes-bbc (minutes)
  "Do some bbc news browsing for MINUTES."
  (interactive "nMinutes of news browsing: ")
  (web-page-for-time "http://www.bbc.co.uk/news/"
		     (format "%d mins" minutes)
		     "Stop reading the news now"))

(defun distraction-minutes-el-reg (minutes)
  "Do some The Register news browsing for MINUTES."
  (interactive "nMinutes of El Reg browsing: ")
  (web-page-for-time "http://www.theregister.co.uk/"
		     (format "%d mins" minutes)
		     "Stop reading El Reg now"))

;;;; web search without a browser

(add-to-list 'load-path (expand-file-name "webstuff/" user-emacs-directory))

(autoload 'gnugol "gnugol"
  "Search the web via gnugol, bring up results in org buffer."
  t)

;;;; re-read buffer that have been changed externally, e.g. by copying in from another machine

(add-to-list 'load-path (expand-file-name "file-handling" user-emacs-directory))

(autoload 'up-to-date-file "buffer-file-sync"
  "Make sure that BUFFER has the same timestamp its file on disk.
If it doesn't, offer to revert it from the disk version.
Optional argument NO-ASK says not to ask, but revert anyway." t)

(autoload 'up-to-date-all-buffers "buffer-file-sync"
  "Make sure that all file buffers have the same times as their files on disk.
If they doesn't, offer to revert them from the disk version.
Optional argument NO-ASK says not to ask, but revert anyway." t)

;;;; find the source related to an error message

(defun find-last-error-source ()
  "Find the source corresponding to the last error."
  (interactive)
  (if (re-search-backward "File \"\\([^\"]+\\)\", line \\([0-9]+\\)," (point-min) t)
      (let ((file (match-string-no-properties 1))
	    (line (string-to-number (match-string-no-properties 2))))
	(find-file file)
	(goto-line line))
    (error "Could not find an error line")))

(global-set-key "\C-cs" 'find-last-error-source)

;;;; insert a comment based on the occurrences of a symbol

(defvar grep-marker "grep result: "
  "A marker for grep result lines.")

(defun insert-grep ()
  "Insert the result of grepping for the current tag."
  (interactive)
  (let ((comment-string (concat comment-start grep-marker
				(mapconcat 'identity
					   (split-string
					    (shell-command-to-string
					     (format "grep %s *.%s"
						     (grep-tag-default)
						     (file-name-extension (buffer-file-name))))
					    "\n")
					   (concat comment-end
						   "\n"
						   comment-start grep-marker))
				"\n")))
    (save-excursion
      (beginning-of-defun)
      (insert comment-string))))

(defun delete-grep ()
  "Delete the results of `insert-grep'."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((end (point))
	  (pattern (concat
		    (or comment-start-skip (concat comment-start "\\s-*"))
		    grep-marker)))
      (beginning-of-line 0)
      (while (looking-at pattern)
	(beginning-of-line 0))
      (beginning-of-line 2)
      (delete-region (point) end))))

;;;; find the buffer visiting the nearest file of a given name

(add-to-list 'load-path (substitute-in-file-name "$MY_ELISP/convenience"))

(autoload 'switch-to-nearest-file-buffer "nearest-file"
  "Switch to the buffer visiting the nearest file called NAME.
First, try all the buffers visiting files of that name in subdirectories
of `default-directory', and if none is found there, try working back up
the directory tree." t)

;;;; openscad setup

(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs/openscad/"))
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))
(autoload 'scad-mode "scad-mode"   "Major mode for editing OpenSCAD code.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `scad-mode-hook'.

Key bindings:
\\{scad-mode-map}" t)

;;;; Run a shell command on the filename at point

(defvar shell-command-on-file-at-point-history nil
  "History commands read by `shell-command-on-file-at-point'.")

(defun shell-command-on-file-at-point (filename command &optional background)
  "On FILENAME, run COMMAND, optionally in the BACKGROUND.
FILENAME is taken from the text around point, and COMMAND is prompted for."
  (interactive
   (let* ((filename-from-text (thing-at-point 'filename))
	  (filename (read-from-minibuffer "Run command on file: " filename-from-text))
	  (suggested-command (save-excursion
			       (goto-char (beginning-of-thing 'filename))
			       (backward-word 1)
			       (thing-at-point 'word)))
	  (command (read-from-minibuffer (format "Command to run on %s: "
						 filename)
					 (if (string= (shell-command-to-string
						       (concat "which " suggested-command))
						      "")
					     nil
					   suggested-command)
					 nil
					 nil
					 'shell-command-on-file-at-point-history))
	  (background (string-match "\\s-*&$" command)))
     (list filename (if background
			(substring command 0 background)
		      command)
	   background)))
  (message "Running %s on %s%s" command filename (if background " in the background" ""))
  (shell-command (concat command " " filename (if background " &" ""))))

;; (global-set-key "\\C-M-!" 'shell-command-on-file-at-point)

;;;; fasting times

(defun fasting-table (start-hour start-mins days)
  "Create a table of how far into a fast I am.
Argument START-HOUR is the starting hour.
Argument START-MINS is the starting minute.
Argument DAYS is the number of days to fast for."
  (interactive "nStarting hour: \nnStarting minute: \nnNumber of days: ")
  (with-output-to-temp-buffer "*fast*"
    (let ((hour-now (nth 2 (decode-time))))
      (dotimes (i 24)
	(princ (format "%02d:%02d %s%s\n"
		       (mod (+ i start-hour) 24) start-mins
		       (mapconcat (lambda (h) (format "%3d" h))
				  (reverse (let ((ds nil))
					     (dotimes (j days)
					       (push (+ i (* j 24)) ds))
					     ds))
				  " ")
		       (if (= (mod (+ i start-hour) 24) hour-now)
			   " <=="
			 "")))))))

;;;; find file from name in buffer, with line number

(defun fflap ()
  "Find file and line at point."
  (interactive)
  (let* ((limit (line-end-position))
	 (line-string (or (if (save-excursion
				(re-search-forward "[^:]+:\\([0-9]+\\)" limit t))
			      (match-string 1)
			    nil)
			  (if (save-excursion
				(re-search-forward "line \\([0-9]+\\)" limit t))
			      (match-string 1)
			    nil)))
	 (line-number (if (stringp line-string)
			  (string-to-number line-string)
			nil)))
    (find-file-at-point)
    (when line-number
      (goto-line line-number))))

;;;; git things

(defun git-this-version (whereat)
  "Checkout the version around WHEREAT (in a git log)."
  (interactive "d")
  (save-excursion
    (goto-char whereat)
    (if (re-search-backward "^commit \\([0-9a-f]+\\)$" (point-min) t)
	(let ((id (match-string-no-properties 1)))
	  (message "Checking out commit %s in %s" id default-directory)
	  (shell-command (concat "git checkout " id)))
      (error "Could not find commit"))))

(add-to-list 'auto-mode-alist '("\\.gitdiffs" . diff-mode))

(setenv "PAGER" "cat")

;;;; ledger

(let ((ledger-emacs-directory (substitute-in-file-name "$OPEN_PROJECTS/ledger-mode")))
  (when (file-directory-p ledger-emacs-directory)
    (add-to-list 'load-path ledger-emacs-directory)
    (autoload 'ledger-mode "ledger-mode"
      "A mode for editing ledger data files."
      t)))

;;;; adjust frame width to match window

(defun adjust-frame-width-to-window ()
  "Adjust the width of the frame to suit the current window.
The currently visible text is used to determine the width."
  (interactive)
  (goto-char (window-start))
  (let ((end (window-end))
	(widest 0))
    (while (and (<= (point) end)
		(not (eobp)))
      (end-of-line 1)
      (let ((x (current-column)))
	(when (> x widest)
	  (setq widest x)))
      (forward-line 1))
    (set-frame-width nil widest)))

;;;; groovy-mode

(add-to-list 'auto-mode-alist (cons "Jenkinsfile" 'groovy-mode))

;;;; run-and-display

(autoload 'run-and-display "run-and-display"
  "Run a shell command and display indirect results; optionally EDIT-OPTIONS.
The standard output of the command is shown, and is searched for
filenames matching `rad-files-regexp'.  Those files are visited
and the buffers are displayed, in view mode if
`rad-use-view-mode' is set, with highlighting from
`rad-highlight-regexps-alist'.  These settings are per-buffer,
and this command prompts to set their values the first time it is
run in a buffer, or when called with a prefix argument."
  t)

(set-default 'rad-kill-previous-buffers t)

;;;; graphviz

(defvar default-tab-width 8
  "No longer defined.")

(autoload 'graphviz-dot-mode "graphics/graphviz-dot-mode.el"
  "Major mode for the dot language."
  t)

(add-to-list 'auto-mode-alist (cons "\\.gv" 'graphviz-dot-mode))

(setq graphviz-dot-view-command "dotview"
      graphviz-dot-auto-indent-on-semi nil)

;;;; csv-mode (now forked)

;; needs information-management directory to be on path

(autoload 'csv-mode "csv-mode"
  "Major mode for editing files of comma-separated value type."
  t)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(defun jcgs/csv-mode-hook ()
  "My setup for CSV mode."
  (csv-field-index-mode -1)
  (auto-fill-mode -1))

(add-hook 'csv-mode-hook 'jcgs/csv-mode-hook)

;;;; daily standup, for work

(when (at-work-p)
  (require 'standup-report))

;;;; Compensate for small fonts

(defun jcgs/set-default-font ()
  "Set the default font.
Done because on these high-resolution screens, Emacs comes up with something very small."
  (interactive)
  (set-frame-font
   ;; "-b&h-lucidatypewriter-medium-r-normal-sans-20-*-75-75-m-*-iso10646-1"
   ;; "-b&h-lucidatypewriter-medium-r-normal-sans-18-*-75-75-m-110-iso10646-1"
   ;; "-b&h-lucidatypewriter-medium-r-normal-sans-12-*-75-75-m-70-iso10646-1"
    "-b&h-lucidatypewriter-medium-r-normal-sans-24-*-75-75-m-140-iso10646-1"
    nil nil)
  (set-frame-parameter nil 'fullscreen nil)
  (sit-for 1)
  (set-frame-parameter nil 'fullscreen 'maximized)
  )

;;;; Load finances entry

(let ((fin-entry-file (substitute-in-file-name
                       "$MY_PROJECTS/qs/financial/finances-entry.el")))
  (when (and (or t (at-home-p))
             (file-exists-p fin-entry-file))
    (load-file fin-entry-file)))

;;;; used by old color-theme code

(defun plist-to-alist (p)
  (if p
      (cons (cons (caar p) (cadr p))
	    (plist-to-alist (cddr p)))
    nil))

;;;; Enter a pound sign etc

(defun insert-pound-sign ()
  "Insert a pound sign."
  (interactive)
  (insert "£"))

(defun insert-euro-sign ()
  "Insert a euro sign."
  (interactive)
  (insert "€"))

;;; end of config-misc.el
