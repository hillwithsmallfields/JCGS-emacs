;;; tracked-compile.el --- run compilations with tracking

;; Copyright (C) 2011, 2012, 2013  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Experimental package for doing code experiments, in the XenClient (BB/OE) environment

;;; Code:

(require 'annotation)

;;;;;;;;;;;;;;;
;; Recording ;;
;;;;;;;;;;;;;;;

(defvar tracking-org-file work-log-file
  "The tracking org file for this buffer.
May be a central one for everything, or you can use separate files for
different projects.")

(defvar tracked-compile-latest-build nil
  "The name of the latest build.")

(add-to-list 'auto-mode-alist (cons (file-name-nondirectory tracking-org-file) 'tracked-compile-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; Date-based filing ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun tracking-open-date (date)
  "Ensure there is an open tracking record for DATE."
  (interactive
   (list
    (read-from-minibuffer "Date (YYYY_MM_DD): "
			  (format-time-string "%Y_%m_%d"))))
  (find-file tracking-org-file)
  ;; we must be in something based on org-mode for some org-mode
  ;; functions we use to work; we mustn't call the mode setup
  ;; function each time, because it kills all local variables
  (if (fboundp 'tracked-compile-mode)
      (unless (eq major-mode 'tracked-compile-mode)
	(tracked-compile-mode))
    (unless (eq major-mode 'org-mode)
      (org-mode)))
  (jcgs/org-open-hierarchical-date date))

(defun tracking-record (date name &optional command motive changes)
  "Record that on DATE we did a build which we call NAME.
Optional COMMAND says what `compile-command' is being used.
Optional MOTIVE is entered by the user.
Optional CHANGES says what the changes are."
  (if (null tracking-org-file)
      (message "No tracking org file for this buffer")
    (let ((bbd bb-bb-directory)		; take local copy of buffer-local
					; variable as we will change buffer
	  (origin-file (or (buffer-file-name) default-directory))
	  (project-description (annotation-project-description)))
      (save-excursion
	(tracking-open-date date)
	(when command
	  (setq compile-command command))
	(goto-char (point-max))
	(insert "**** Build " name "\n     ")
	(when project-description
	  (insert "In " project-description "\n     "))
	(when command
	  (message "Setting recorded command to %S" command)
	  (org-set-property "compile-command" command)
	  (org-set-property "compile-file" origin-file))
	(when (stringp motive)
	  (org-set-property "motive" motive))
	(when changes
	  (org-set-property "changes" changes))))))

(defun tracked-compile (&optional motive)
  "Run a compilation, with tracking in the file named in `tracking-org-file'.
Optional argument MOTIVE says what the compilation is meant to acheive."
  (interactive "sReason for compilation: ")
  (let* ((tracking-date (format-time-string "%Y_%m_%d"))
	 (tracking-name (concat tracking-date (format-time-string "_%H_%M_%S"))))
    (setq tracked-compile-latest-build tracking-name)
    (let ((bb-dir (ancestor-directory-containing default-directory "bb"))
	  (git-dir (ancestor-directory-containing default-directory "git")))
      (let ((changes (funcall tracked-compile-unrecorded-changes-function
			      git-dir)))
	(tracking-record tracking-date
			 tracking-name
			 compile-command
			 motive
			 changes))
      (message "tracked-compile from buffer %s, bb-dir %s, git-dir %s"
	       (current-buffer) bb-dir git-dir)
      (when tracked-compile-ensure-initialized-snapshot-system-function
	(funcall tracked-compile-ensure-initialized-snapshot-system-function
		 git-dir))
      (funcall tracked-compile-snapshot-function
	       git-dir
	       tracking-name)
      (let* ((t-o-f tracking-org-file)
	     (compilation-buffer (compile (eval compile-command))))
	(save-excursion
	  (find-file t-o-f)
	  (mapcar (lambda (flag)
		    (org-toggle-tag flag t))
		  (tracked-compile-get-flags nil git-dir)))
	(set-buffer compilation-buffer)
	(setq tracking-org-file t-o-f) ; set local to compilation buffer
	(add-hook 'compilation-finish-functions
		  'tracked-compile-compilation-finish-function t t)))))

;;;;;;;;;;;;;;;;;;;
;; tracked tests ;;
;;;;;;;;;;;;;;;;;;;

(defun strings-matching-pattern-in-file (pattern number file)
  "Return all strings matching PATTERN capture NUMBER in the contents of FILE."
  (let ((was-visiting (find-buffer-visiting file))
	(results nil))
    (save-excursion
      (save-window-excursion
	(find-file file)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward pattern (point-max) t)
	    (push (match-string-no-properties number) results))
	  results)))))

(defvar latest-tested-laptop nil
  "The latest laptop to be used by `tracked-test'.")

(defun tracking-format-current-date ()
  "Return the current date, as a string."
  (format-time-string "%Y_%m_%d"))

(defun tracked-test (&optional laptop)
  "Prepare to record a test result, with tracking by date and file versions.
The record is made in the file named in `tracking-org-file'.
Optional argument LAPTOP says which laptop you're running the test on."
  (interactive
   (list (prompt-for-laptop "Run test on laptop: ")))
  (when laptop
    (setq latest-tested-laptop laptop))
  (shell-command (format "ssh root@%s \"echo -n > /var/log/messages; reboot -f\"" laptop))
  (let* ((tracking-date (tracking-format-current-date))
	 (tracking-name (concat tracking-date (format-time-string "_%H_%M_%S"))))
    (tracking-open-date tracking-date)
    (goto-char (point-max))
    (insert "**** Test " tracking-name "\n     ")
    (let ((raw-ls (shell-command-to-string
		   (format "ssh root@%s ls -l /usr/lib/xen/bin/qemu-dm"
			   (getenv "LAPTOPNAME")))))
      (when (string-match
	     "^[-rwx.]+.+?\\([0-9]\\{7,9\\} [a-z]\\{3\\} +[0-9]+ [0-9]+:[0-9]+\\) /usr"
	     raw-ls)
	(org-set-property "qemu-file"
			  (substring raw-ls (match-beginning 1) (match-end 1)))))
    (laptop-state-record laptop)
    (let* ((files-named (strings-matching-pattern-in-file
			 " \\([-a-z0-9:_]+/[-a-z0-9/.:_]+\\)" 1
			 (format "/home/xc_tftpboot/pxe/%s/pxelinux.cfg"
				 (getenv "LAPTOPNAME")))))
      (dolist (file files-named)
	(let* ((basename (file-name-sans-extension
			  (file-name-nondirectory file)))
	       (fullname (file-truename
			  (expand-file-name file
					    "/home/xc_tftpboot/pxe")))
	       (original (string-match "original" fullname))
	       (attributes (file-attributes fullname)))
	  (org-set-property basename (format "%d %s%s"
					     (nth 7 attributes)
					     (format-time-string
					      "%b %d %H:%M" (nth 5 attributes))
					     (if original
						 " (original)"
					       " (replacement)"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tracked shell commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tracked-recent-shell-commands nil
  "Recent shell commands.
Used while selecting a shell command to log.")

(defun tracked-read-recent-shell-command (prompt)
  "Select a recent shell command from the history.
Argument PROMPT is passed to `read-from-minibuffer'."
  (setq tracked-recent-shell-commands (ring-elements comint-input-ring))
  (when nil
    (mapcar (function
	     (lambda (str)
	       (set-text-properties 0 (length str) nil str)
	       str))
	    tracked-recent-shell-commands)
    (message "recent-commands are now %S" tracked-recent-shell-commands))
  (read-from-minibuffer prompt
			(car tracked-recent-shell-commands)
			nil		; keymap
			nil		; read
			'tracked-recent-shell-commands
			))

(defun tracked-recent-shell-command (command)
  "Record a recent shell COMMAND in your work log.
For use from the comint (shell) buffer."
  (interactive
   (list
    (tracked-read-recent-shell-command "Record shell command: ")))
  (save-window-excursion
    (save-excursion
      (tracking-open-date (tracking-format-current-date))
      (goto-char (point-max))
      (insert "\n        $ " command "\n\n"))))

(require 'shell)			; for shell-mode-map
(define-key shell-mode-map (kbd "C-<return>") 'tracked-recent-shell-command)

;;;;;;;;;;;;;;;;;;
;; laptop state ;;
;;;;;;;;;;;;;;;;;;

(defvar laptop-states nil
  "Alist of laptop names to their states.
The states are alists of component names to component states.")

(defun laptop-states-initialize ()
  "Initialize `laptop-states' from the tracking file."
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward ":laptop-\\([-a-z]+\\)--\\([^:]+\\): \\(.+\\)" (point-min) t)
      (let ((laptop (match-string-no-properties 1))
	    (prop (match-string-no-properties 2)))
	;; keep only the one nearest the end of each file, for each
	;; laptop-property combination:
	(unless (laptop-state-get laptop prop)
	  (laptop-state-put laptop
			    prop
			    (match-string-no-properties 3)))))))

(defun laptop-state-get (laptop &optional component)
  "For LAPTOP, get state COMPONENT."
  (let ((state (cdr (assoc laptop laptop-states))))
    (if (null component)
	state
      (and state
	   (cdr (assoc component state))))))

(defun laptop-state-put (laptop component value)
  "For LAPTOP, set state COMPONENT to VALUE."
  (let ((state-holder (assoc laptop laptop-states)))
    (unless state-holder
      (setq state-holder (cons laptop nil)
	    laptop-states (cons state-holder laptop-states)))
    (let ((component-holder (assoc component (cdr state-holder))))
      (if component-holder
	  (rplacd component-holder value)
	(rplacd state-holder
		(cons (cons component value)
		      (cdr state-holder)))))))

(defun laptop-state-put-alist (laptop pairs)
  "Into the state of LAPTOP write data from PAIRS."
  (dolist (pair pairs)
    (message "putting %S %S into state for %S" (car pair) (cdr pair) laptop)
    (laptop-state-put laptop (car pair) (cdr pair))))

(defun laptop-state-record (laptop)
  "Write the state of LAPTOP into the log file, as properties."
  (goto-char (point-max))
  (let ((state (laptop-state-get laptop)))
    (dolist (prop state)
      (org-set-property (format "laptop-%s--%s" laptop (car prop))
			(cdr prop)))))

;;;;;;;;;;;;;;;;;;;;;;
;; tracked commands ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar tracked-commands nil
  "Alist of commands that can be done through `tracked-command'.
Each element is a cons of:
  the command name
  a function to prompt for the arguments.")

(defun tracked-command (command &rest args)
  "Execute COMMAND with ARGS, noting it in the file in `tracking-org-file'.
Commands are defined in `tracked-commands'."
  (interactive
   (let* ((command (completing-read "Command: "
				    tracked-commands))
	  (description (assoc command tracked-commands))
	  (arg-reader (third description))
	  (args (if arg-reader
		    (funcall arg-reader command)
		  nil)))
     (cons command args)))
  (let* ((description (assoc command tracked-commands))
	 (directory (second description))
	 (command-file (if directory
			   (expand-file-name command
					     (substitute-in-file-name directory))
			 command))
	 (command-string (mapconcat
			  'identity
			  (cons command-file args)
			  " "))
	 (result-string (shell-command-to-string command-string))
	 (state-updater (fourth (assoc command tracked-commands))))
    (message "Result is %s" result-string)
    (tracking-open-date (format-time-string "%Y_%m_%d"))
    (goto-char (point-max))
    (insert "**** Command " command-string "\n     ")
    (if (> (length result-string) 0)
	(progn
	  (insert "Result:\n")
	  (let ((result-start (point)))
	    (insert result-string)
	    (indent-rigidly result-start (point) 5))
	  (insert "\n"))
      (message "No output from command %s" command-string))
    (message "state-updater is %S" state-updater)
    (when state-updater
      (apply state-updater args))))

(defun define-tracked-command (name &optional directory arg-reader state-updater)
  "Define a tracked command with NAME ARG-READER STATE-UPDATER.
Optional argument DIRECTORY is where the command is defined."
  (let ((holder (assoc name tracked-commands))
	(definition (list directory arg-reader state-updater)))
    (if holder
	(rplacd holder definition)
      (setq tracked-commands
	    (cons (cons name definition)
		  tracked-commands)))))

;;;;;;;;;;;;;;;;;;;
;; Build results ;;
;;;;;;;;;;;;;;;;;;;

(defvar tracked-compile-messages-to-tags-alist
  '(("exited abnormally" . "build_failed")
    ;; todo: find more of these
    ("finished" . "built"))
  "Alist matching compilation end messages to tags.")

(defun tracked-compile-messages-to-tags (message)
  "Find a tag for MESSAGE."
  (catch 'found
    (dolist (pattern-pair tracked-compile-messages-to-tags-alist)
      (when (string-match (car pattern-pair) message)
	(throw 'found (cdr pattern-pair))))))

(defun tracked-compile-compilation-finish-function (buffer message)
  "Function to run when compilation finishes.
Arguments BUFFER and MESSAGE are for the compilation finish function calling protocol."
  (let ((tag (tracked-compile-messages-to-tags message)))
    (when tag
      (set-buffer buffer)
      (when tracking-org-file
	;; todo: I'd like to grab the error messages here, and put them in the journal
	(save-excursion
	  (find-file-other-window tracking-org-file)
	  ;; (message "Setting overlay, tracked-compile-mode-current-build-overlay is %S" tracked-compile-mode-current-build-overlay)
	  (save-excursion
	    (goto-char (point-max))
	    (org-back-to-heading)
	    (org-toggle-tag tag t)
	    (tracked-compile-setup-overlay)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flags in source code ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tracked-compile-flagged-files nil
  "A list of the files containing flags.")

(make-variable-buffer-local 'tracked-compile-flagged-files)

(defun tracked-compile-get-flagged-files (&optional force git-directory)
  "Return a list of files with flags.
A cached value is used if available unless optional FORCE is non-nil.
Optional GIT-DIRECTORY says where to look, to save time on calculating it."
  (interactive (list t nil))
  (if (and (not force)
	   (consp tracked-compile-flagged-files)
	   (file-exists-p (car tracked-compile-flagged-files)))
      tracked-compile-flagged-files
    (unless (stringp git-directory)
      (setq git-directory (ancestor-directory-containing default-directory "git")))
    (setq tracked-compile-flagged-files
	  (mapcar (lambda (name) (expand-file-name name git-directory))
		  (nreverse
		   (cdr
		    (nreverse
		     (split-string
		      (shell-command-to-string
		       (format "cd %s; find git -name \"*.c\" -exec grep -l \"flag:\" {} \\;"
			       git-directory))
		      "\n"))))))))

(defun tracked-compile-get-flags (&optional force git-directory)
  "Return a list of flagged #if directives that are on.
A cached value for the files to look in is used if available unless
optional FORCE is non-nil.
Optional GIT-DIRECTORY says where to look, to save time on calculating it."
  (unless (or force
	      (and (stringp git-directory)
		   (file-directory-p git-directory)))
    (setq git-directory (ancestor-directory-containing default-directory "git")))
  (message "Getting flags for %S, flagged files are %S" git-directory (tracked-compile-get-flagged-files force git-directory))
  (nreverse
   (cdr
    (nreverse
     (mapcar (function (lambda (raw)
			 (if (string-match "flag:\\s-*\\([a-z_]+\\)" raw)
			     (match-string 1 raw)
			   raw)))
	     (split-string
	      (let ((command (format "cd %s; grep \"flag:\" %s | grep \"#if 1\""
				     git-directory
				     (mapconcat 'identity (tracked-compile-get-flagged-files force git-directory) " "))))
		(message "Getting flags using command: %s" command)
		(shell-command-to-string command))
	      "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Winding to specified state ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tracked-compile-get-applied-patches (&optional git-directory)
  "Return a list of the currently applied patches.
Optional GIT-DIRECTORY says where to look, to save time on calculating it."
  (unless (stringp git-directory)
    (setq git-directory (ancestor-directory-containing default-directory "git")))
   (split-string (shell-command-to-string (format "cd %s/git; guilt applied" git-directory)) "\n"))

(defun tracked-compile-patch-applied-p (patch-name &optional git-directory)
  "Return whether PATCH-NAME is currently applied.
Optional GIT-DIRECTORY says where to look, to save time on calculating it."
  (member patch-name (tracked-compile-get-applied-patches git-directory)))

(defun tracked-compile-build-around-point ()
  "Return the build name for the text around point."
  (save-excursion
    (let* ((raw-name (org-get-heading t))
	   (name (substring raw-name 6)))
      (set-text-properties 0 (length name) nil name)
      name)))

(defun tracked-compile-versions-list ()
  "Return a list of the versions described in this buffer.
Each one is consed with nil, to make it suitable for completion."
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\*\\*\\* Build \\([^ ]+\\)" (point-max) t)
	(push (match-string-no-properties 1)
	      result)))
    result))

(defun revert-buffers-visiting-files-below (directory)
  "Revert any buffers in files within DIRECTORY and its subdirectories."
  (setq directory (concat "^" (regexp-quote (file-truename directory))))
  (save-excursion
    (dolist (buffer (buffer-list))
      (let ((filename (buffer-file-name buffer)))
	(when filename
	  (setq filename (file-truename filename))
	  (when (string-match directory filename)
	    (set-buffer buffer)
	    (revert-buffer)))))))

(defun tracked-compile-rollback-to-version (version)
  "Go back to, and build, VERSION."
  (interactive
   '(list (completing-read "Rollback to version: "
		     (tracked-compile-versions-list)
		     nil
		     t)))
  (goto-char (point-min))
  (if (re-search-forward (format "^\\*\\*\\*\\* Build %s" version) (point-max) t)
      (let ((git-directory (ancestor-directory-containing
			    (file-name-directory (org-entry-get (point) "compile-file"))
			    "git"))
	    (recorded-compile-command (org-entry-get (point) "compile-command")))
	(tracked-compile-setup-overlay)
	(funcall tracked-compile-rollback-function git-directory version)
	(revert-buffers-visiting-files-below git-directory)
	(message "Compiling using retrieved command %s" recorded-compile-command)
	(compile recorded-compile-command))
    (error "Could not find version %s") version))

(defun tracked-compile-rollback-to-version-around-point ()
  "Go back to, and build, the version around point."
  (interactive)
  (tracked-compile-rollback-to-version
   (tracked-compile-build-around-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay to indicate latest build ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tracked-compile-mode-current-build-overlay nil
  "Overlay to indicate the current build.")

(make-variable-buffer-local 'tracked-compile-mode-current-build-overlay)

(defun tracked-compile-setup-overlay ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (if (outline-on-heading-p)
      ;; we are already looking at a heading
      (beginning-of-line)
    ;; else go back to previous heading
    (outline-previous-visible-heading 1))
  (let* ((beg (point))
	 (end (progn (outline-end-of-subtree)
		     (point))))
    (message "setting overlay, tracked-compile-mode-current-build-overlay is %S in %S" tracked-compile-mode-current-build-overlay (current-buffer))
    (if (overlayp tracked-compile-mode-current-build-overlay)
	(move-overlay tracked-compile-mode-current-build-overlay
		      beg end)
      (setq tracked-compile-mode-current-build-overlay
	    (make-overlay beg end))
      (overlay-put tracked-compile-mode-current-build-overlay
		   'face
		   '(background-color . "yellow"))
      (when nil
	(overlay-put tracked-compile-mode-current-build-overlay
		     'display
		     '(left-fringe vertical-stripes))))
    (message "set overlay, tracked-compile-mode-current-build-overlay is %S in %S" tracked-compile-mode-current-build-overlay (current-buffer))))

;; (define-fringe-bitmap 'vertical-stripes [ 52428 52428 52428 52428 52428 52428 52428 52428 52428 ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to call in back-ends ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tracked-compile-ensure-initialized-snapshot-system-function nil)

(defvar tracked-compile-unrecorded-changes-function nil)

(defvar tracked-compile-snapshot-function nil)

(defvar tracked-compile-rollback-function nil)

(defvar tracked-compile-list-snapshots-function nil)

;;;;;;;;;;;;;;;;;;;;;;
;; Back ends: darcs ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun tracked-compile-ensure-initialized-snapshot-system-function-darcs (git-dir)
  "Initialize compile version tracking for GIT-DIR using darcs."
  (unless (file-directory-p (expand-file-name "_darcs" git-dir))
    (message "Setting up darcs for %s" git-dir)
    (setenv "EMAIL" user-mail-address)
    (shell-command
     (format
      "cd %s; darcs initialize; find . -name \"*.c\" -o -name \"*.h\" | xargs darcs add ; darcs record --all -m \"Initial adding\""
      git-dir))))

(defun tracked-compile-unrecorded-changes-function-darcs (git-dir)
  "Return the unrecorded changes for GIT-DIR."
  (shell-command-to-string (format "cd %s/git; darcs whatsnew --unified" git-dir)))

(defun tracked-compile-snapshot-function-darcs (git-dir tracking-name)
  "Take a snapshot for GIT-DIR called TRACKING-NAME using darcs."
  (let ((command (format "cd %s/git; darcs record --all  -m \"%s-c\"; darcs tag  -m \"%s\"" git-dir tracking-name tracking-name)))
    (message "Taking snapshot by doing: %s" command)
    (shell-command command))
  (when t
    (message "Changes list is now %S" (tracked-compile-list-snapshots-function-darcs git-dir))))

(defun tracked-compile-rollback-function-darcs (git-dir snapshot-name)
  "Rollback GIT-DIR to SNAPSHOT-NAME using darcs."
  (let ((command (format
















		  ;; todo: I get "Shall I rollback this patch? (1/7)  [ynWvplxdaqjk], or ? for help: darcs: promptCharFancy: unexpected end of input"
















		  ;; the "--all" means don't prompt for whether to do each one
		  "cd %s; darcs rollback --all --from-patch \"%s\" -m \"Rollback to %s\""
		  git-dir snapshot-name snapshot-name)))
    (message "Doing rollback using command: %s" command)
    (shell-command command)))

(defun tracked-compile-list-snapshots-function-darcs (git-dir)
  "Get the snapshots list for GIT-DIR using darcs."
  (split-string
   (shell-command-to-string (format "cd %s; darcs show tags" git-dir))
   "\n" t))

;;;;;;;;;;;;;;;;;;;;;;
;; Back ends: guilt ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun tracked-compile-snapshot-function-guilt (git-dir tracking-name)
  "Take a snapshot of GIT-DIR with TRACKING-NAME using guilt."
  (let ((command (format "cd %s/git; guilt new -f %s" git-dir tracking-name)))
    (message "Taking snapshot by doing: %s" command)
    (shell-command command)))

(defun tracked-compile-rollback-function-guilt (git-dir snapshot-name)
  "Rollback GIT-DIR to SNAPSHOT-NAME, using guilt."
  (let ((guilt-command
	 (if (tracked-compile-patch-applied-p snapshot-name git-directory)
	     (format "cd %s/git; guilt pop %s; guilt push;" git-directory snapshot-name)
	   (format "cd %s/git; guilt push %s" git-directory snapshot-name)
	   )))
    (message "Doing %s" guilt-command)
    (shell-command guilt-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup for specific back-ends ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tracked-compile-use-darcs ()
  "Set the tracked compilation system up to use darcs for tracking."
  (interactive)
  (setq tracked-compile-snapshot-function 'tracked-compile-snapshot-function-darcs
	tracked-compile-unrecorded-changes-function 'tracked-compile-unrecorded-changes-function-darcs
	tracked-compile-ensure-initialized-snapshot-system-function 'tracked-compile-ensure-initialized-snapshot-system-function-darcs
	tracked-compile-rollback-function 'tracked-compile-rollback-function-darcs
	tracked-compile-list-snapshots-function 'tracked-compile-list-snapshots-function-darcs))

(defun tracked-compile-use-guilt ()
  "Set the tracked compilation system up to use git and guilt for tracking."
  (interactive)
  (setq tracked-compile-snapshot-function 'tracked-compile-snapshot-function-guilt
	tracked-compile-rollback-function 'tracked-compile-rollback-function-guilt))

;;;;;;;;;;;;;;;;
;; Major mode ;;
;;;;;;;;;;;;;;;;

(defun narrowest-margin (text-lines)
  "Return the narrowest margin width in TEXT-LINES.
TEXT-LINES may be either a string or a list of strings."
  (when (stringp text-lines)
    (setq text-lines (split-string text-lines "\n")))
  (apply 'min (mapcar (function
		       (lambda (line)
			 (or (string-match "\\S-" line)
			     0)))
		      text-lines)))

(defun tracked-compile-yank (&optional arg)
  "Like `yank', but if at the left margin, indent to suit this mode.
Optional argument ARG is passed on to `yank'."
  (interactive "*P")
  ;; if off the margin but only whitespace is on the line, get rid of
  ;; the whitespace first
  (let ((initial-column (current-column)))
    (when (save-excursion
	    (beginning-of-line)
	    (looking-at "^\\s-*$"))
      (delete-region (line-beginning-position) (line-end-position)))
    (let ((at-margin (bolp)))
      (yank arg)
      ;; if it was a multi-line insert at the margin, adjust the
      ;; indentation
      (when (and at-margin
		 (string-match "\n" (current-kill 0 t)))
	(put-text-property (region-beginning) (region-end) 'blockquote t)
	(indent-rigidly (region-beginning)
			(region-end)
			(- 8 (narrowest-margin (current-kill 0 t))))
	(unless (bolp)
	  (insert "\n")))
      (indent-to initial-column))))

(defvar tracked-compile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?c] 'tracked-compile-build-around-point)
    (define-key map [?\C-c ?d] 'tracking-open-date)
    (define-key map [?\C-y] 'tracked-compile-yank)
    map)
  "Map for `tracked-compile-mode'.")

(define-derived-mode tracked-compile-mode org-mode "Tracked-compilations"
  "Major mode for tracking compiled versions."
  (laptop-states-initialize))

(provide 'tracked-compile)

;; (tracked-compile-use-git)
(tracked-compile-use-darcs)

(find-file tracking-org-file)

;;; tracked-compile.el ends here
