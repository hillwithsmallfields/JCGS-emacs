;;;; Configuration for things included in the emacs distribution
;;; Time-stamp: <2018-11-22 11:20:13 jcgs>

;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

;;;; General bits and pieces

(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;;;; Garbage collection

(setq garbage-collection-messages t
      gc-cons-threshold (* 1024 1024)
      )

;;;; Time

(add-hook 'before-save-hook 'time-stamp)

(setq display-time-and-date t)
(display-time)

(when (string-match "elijah\\|duralium" (system-name))
  (display-battery-mode))

;;;; Commands

(setq disabled-command-function nil)

(let ((nc 0))
  (mapatoms
   (lambda (atom)
     (if (commandp atom)
	 (setq nc (1+ nc)))))
  (message "%d commands defined" nc))

;;;; local variables

(when (boundp 'safe-local-variable-values)
  (add-to-list 'safe-local-variable-values
	       (cons 'parser 'read))
  (add-to-list 'safe-local-variable-values
	       (cons 'TeX-Master t)))

;;;; editing

(setq kill-whole-line t
      parens-require-spaces nil)

(set-default 'indent-tabs-mode nil)

(defcustom quotes-require-spaces t
  "If non-nil, add whitespace as needed when inserting quotes.
This affects `insert-quotes'."
  :type 'boolean
  :group 'lisp)

(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `\"\"' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

This command assumes point is not in a string or comment."
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(global-set-key "\M-\"" 'insert-quotes)

(global-set-key "\C-x\M-f" 'find-file-at-point)

;;;; motion

(setq line-move-visual nil
      shift-select-mode nil)

;;;; backup files

(setq version-control t)
(add-to-list 'backup-directory-alist
	     (cons (or (getenv "COMMON")
		       (expand-file-name "~/common"))
		   (expand-file-name "~/common-backups")))

;;;; UI

(setq enable-recursive-minibuffers t)
(when (fboundp 'minibuffer-indicate-depth-mode)
  (minibuffer-indicate-depth-mode 1))

(tool-bar-mode -1)
(menu-bar-mode -1)

(add-hook 'completion-setup-hook 'shrink-window-if-larger-than-buffer)

(add-to-list 'completion-ignored-extensions ".ogg")

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(defun bring-to-top-of-menu (menu item)
  "In MENU, bring ITEM to the top."
  ;; (message "In %S trying to bring %S to the top" menu item)
  (condition-case var
      (let ((old (catch 'found
		   (let* ((trailing menu)
			  (find-old (cdr trailing)))
		     (while find-old
		       (if (eq (caar find-old) item)
			   (throw 'found trailing)
			 (setq trailing find-old
			       find-old (cdr find-old)))))
		   nil)))
	(if old
	    (progn
	      (rplacd menu (cons (cadr old) (cdr menu)))
	      (rplacd old (cddr old)))
	  (message "%S was not present in menu, so could not move it to top" item)))
    (error (message "Error in moving menu items"))))

(defun replace-in-menu (menu old new)
  "In MENU, replace occurrences of OLD with NEW."
  (message "Replacing %S with %S in %S" old new menu)
  (let ((m (cdr menu)))
    (while m
      (let ((item (car m)))
	(message "Does %S match %S?" item old)
	(cond
	 ((eq (cdddr item) old)
	  (message "matched tail")
	  )
	 ((eq (car item) old)
	  (message "matched on car"))
	 ((and (consp (cdddr item))
	       (>= (length item) 4)
	       (eq (nth 1 item) 'menu-item)
	       (eq (nth 3 item) old))
	  (message "matched menu-item"))))
      (setq m (cdr m)))))

(condition-case error-var
    ;; These don't work when you don't have menus, of course!  We
    ;; could look at `menu-bar-mode', but I'd still like to make these
    ;; re-arrangements in case of menus appearing later in the
    ;; session.  So just catch errors harmlessly.
    (progn
      (bring-to-top-of-menu menu-bar-help-menu 'describe)
      (unless (>= emacs-major-version 23)
	(bring-to-top-of-menu menu-bar-describe-menu 'describe-key))
      (bring-to-top-of-menu menu-bar-describe-menu 'describe-variable)
      (bring-to-top-of-menu menu-bar-describe-menu 'describe-function)
      ;; (replace-in-menu menu-bar-file-menu 'find-file 'flexi-find-file)
      )
  (error (message "Problem in re-arranging menus")))

(setq mouse-avoidance-threshold 10
      mouse-avoidance-nudge-dist 20)

(mouse-avoidance-mode 'proteus)

(setq line-move-visual nil)

(defun recenter-top-bottom (&optional arg)
  "Move current line to window center, top, and bottom, successively.
With no prefix argument, the first call redraws the frame and
 centers point vertically within the window.  Successive calls
 scroll the window, placing point on the top, bottom, and middle
 consecutively.  The cycling order is middle -> top -> bottom.

A prefix argument is handled like `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

If the transient mark is set, the region is centred in the window.

Top and bottom destinations are actually `scroll-margin' lines
 the from true window top and bottom."
  (interactive "P")
  (cond
   (arg (recenter arg))                 ; Always respect ARG.
   ((region-active-p)
    (message "region active")
    (let* ((lines (count-lines (region-beginning) (region-end)))
	   (slack (- (window-height)
		     (* scroll-margin 2)
		     (if header-line-format 2 1)
		     lines)))
      (recenter (* (+ scroll-margin
		      (if (> slack 0)
			  (/ slack 2)
			0))
		   (if (= (point) (region-beginning)) 1 -1)))))
   ((or (not (eq this-command last-command))
	(eq recenter-last-op 'bottom))
    (setq recenter-last-op 'middle)
    (recenter))
   (t
    (let ((this-scroll-margin
	   (min (max 0 scroll-margin)
		(truncate (/ (window-body-height) 4.0)))))
      (cond ((eq recenter-last-op 'middle)
	     (setq recenter-last-op 'top)
	     (recenter this-scroll-margin))
	    ((eq recenter-last-op 'top)
	     (setq recenter-last-op 'bottom)
	     (recenter (- -1 this-scroll-margin))))))))

(defvar other-window-or-frame-starting-window nil)

(defun other-window-or-frame (&optional count)
  "Select another window, or frame, or screen.
COUNT can be passed in to make it negative."
  (interactive "p")
  (if (eq this-command last-command)
      (progn
	(other-window count)
	(when (eq (selected-window) other-window-or-frame-starting-window)
	  (other-frame count)
          ;; (setq other-window-or-frame-starting-window (selected-window))
          ))
    (setq other-window-or-frame-starting-window (selected-window))
    (other-window count)))

(global-set-key "\C-xo" 'other-window-or-frame)

;;;; History

(setq history-length t
      ;; deleting duplicates would be nice, but I once started a
      ;; project to use command history to predict next commands, and
      ;; would like to complete it sometime... but for now, let's
      ;; delete them, for speed of finding the one I want
      history-delete-duplicates t)

;;;; Display

(setq default-indicate-empty-lines t
      indicate-buffer-boundaries 'left
      split-height-threshold 12
      split-width-threshold 0
      split-window-preferred-function 'split-window-sensibly ; 'split-window-horizontally
      default-indicate-buffer-boundaries 'left
      )

(global-font-lock-mode 1)

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ;Man page
        (".*Dired.*"             . font-lock-comment-face) ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face) ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face) ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face) ; Modified
        ("^.[%].*"               . font-lock-keyword-face))) ; Read only

(defun buffer-menu-custom-font-lock  ()
  "Font locking function for buffer menu buffers."
  (let ((font-lock-unfontify-region-function
         (lambda (start end)
           (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
         '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)

(add-to-list 'default-frame-alist (cons 'font "6x10"))

(defun jcgs/frame-setup ()
  "Set up a frame the way I usually like it."
  (interactive)
  (when window-system
    ;; (set-face-font 'mode-line "6x10") ; is this included in set-frame-font?
    (set-frame-font "6x10")
    ;; (set-face-font 'mode-line "-misc-fixed-medium-r-normal-*-8-*-*-*-c-*-iso10646-*") ; probably too small
    (set-face-background 'mode-line "brown")
    (set-face-foreground 'mode-line "white")

    (message "frame width is %d" (frame-width))

    (cond
     ((>= (frame-width) 240)
      (delete-other-windows)
      (add-to-list 'load-path (expand-file-name "appearance" user-emacs-directory))
      (require 'split-window-multi)
      (split-to-80-columns)))

    (when (fboundp 'modify-frame-parameters)
      (modify-frame-parameters
       nil
       '((scroll-bar-width . 7)
	 (scroll-bar-background . "white")
	 (scroll-bar-foreground . "red"))))))

(when (fboundp 'size-indication-mode)
  (size-indication-mode 1))

;; from http://www.emacswiki.org/cgi-bin/wiki/ModeLineDirtrack
(defun add-mode-line-dirtrack ()
      (add-to-list 'mode-line-buffer-identification
                   '(:propertize (" " default-directory " ") face dired-directory)))

(when (= emacs-major-version 22)
  (add-hook 'shell-mode-hook 'add-mode-line-dirtrack))

;;;; file changes

(setq revert-without-query '("\\.log"))

;;;; image-dired

(defun copy-image-filename-to-kill-ring ()
  "Copy the name of the current image, to the `kill-ring'."
  (interactive)
  (let ((filename (image-dired-original-file-name)))
    (message "%s" filename)
    (kill-new filename)))

(setq image-dired-dir (expand-file-name "~/tmp")
      image-dired-thumbnail-storage 'standard
      image-dired-rotate-original-ask-before-overwrite nil)

(eval-after-load "image-dired"
  '(define-key image-dired-thumbnail-mode-map "?" 'copy-image-filename-to-kill-ring))

;;;; Bookmarks

(setq bookmark-default-file (substitute-in-file-name
			     "$COMMON/var/bookmarks.bmk")
      bookmark-save-flag 1
      bookmark-bmenu-toggle-filenames nil
      bookmark-automatically-show-annotations t
      )

;;;; Desktop, history, and session control

(if (fboundp 'desktop-save-mode)
    (desktop-save-mode 1)
  (desktop-read))

(when (boundp 'desktop-globals-to-save)
  (push 'Info-history-list desktop-globals-to-save)
  (push 'kill-ring desktop-globals-to-save))

(setq desktop-restore-eager t ; 8
      confirm-kill-emacs 'yes-or-no-p
      savehist-coding-system 'utf-8
      savehist-file (substitute-in-file-name "$COMMON/var/emacs-history")
      savehist-file-modes nil		; can't set them on FAT file systems
      desktop-dirname (substitute-in-file-name "$HOME")
      )

;; (when (fboundp 'savehist-mode)
;;   (savehist-mode 1))

;;;; Ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
				    (if (> (frame-width) 150)
					(split-window-horizontally arg)
				      (split-window-vertically arg))))

;;;; Compare windows

(setq compare-ignore-whitespace t
      ;; compare-windows-sync t
      )

;;;; Compilation

(setq next-error-highlight t
      next-error-highlight-no-select t
      next-error-recenter t)

(defvar voice-announce-compilation-finish nil
  "*Whether to make a voice announcement of compilation finishing.")

(defvar browser-announce-compilation-finish nil
  "*Whether to notify compilation finishing via the browser.")

(defvar compilation-finish-count 0
  "How many compilations have now finished.
Displayed to make the notification pages look more obviously distinct,
in case several build up before you dismiss them.")

(defun jcgs-compilation-finished (buffer manner)
  "Note that in BUFFER, compilation has finished in MANNER."
  ;; (message "Compilation finished %s %s" buffer manner)
  (when (and voice-announce-compilation-finish
	     (string-match "hosea" (system-name)))
    (shell-command (format "flite -t \"Compilation %s\"" manner)))
  (when browser-announce-compilation-finish
    (require 'notify-via-browse-url)
    (setq compilation-finish-count (1+ compilation-finish-count))
    (notify-via-browse-url nil
			   (format "Compilation %d finished: %s (at %s)"
				   compilation-finish-count
				   manner
				   (current-time-string))
			   "<a href=\"#end\">Jump to end</a><hr>\n"
			   (list "<pre>" buffer "</pre>\n")
			   "<hr><a name=\"end\">End of notification</a>")))

(add-hook 'compilation-finish-functions 'jcgs-compilation-finished)

(defvar pre-compilation-frames nil
  "A hack to capture the frame configuration before the compilation window appears.")

(setq compilation-buffer-name-function
      (lambda (foo)
        "A hack to capture the frame configuration before the compilation window appears."
        (setq pre-compilation-frames (current-frame-configuration))
        (format "*Compilation from %s*" (buffer-name))))

(defun jcgs/hide-successful-compilation (buffer manner)
  "Handle a successful compilation.
Argument BUFFER is ignored.
Argument MANNER is used to decide what to do."
  (when (and pre-compilation-frames (equal manner "finished"))
    (set-frame-configuration pre-compilation-frames))
  (message "Compilation %s" manner))

(add-hook 'compilation-finish-functions 'jcgs/hide-successful-compilation)

(defun ancestor-directory-containing (directory file)
  "Return the closest parent* directory of DIRECTORY that has FILE in it."
  (interactive
   (list default-directory
	 (read-from-minibuffer "Insert parent directory containing file: ")))
  (while (and (stringp directory)
	      (not (file-exists-p (expand-file-name file directory))))
    (when (string-match "/$" directory)
      (setq directory (substring directory 0 -1)))
    (setq directory (file-name-directory directory)))
  (when (and directory (interactive-p))
    (insert directory))
  directory)

(defun find-ancestral-file (file)
  "Find FILE in a directory above the present one.
The nearest FILE is used."
  (interactive "sFind ancestral file: ")
  (find-file
   (expand-file-name file
		     (ancestor-directory-containing default-directory file))))

(setq compilation-ask-about-save nil
      compilation-read-command nil)

;;;; Info

(add-to-list 'Info-default-directory-list
	     (expand-file-name
	      (substitute-in-file-name "$GATHERED/info")))

;;;; Mail

(setq mail-self-blind t
      user-mail-address "john.sturdy@grapeshot.com")

;;;; Comint

(require 'comint)			; so we can overwrite comint-snapshot-last-prompt

(setq comint-use-prompt-regexp t
      shell-prompt-pattern "^[a-z0-9]+@[-a-z0-9]+:[-._/a-z0-9~]+\\$"
      comint-prompt-read-only t
      )

(when (or (not (boundp 'jcgs-added-github-prompt))
	  (not jcgs-added-github-prompt))
  (setq comint-password-prompt-regexp
	(concat "\\(Password for.+:\\)\\|\\(" comint-password-prompt-regexp "\\)")
	jcgs-added-github-prompt t))

;;;; Shell

(defun jcgs/shell-mode-send-input ()
  "Send the current line to the shell, even if the shell buffer objects."
  (interactive)
  (let ((inhibit-read-only t))
    (comint-send-input)))

(defvar jcgs/shell-mode-recorded-commands nil
  "For deduplication.
Not persistent between sessions, and reset each day.")

(make-variable-buffer-local 'jcgs/shell-mode-recorded-commands)

(defconst jcgs/shell-command-record-regexp
  "^ +\\([^:]+\\):\\([^$]+\\)\\$ \\(.+\\)$")

(defun jcgs/shell-command-records-get-buffer ()
  "Get the buffer of the current line, and put it on the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at jcgs/shell-command-record-regexp)
        (kill-new (match-string-no-properties 1))
      (error "Not on a shell history line"))))

(defun jcgs/shell-command-records-get-command ()
  "Get the command of the current line, and put it on the kill ring.."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at jcgs/shell-command-record-regexp)
        (kill-new (match-string-no-properties 3))
      (error "Not on a shell history line"))))

(defun jcgs/shell-command-records-get-directory ()
  "Get the directory of the current line, and put it on the kill ring.."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at jcgs/shell-command-record-regexp)
        (kill-new (match-string-no-properties 2))
      (error "Not on a shell history line"))))

(defun jcgs/shell-command-records-set-buffer-other-window ()
  "Select the buffer of the current line, in another window."
  (interactive)
  (switch-to-buffer-other-window
   (save-excursion
     (beginning-of-line 1)
     (if (looking-at jcgs/shell-command-record-regexp)
         (match-string-no-properties 1)
       (error "Not on a shell history line")))))

(defun jcgs/shell-command-records-insert-command-other-window ()
  "Insert the command on this line into the other window."
  (interactive)
  (let ((command (save-excursion
                   (beginning-of-line 1)
                   (if (looking-at jcgs/shell-command-record-regexp)
                       (match-string-no-properties 3)
                     (error "Not on a shell history line")))))
    (other-window 1)
    (insert command)))

(defun jcgs/shell-command-records-run-in-nearest-shell (&optional no-confirm)
  "Run the command on this line, in the most recent shell buffer.
Ask for confirmation of the buffer, except with prefix arg NO-CONFIRM."
  (interactive "P")
  (let* ((command (save-excursion
                    (beginning-of-line 1)
                    (if (looking-at jcgs/shell-command-record-regexp)
                        (match-string-no-properties 3)
                      (error "Not on a shell history line"))))
         (shell-buffer (catch 'got-one
                         (dolist (buffer (buffer-list))
                           (set-buffer buffer)
                           (when (eq major-mode 'shell-mode)
                             (throw 'got-one buffer)))
                         nil)))
    (when (and shell-buffer (or no-confirm (y-or-n-p (format "Run %s in %s? " command shell-buffer))))
      (switch-to-buffer shell-buffer)
      (comint-send-string (get-buffer-process shell-buffer) (concat command "\n")))))

(defun jcgs/shell-command-records-run-in-same-shell (&optional no-confirm)
  "Run the command on this line, in the same shell it was originally run in.
Ask for confirmation of the buffer, except with prefix arg NO-CONFIRM."
  (interactive "P")
  (let* ((buffer-directory-command (save-excursion
                                     (beginning-of-line 1)
                                     (if (looking-at jcgs/shell-command-record-regexp)
                                         (list
                                          (match-string-no-properties 1)
                                          (match-string-no-properties 2)
                                          (match-string-no-properties 3))
                                       (error "Not on a shell history line"))))
         (shell-buffer (nth 0 buffer-directory-command))
         (directory (nth 1 buffer-directory-command))
         (command (nth 2 buffer-directory-command)))
    (when (and shell-buffer (or no-confirm (y-or-n-p (format "Run %s in %s? (dir %s)" command shell-buffer directory))))
      (switch-to-buffer shell-buffer)
      (unless (equal directory default-directory)
        (comint-send-string (get-buffer-process shell-buffer)
                            (concat "cd " directory "\n")))
      (comint-send-string (get-buffer-process shell-buffer) (concat command "\n")))))

(defun jcgs/shell-command-records-run-in-same-shell-with-gdb (&optional no-confirm)
  "Run the command on this line with gdb, in the same shell it was originally run in.
Ask for confirmation of the buffer, except with prefix arg NO-CONFIRM."
  (interactive "P")
  (let* ((buffer-directory-command (save-excursion
                                     (beginning-of-line 1)
                                     (if (looking-at
                                          jcgs/shell-command-record-regexp)
                                         (list
                                          (match-string-no-properties 1)
                                          (match-string-no-properties 2)
                                          (match-string-no-properties 3))
                                       (error "Not on a shell history line"))))
         (shell-buffer (nth 0 buffer-directory-command))
         (directory (nth 1 buffer-directory-command))
         (command-line-parts (split-string (nth 2 buffer-directory-command)))
         (command (car command-line-parts))
         (command-args (mapconcat 'identity
                                  (cons "run" (cdr command-line-parts))
                                  " ")))
    (when (and shell-buffer (or no-confirm
                                (y-or-n-p (format "Run %s in %s? (dir %s)"
                                                  command
                                                  shell-buffer
                                                  directory))))
      (switch-to-buffer shell-buffer)
      (unless (equal directory default-directory)
        (comint-send-string (get-buffer-process shell-buffer)
                            (concat "cd " directory "\n")))
      (kill-new command-args)
      (message "run command %S is on the kill ring" command-args)
      (comint-send-string (get-buffer-process shell-buffer)
                          (concat "gdb " command "\n")))))

(defvar jcgs/shell-command-records-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "b" 'jcgs/shell-command-records-get-buffer)
    (define-key map "c" 'jcgs/shell-command-records-get-command)
    (define-key map "d" 'jcgs/shell-command-records-get-directory)
    (define-key map "g" 'jcgs/shell-command-records-run-in-same-shell-with-gdb)
    (define-key map "B" 'jcgs/shell-command-records-set-buffer-other-window)
    (define-key map "C" 'jcgs/shell-command-records-insert-command-other-window)
    (define-key map "r" 'jcgs/shell-command-records-run-in-nearest-shell)
    (define-key map (kbd "RET") 'jcgs/shell-command-records-run-in-same-shell)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'outline-up-heading)
    map))

(define-derived-mode jcgs/shell-command-records-mode jcgs/org-journal-mode "Shell command records"
  "Major mode for an accumulated list of shell commands.")

(defun jcgs/shell-mode-record-command-in-journal (shell-command)
  "Record SHELL-COMMAND in my journal file."
  (when (and (boundp 'jcgs/shell-mode-accumulated-command-history-file)
	     (stringp jcgs/shell-mode-accumulated-command-history-file)
	     (file-writable-p jcgs/shell-mode-accumulated-command-history-file)
	     (not (string-match "^!\\(!\\|[-0-9]+\\)$" shell-command)))
    (set-text-properties 0 (length shell-command)
			 nil shell-command)
    (let ((cwd default-directory)
	  (buffer (buffer-name)))
      (save-window-excursion
	(find-file jcgs/shell-mode-accumulated-command-history-file)
	(goto-char (point-max))
	(when (let ((date (decode-time)))
		(jcgs/org-journal-open-date (nth 5 date) (nth 4 date) (nth 3 date) t
                                            'jcgs/shell-command-records-mode))
	  (setq jcgs/shell-mode-recorded-commands nil))
	(goto-char (point-max))
	(dolist (command-line (split-string shell-command "\n" t))
	  ;; todo: fix this filtering, it doesn't seem to be working
	  ;; todo: filter out history commands that don't alter anything
	  ;; todo: filter out "history" commands
	  (unless (member command-line jcgs/shell-mode-recorded-commands)
	    (push command-line jcgs/shell-mode-recorded-commands)
	    ;; (message "Got shell command %S to record as happening in buffer %S with default directory %s." command-line buffer cwd)
	    (let ((was-cd (string-match "^cd\\(?:\\s-+\\(.+\\)\\)?" command-line)))
	      (when was-cd
		(let ((cd-arg (match-string 1 command-line)))
		  ;; (message "old cwd %S" cwd)
		  (setq cwd (if cd-arg
				(expand-file-name cd-arg cwd) ; todo: this area is not behaving as I expected, cwd is already one too high
			      (getenv "HOME")))
		  ;; (message "Command was cd, with arg %S; cwd now %S" cd-arg cwd)
		  ))))
	  (delete-blank-lines)
	  (insert "    " buffer ":" cwd "$ " command-line)
	  (basic-save-buffer)
	  (bury-buffer))))))

(defun jcgs/shell-mode-interrupt-subjob ()
  "Getting around a problem with it complaining that the text is
read-only (although I don't think I'd changed anything related)
--- 20121114 JCGS."
  (interactive)
  (let ((inhibit-read-only t))
    (comint-interrupt-subjob)))

(defun jcgs/shell-mode-erase-buffer ()
  "Erase the buffer, even if it objects."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq comint-last-prompt-overlay nil)))

(defun jcgs/shell-mode-latest-jira-mention ()
  "Insert the latest thing that looks like a jira ticket."
  (interactive)
  (let ((ticket (save-excursion (re-search-backward "\\<[A-Z][A-Z]-[0-9]\\{3,5\\}\\>" (point-min) t))))
    (if ticket
        (insert (match-string 0))
      (error "Could not find anything that looked like a JIRA ticket"))))

(defun jcgs/shell-mode-setup ()
  "Set shell mode up the way I want it."
  (add-hook 'comint-input-filter-functions 'jcgs/shell-mode-record-command-in-journal t t)
  (define-key shell-mode-map "\C-ce" 'jcgs/shell-mode-erase-buffer)
  (define-key shell-mode-map "\C-cr" 'comint-fix-ssh-known-hosts)
  (define-key shell-mode-map "\C-c\C-c" 'jcgs/shell-mode-interrupt-subjob)
  (define-key shell-mode-map "\C-cj" 'jcgs/shell-mode-latest-jira-mention)
  (define-key shell-mode-map "\r" 'jcgs/shell-mode-send-input)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (highlight-regexp "succeeded" 'hi-green)
  (highlight-regexp "failed" 'hi-red-b)
  (highlight-lines-matching-regexp "^==[0-9]+==") ; valgrind output
  (highlight-lines-matching-regexp "^FAIL: " 'hi-red-b))
   
(add-hook 'shell-mode-hook 'jcgs/shell-mode-setup)

;; (unless (string-match "Password for" comint-password-prompt-regexp))

(defun comint-fix-ssh-known-hosts ()
  "Look for an \"offending key\" message, and fix the file, and retry."
  (interactive)
  (when (re-search-backward "Offending.+key in \\([^:]+\\):\\([0-9]+\\)" (point-min) t)
    (let* ((filename (match-string-no-properties 1))
	   (file-buffer (find-buffer-visiting filename))
	   (line-number (string-to-number (match-string-no-properties 2))))
      (save-window-excursion
	(save-excursion
	  (if (null file-buffer)
	      (find-file filename)
	    (set-buffer file-buffer)
	    (revert-buffer t t))
	  (goto-line line-number)
	  (beginning-of-line 1)
	  (let ((line-start (point)))
	    (beginning-of-line 2)
	    (delete-region line-start (point)))
	  (let ((delete-old-versions t))
	    (basic-save-buffer))
	  (if file-buffer
	      (bury-buffer file-buffer)
	    (kill-buffer file-buffer))))
      (let ((last-command (comint-previous-input-string 0)))
	(goto-char (point-max))
	(insert last-command)
	(comint-send-input)))))

(defvar jcgs-notify-on-command-finishing nil
  "Whether to notify me when the current shell command finishes.")

(defun jcgs-notify-on-command-finishing ()
  "Ask to be notified when the current shell command finishes."
  (interactive)
  (setq jcgs-notify-on-command-finishing t))

(defun jcgs-comint-watch-for-prompt (string)
  "Watch for the prompt appearing.
Argument STRING is the string to check."
  (when (and jcgs-notify-on-command-finishing
	     (string-match shell-prompt-pattern string))
    (setq jcgs-notify-on-command-finishing nil)
    (require 'notify-via-browse-url)
    (notify-via-browse-url nil "Shell command completed" "Shell command completed")))

(defun jcgs-add-notify-on-command-finishing ()
  "Hook function for adding notification to my shells."
  (add-hook 'comint-output-filter-functions 'jcgs-comint-watch-for-prompt)
  (define-key shell-mode-map "\C-cn" 'jcgs-notify-on-command-finishing))

(add-hook 'shell-mode-hook 'jcgs-add-notify-on-command-finishing)

(defun comint-snapshot-last-prompt ()
  "`snapshot' any current `comint-last-prompt-overlay'.
Freeze its attributes in place, even when more input comes along
and moves the prompt overlay."
  (when (and (boundp 'comint-last-prompt-overlay)
	     comint-last-prompt-overlay
	     (>= (overlay-start comint-last-prompt-overlay) (point-min))
	     (<= (overlay-end comint-last-prompt-overlay) (point-max)))
    (let ((inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (add-text-properties (overlay-start comint-last-prompt-overlay)
			   (overlay-end comint-last-prompt-overlay)
			   (overlay-properties comint-last-prompt-overlay)))))

;;;; Inferior Lisp

(setq inferior-lisp-program "sbcl"
      scheme-program-name "siod")

;;;; Terminal

(fset 'ctl-x-map ctl-x-map)

(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "M-s-x") 'execute-extended-command)
     (define-key term-raw-map (kbd "C-s-n") 'next-line)
     (define-key term-raw-map (kbd "C-s-p") 'previous-line)
     (define-key term-raw-map (kbd "C-s-f") 'forward-char)
     (define-key term-raw-map (kbd "C-s-b") 'backward-char)
     (define-key term-raw-map (kbd "C-s-SPC") 'set-mark-command)
     (define-key term-raw-map (kbd "M-s-w") 'kill-ring-save)
     (define-key term-raw-map (kbd "C-s-s") 'isearch-forward)
     (define-key term-raw-map (kbd "C-s-r") 'isearch-backward)
     (define-key term-raw-map (kbd "M-s-<") 'beginning-of-buffer)
     (define-key term-raw-map (kbd "M-s->") 'end-of-buffer)
     (define-key term-raw-map (kbd "C-s-x") 'ctl-x-map)))

;;;; Emacs Server

(setq ;; server-use-tcp t
      server-host (system-name)
      server-auth-dir (expand-file-name "~/.emacs-servers"))

(unless (file-directory-p server-auth-dir)
  (make-directory server-auth-dir))

(set-file-modes server-auth-dir (file-modes-symbolic-to-number "go-rwx" (file-modes server-auth-dir)))

(server-start)

;;;; my own colour themes

(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

(defun color-theme-vellum ()
  "Brightly illuminated vellum manuscript theme.
John Sturdy <john@cb1.com>"
  (interactive)
  (color-theme-standard)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     '(color-theme-vellum
       ((background-color . "Wheat"))
       (modeline ((t (:foreground "brown" :background "black"))))
       (font-lock-keyword-face ((t (:foreground "purple4"))))
       (font-lock-string-face ((t (:foreground "Brown"))))
       )))


  
  ;; (color-theme-install
  ;;  '(color-theme-vellum
  ;;    ;; frame parameters
  ;;    ((foreground-color . "brown")
  ;;     (background-color . "wheat")
  ;;     (cursor-color . "red")
  ;;     (mouse-color . "red")
  ;;     )
  ;;    ;; variable settings
  ;;    (
  ;;     )
  ;;    ;; face definitions
  ;;    (modeline ((t (:background "purple"
  ;; 				:foreground "dark red"
  ;; 				:height 0.9
  ;; 				:bold t))))
  ;;    (font-lock-comment-face ((t (:foreground "purple" :italic t))))
  ;;    (font-lock-function-name-face ((t (:background "gold"
  ;; 						    :foreground "red"
  ;; 						    :height 1.25
  ;; 						    :bold t
  ;; 						    :box (:color "dark green"
  ;; 								 :style released-button
  ;; 								 :width 2)))))
  ;;    (font-lock-variable-name-face ((t (:background "yellow" :foreground "green"))))
  ;;    (font-lock-type-face ((t (:background "yellow" :foreground "blue"))))
  ;;    (font-lock-builtin-face ((t (:foreground "DarkBlue"))))
  ;;    (font-lock-string-face ((t (:foreground "DarkSlateBlue" :italic t))))
  ;;    (font-lock-warning-face ((t (:foreground "orange" :bold t))))
  ;;    ))
  )

;;;; colour in the tty interface

(defun colour-enable-tty-frame ()
  "Toggle the colouredness of the current tty frame."
  (interactive)
  (if (null (frame-parameter (selected-frame) 'tty-color-mode))
      (set-frame-parameter (selected-frame) 'tty-color-mode 8)
    (set-frame-parameter (selected-frame) 'tty-color-mode nil)))

;;;; erc

(setq erc-server "irc-int.xci-test.com" 
      erc-port 1494
      erc-nick "jcgs"
      erc-user-full-name user-full-name 
      ;; erc-email-userid "userid"	     ; for when ident is not activated
      ;; erc-prompt-for-password t
      erc-password "aVRu3naK"		; "dJ49kRzm"
      erc-auto-query 'window
      erc-autojoin-channels-alist '(("irc-int.xci-test.com"
				     "#xc"
				     "#the_kitchen"
				     "#nxbuild"
				     )
				    ("cangenet.panaceas.com"
				     "#ish"))
      erc-hide-list '("JOIN" "PART" "QUIT")
      )				       ; OPN doesn't require passwords

(defun show-erc ()
  "Show my usual erc channels."
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer "#xc")
    (if (>= (frame-width) 80)
	(split-window-horizontally)
      (split-window))
    (other-window 1)
    (switch-to-buffer "#the_kitchen")
    (message (substitute-command-keys "\\[exit-recursive-edit] to restore previous windows"))
    (recursive-edit))
  (bury-buffer "#the_kitchen")
  (bury-buffer "#xc"))

(defun erc-tidy ()
  "Remove junk from an erc buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (dolist (delendum '("has quit"
			  "has joined channel"
			  "^\\s-+\\[[0-9:]+\\]"))
	(goto-char (point-min))
	(delete-matching-lines delendum)))))

;;;; messages-mode

(when (and (boundp 'messages-buffer-mode-map)
	   (keymapp messages-buffer-mode-map))
  (define-key messages-buffer-mode-map "\C-c\C-e" 'jcgs/shell-mode-erase-buffer))

;;;; browse-url

(setq browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t)

;;;; manual

(setq Man-width 80)

;;;; fix a problem I get on my ubuntu machine at work

;;; The problem is it's easy to get rid of a frame accidentally, but I
;;; can't recreate it with C-x 5 b sometimes.

(defun suspend-frame-with-query ()
  "Does `suspend-frame' but check first."
  (when (yes-or-no-p "Suspend frame? ")
      (suspend-frame)))

(let ((case-fold-search t))
  (when (string-match "ubuntu" (shell-command-to-string "uname -a"))
    (global-set-key "\C-z" 'suspend-frame-with-query)))

;;; end of config-distribution.el
