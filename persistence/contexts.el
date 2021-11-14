;;; Time-stamp: <2021-11-14 18:24:28 jcgs>
;;;; contexts.el -- manipulate sets of buffers for particular contexts
;;; This is connected with my save-emacs-context code.

(provide 'contexts)
(require 'memory-monitor)

(mapcar 'makunbound '(context-contexts-directory
		      current-context
		      context-contexts
		      contexts-loaded
		      ;; contexts-observed
		      context-files
		      context-keymap
		      context-list-mode-keymap
		      context-annotation))

(defvar context-contexts-directory (substitute-in-file-name "$SYNCED/var/emacscontexts")
  "The directory for holding context files.
Each context file is the elisp to run to load that context.
This requires the restorebuffer package, and calls context-mark-loaded
to maintain the data structures. It is re-written by context-save.")

(defvar current-context ""
  "The name of the current context.")

(defvar context-mode-line-string ""
  "What we display as the currently loaded context.")

(defun context-update-mode-line ()
  "Update the mode-line display of the context."
  (setq context-mode-line-string
	(mapconcat (function
		    (lambda (context-descr)
		      (if (string= (car context-descr) current-context)
			  (propertize current-context
				      'face (cons 'foreground-color "blue"))
			(car context-descr))))
		   contexts-loaded ",")))

(if (not (memq 'context-mode-line-string global-mode-string))
    (setq global-mode-string
	  (append global-mode-string
		  '("[" context-mode-line-string "]"))))

(defvar context-contexts
  (mapcar (function
	   (lambda (contextname)
	     (list (substring contextname
			      0 (string-match "\\.el$" contextname))
		   (expand-file-name contextname
				     context-contexts-directory)
		   nil)))
	  (directory-files context-contexts-directory nil "\\.el$"))
  "The list of possible contexts.
Each entry is a list, thus:
  The name of the context
  The name of the file containing the elisp to run to load that context
  ")

(defvar contexts-observed nil
  "An alist of all the contexts that have been seen this session.
Car of the pair is the context name, cdr starts a list of the
directories this context covers.")

(defvar context-files nil
  "The list of files in the current context.")

(defvar context-annotation ""
  "String annotating this context, e.g. what you were doing in it last")

(defun context-message (format-string &rest format-args)
  "Like message, but also save in .context-log"
  (save-window-excursion
  (let ((string (apply 'format format-string format-args)))
    (find-file "~/.context-log")
    (goto-char (point-max))
    (insert string "\n")
    (basic-save-buffer)
    (bury-buffer)))
  (apply 'message format-string format-args))

(context-message "Emacs session started at %s" (current-time-string))

(defun context-file-name (name)
  "Return the name of the context file for context NAME."
  (expand-file-name (format "%s.el" name) context-contexts-directory))

(defvar context-load-prompt
  "Context to load: "
  "String to give as prompt for loading a context.
This normally asks \"Context to load: \", but when called to get the
next context from another command, will ask which context to replace
the old one by.")

(defun context-select-next ()
  "Ask for the next context, and select it."
  (let ((context-load-prompt "Switch to next context: "))
    (call-interactively 'context-load)))

(defun context-load (context)
  "Load CONTEXT into emacs, pushing out the previous one."
  (interactive
   (let ((context (context-list-mode-context-at-point)))
     (if context
	 (list context)
       (list (completing-read context-load-prompt
			      context-contexts
			      nil
			      t
			      nil)))))
  (context-message "Loading context %s" context)
  (unless (string= current-context "")
    (context-message "Saving and removing existing current context %s" current-context)
    (context-save-and-kill))
  (context-mark-loaded context);; temporary, until all old contexts are contected
  (with-memory-monitor (format "While loading context %s, there were " context)
		       (load-file (context-file-name context)))
  (context-update-mode-line)
  (cond
   ((string= context-annotation "") nil)
   ((string-match "\n" context-annotation)
    (with-output-to-temp-buffer "*Annotation*"
      (princ context-annotation))
    (sit-for 4))
   (t (context-message "%s" context-annotation))))

(defvar contexts-loaded nil
  "List of current contexts, as an alist.")

(defun context-list-mode-context-at-point ()
  "Return which context the point is on (in the context list buffer)."
  (if (eq major-mode 'context-list)
      (save-excursion
	(beginning-of-line 1)
	(if (re-search-forward ".. \\(.+\\)$" (point-max) t)
	    (buffer-substring (match-beginning 1) (match-end 1))
	  nil))
    nil))

(defun context-list-mode ()
  "Major mode for displaying context lists."
  (interactive)
  (fundamental-mode)
  (setq major-mode 'context-list
	mode-name "Context list")
  (use-local-map context-list-mode-keymap)
)

(defun context-make-list-buffer ()
  "List the available contexts, marking which are loaded."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create " *Contexts*"))
  (toggle-read-only -1)
  (erase-buffer)
  (dolist (context context-contexts)
    (let* ((name (car context))
	   (loaded (assoc name contexts-loaded))
	   (observed (assoc name contexts-observed))
	   )
      (insert (format "%s %s\n"
		      (cond
		       ((string= name current-context) "=>")
		       (loaded "* ")
		       (observed ". ")
		       (t "  "))
		      name))))
  (toggle-read-only 1)
  (goto-char (point-min))
  (context-list-mode))

(defun context-mark-loaded (context &optional files)
  "Mark that CONTEXT, containing FILES, is loaded."
  (context-message "Adding %s to loaded contexts list" context)
  (setq current-context context)
  (when files
    (setq files (mapcar 'substitute-in-file-name files)
	  context-files files))
  (let ((entry (assoc context contexts-loaded)))
    (if entry
	(if files
	    (rplaca (cdr (cdr entry))
		    files))
      (push (list context
		  (context-file-name context)
		  files)
	    contexts-loaded)))
  (context-update-mode-line))

(defun context-mark-unloaded (context)
  "Mark that CONTEXT is not loaded."
  (context-message "Removing %s from loaded contexts list" context)
  (setq contexts-loaded
	(delete-if (function
		    (lambda (pair)
		      (string= (car pair) context)))
		   contexts-loaded))
  (when (string= current-context context)
    (let ((pair (assoc context contexts-observed)))
      (unless pair
	(setq pair (cons context nil)
	      contexts-observed (cons pair contexts-observed)))
      (rplacd pair (context-list-directories-in-buffers))))
  (if (string= context current-context)
      (setq current-context ""))
  (context-update-mode-line))

(defun context-add (context)
  "Load CONTEXT into emacs, as well as any others that you have loaded."
  (interactive
   (let ((context (context-list-mode-context-at-point)))
     (if context
	 (list context)
       (list (completing-read context-load-prompt
			      context-contexts
			      nil
			      t
			      nil)))))
  (context-message "Adding context %s" context)
  (context-mark-loaded context);; temporary, until all old contexts are contected
  (load-file (context-file-name context)))

(defun context-unload (context)
  "Unload CONTEXT."
  (interactive
   (let ((context (context-list-mode-context-at-point)))
     (if context
	 (list context)
       (list (completing-read "Unload context: "
			      contexts-loaded
			      nil
			      t
			      nil)))))
  (context-message "Unloading context %s" context)
  (let ((files-of-context (car (cdr (cdr (assoc context contexts-loaded))))))
    (mapcar (function (lambda (file)
			(let ((buf (get-file-buffer file)))
			  (if buf (kill-buffer buf)))))
	    files-of-context))
  (context-mark-unloaded context))

(defvar context-keep-control-buffers nil
  "Whether to keep the context control buffers around")

(defun context-save (trytochange)
  "Save the current context."
  (interactive (list t))
  (context-message "Saving current context (%s)" current-context)
  (let ((buffers-of-context (context-list-buffers-in-directories)))
    (setq context-files
	  (mapcar 'buffer-file-name
		  buffers-of-context))
    (save-window-excursion
      (find-file (context-file-name current-context))
      (erase-buffer)
      (require 'savebuffer)
      (insert (format ";;;; context %s\n;;; machine %s\n;;; time %s\n;;; user %s\n\n"
		      current-context
		      (system-name)
		      (current-time-string)
		      (user-full-name)))
      (insert "(require 'restorebuffer)\n")
      (insert (format "(context-mark-loaded \"%s\"\n '("
		      current-context))
      (mapcar (function (lambda (file)
			  (insert (format "   %s\n" (prin1-to-string (unsubstitute-in-file-name file))))))
	      context-files)
      (insert  "))\n")
      (context-message "  Saving context buffers: %s" buffers-of-context)
      (mapcar 'save-one-buffer-configuration
	      buffers-of-context)
      (goto-char (point-max))
      (context-message "  Saving context annotation \"%s\"" context-annotation)
      (insert (format "\n(setq context-annotation %s)\n" (prin1-to-string context-annotation)))
      (goto-char (point-max))
      (insert "\n;;; end of context\n")
      (basic-save-buffer)
      (if context-keep-control-buffers
	  (bury-buffer (current-buffer))
      (kill-buffer (current-buffer)))
      )
    (if trytochange
	(progn
	  (setq current-context "")
	  (mapcar 'kill-buffer buffers-of-context)
	  (context-select-next)))))

(defun context-kill (trytochange)
  "Kill the current context. With non-nil argument, ask for one to which to switch."
  (interactive (list t))
  (context-message "killing current context(%s)" current-context)
  (mapcar (function
	   (lambda (buffer)
	     (context-message "  killing buffer %s" buffer)
	    (kill-buffer buffer)))
	  (context-list-buffers-in-directories))
  (context-message "Removed all buffers in context %s" current-context)
  (context-mark-unloaded current-context)
  (setq current-context "")
  (if trytochange (context-select-next))
  (context-update-mode-line))

(defun context-save-and-kill ()
  "Save the current context, and remove it."
  (interactive)
  (context-message "Saving and killing current context %s" current-context)
  (context-save nil)
  (context-kill nil))

(defun context-delete (context)
  "Delete CONTEXT."
  (interactive
   (let ((context (context-list-mode-context-at-point)))
     (if context
	 (list context)
       (list (completing-read "Context to delete: "
			      context-contexts
			      nil
			      t
			      nil)))))
  (delete-file context-file-name context)
  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! must also remove from contexts alist
  )

(defun context-create (context)
  "Create a context called CONTEXT, initially containing just the current buffer."
  (interactive "sName for new context: ")
  (if (assoc context context-contexts)
      (error "Context %s already exists!" context))
  (if (null (buffer-file-name))
      (error "Context must be started from a file-visiting buffer"))
  (if (not (string= current-context ""))
      (let ((buffers-of-context (context-list-buffers-in-directories)))
	(context-save nil)
	(mapcar 'kill-buffer buffers-of-context)))
  (context-mark-loaded context)
  (setq current-context context
	context-files (list (buffer-file-name))
	context-contexts (cons
			  (cons context (context-file-name context))
			  context-contexts)))

(defun context-annotate ()
  "Annotate the current context"
  (interactive)
  (save-window-excursion
    (let ((annotation-buffer (get-buffer-create "*Context annotation*")))
      (switch-to-buffer annotation-buffer)
      (erase-buffer)
      (insert context-annotation)
      (context-message (substitute-command-keys
		"\\[exit-recursive-edit] when you have finished with annotation"))
      (recursive-edit)
      (set-buffer annotation-buffer)
      (setq context-annotation (buffer-string))
      (bury-buffer))))

(defun context-add-buffer ()
  "Add the current buffer to the current context."
  (interactive)
  (if (null (buffer-file-name))
      (error "Only file-visiting buffers may be added to contexts."))
  (context-message "Adding buffer %s to current context (%s)" (buffer-name) current-context)
  (if (not (member (buffer-file-name) context-files))
      (setq context-files (cons (buffer-file-name) context-files))))

(defun context-just-this-context ()
  "Kill all buffers belonging to observed contexts other than the current one."
  (interactive)
  (context-message "Removing buffers for all contexts except current (%s)" current-context)
  (let ((dirs nil))
    (dolist (observed contexts-observed)
      (unless (string= (car observed) current-context)
	(dolist (dir (cdr observed))
	  (pushnew (file-truename dir) dirs :test 'string=))))
    (context-message "Will remove buffers for directories:")
    (dolist (dir dirs) (context-message "    %S" dir))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer)))
	(when file
	  (setq file (file-truename file))
	  (context-message " Considering removing buffer for file %s" file)
	  (when (member (file-name-directory file) dirs)
	    (context-message "  Removing %s" buffer)
	    (kill-buffer buffer)))))))

(defun context-list-buffers-in-directories ()
  "Return all the buffers visiting files in directories which have any
file visited by a buffer in the current context."
  (let ((dirs (context-list-directories-in-buffers))
	(buffers nil))
    (mapcar
     (function
      (lambda (buffer)
	(let ((bfn (buffer-file-name buffer)))
	  (if (and bfn
		   (member (file-name-directory bfn)
			   dirs))
	      (setq buffers (cons buffer buffers))))))
     (buffer-list))
    (context-message "Buffers in directories of current context %s are %S" current-context buffers)
    buffers))

(defun context-list-directories-in-buffers ()
  "Return all the directories which have any file visited by a buffer in
the current context."
  (let ((dirs nil))
    (mapcar
     (function
      (lambda (visited-filename)
	(let ((dir (file-name-directory visited-filename)))
	  (if (not (member dir dirs))
	      (setq dirs (cons dir dirs))))))
     context-files)
    ;; (context-message "directories for current context %s are %S" current-context dirs)
    dirs))

(defun context-examine ()
  "Display the current context"
  (interactive)
  (with-output-to-temp-buffer "*Context*"
    (princ (format "Current context is %s\n" current-context))
    (let ((files context-files)
	  (buffers (context-list-buffers-in-directories))
	  (contexts contexts-loaded))
      (princ "\nFiles in this context:\n")
      (while files
	(princ (car files)) (princ "\n")
	(setq files (cdr files)))
      (princ "\nBuffers that will go into this context on saving context:\n")
      (while buffers
	(princ (car buffers)) (princ "\n")
	(setq buffers (cdr buffers)))
      (princ "\nAll loaded contexts:\n")
      (while contexts
	(princ (caar contexts)) (princ "\n")
	(setq contexts (cdr contexts))))))

(defun context-quitbuffer ()
  "Quit the context buffer."
  (interactive)
  (if (eq major-mode 'context-list)
      (kill-buffer nil)
    (error "not in context list buffer")))

(defvar context-keymap (make-keymap) "Keys for context stuff.")
(fset 'context-keymap context-keymap)
(suppress-keymap context-keymap)

(defun context-quickhelp ()
  "Quick help on context commands."
  (interactive)
  (with-output-to-temp-buffer "*Help"
    (princ (substitute-command-keys
"Context commands are:
\\{context-keymap}"))))

(define-key context-keymap "l" 'context-load)
(define-key context-keymap "L" 'context-make-list-buffer)
(define-key context-keymap "s" 'context-save)
(define-key context-keymap "n" 'context-create)
(define-key context-keymap "k" 'context-save-and-kill)
(define-key context-keymap "K" 'context-kill)
(define-key context-keymap "d" 'context-delete)
(define-key context-keymap "b" 'context-add-buffer)
(define-key context-keymap "+" 'context-add)
(define-key context-keymap "-" 'context-unload)
(define-key context-keymap "1" 'context-just-this-context)
(define-key context-keymap "x" 'context-examine)
(define-key context-keymap "?" 'context-quickhelp)
(define-key context-keymap "a" 'context-annotate)

(defvar context-list-mode-keymap (copy-keymap context-keymap)
  "Keymap for context list mode.")

(define-key context-list-mode-keymap "q" 'context-quitbuffer)
(define-key context-list-mode-keymap "n" 'next-line) ; overrides context-create
(define-key context-list-mode-keymap "p" 'previous-line)

(if (null (key-binding "\e&")) (define-key esc-map "&" context-keymap))

;;;; nasty hackery to revise context files

(defun context-revise-context-files ()
  "Revise the context files"
  (interactive)
  (mapcar 'context-load
	  (mapcar 'car context-contexts)))

;;; end of contexts.el
