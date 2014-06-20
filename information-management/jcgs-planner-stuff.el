;;; Time-stamp: <2008-01-06 14:54:14 jcgs>

(defvar planner-buffer-for-latest-task nil
  "Which planner buffer we most recently started a task in using planner-dwim.")

(defvar planner-current-task-overlay nil)

(defvar planner-task-before-typing-break nil
  "The planner task current at the start of a typing break.")

(defvar planner-task-before-typing-break-short nil
  "Short version of the planner task current at the start of a typing break.")

(defvar planner-task-typing-break-string "Typing break"
  "String describing a typing break, to be used in place of a planner task name.")

(defun jcgs-planner-pre-publish-function (project)
  "Pre-hook for publishing my planner pages."
  (message "in jcgs-planner-pre-publish-function(%S)" project))

(defun planner-dwim ()
  "If in a planner buffer, clock out of a task if clocked into it, otherwise clock in to it.
Otherwise, create a planner task."
  (interactive)
  (cond
   ((eq major-mode 'planner-mode)
    (let* ((task-info (planner-current-task-info))
	   (that-task (planner-task-description task-info)))
      (when task-info
	(if (equal planner-timeclock-current-task that-task)
	    (progn
	      (timeclock-out)
	      (setq planner-buffer-for-latest-task nil)
	      (message "Clocked out of %s" that-task))
	  (planner-task-in-progress)
	  (setq planner-buffer-for-latest-task (current-buffer))
	  (unless (eq (next-window)
		      (get-buffer-window (current-buffer)))
	    (delete-window))
	  (message "Clocked in to %s" that-task)))))
   (t
    (call-interactively 'planner-create-task-from-buffer))))

(defun planner-highlight-current-task (old-status new-status)
  "Highlight the current task."
  (cond
   ((string= new-status "o")
    (if (overlayp planner-current-task-overlay)
	(move-overlay planner-current-task-overlay
		      (planner-line-beginning-position)
		      (planner-line-end-position)
		      (current-buffer))
      (setq planner-current-task-overlay
	    (make-overlay (planner-line-beginning-position)
			  (planner-line-end-position)
			  (current-buffer)))
      (overlay-put planner-current-task-overlay
		   'face (cons 'background-color "yellow"))))
   ((null planner-timeclock-current-task)
    (when (overlayp planner-current-task-overlay)
      (delete-overlay planner-current-task-overlay))
    (setq planner-current-task-overlay nil)))
  t)

(defvar planner-task-stack-current-task nil
  "The latest task to be opened.")

(defun planner-push-task (task)
  "Enter and start a new TASK, remembering the old one."
  (interactive "sTask: ")
  (let* ((that-task (and planner-task-stack-current-task
		     (planner-task-description planner-task-stack-current-task)))
	 (that-id (if (stringp that-task)
		      (planner-id-get-id-from-string that-task)
		    nil)))
    (when that-id
      (message "planner-push-task: that-id %S" that-id))))

(defun planner-maybe-pop-task (old-status new-status)
  "Possibly pop to a previous task.
Argument OLD-STATUS is the old status.
Argument NEW-STATUS is the new status."
  (if (string= new-status "o")
      (setq planner-task-stack-current-task (planner-current-task-info))
    (setq planner-task-stack-current-task nil)
    (when (string= new-status "X")
      (let* ((task-info (planner-current-task-info))
	     (that-task (planner-task-description task-info)))
	(if task-info
	    (when (string-match "started from \\([0-9]+\\)" that-task)
	      (let ((parent-task (string-to-number (match-string-no-properties 1))))
		(message "Parent task %d" parent-task)
		;; perhaps use something like (planner-id-jump-to-linked-task (saved planner-task-stack-current-task))
		))
	  (message "no task info")))))
  t)

(add-hook 'planner-mark-task-hook 'planner-maybe-pop-task)

(defun planner-start-typing-break ()
  "Planner actions for the start of a typing break."
  (setq planner-task-before-typing-break planner-timeclock-current-task
	planner-task-before-typing-break-short planner-timeclock-current-task-short
	planner-timeclock-current-task planner-task-typing-break-string)
  (when (and (consp timeclock-last-event)
	     (not (equal (downcase (car timeclock-last-event)) "o")))
    (timeclock-out))
  (timeclock-in nil planner-task-typing-break-string nil))

(defun planner-end-typing-break ()
  "Planner actions for the end of a typing break."
  (when timeclock-last-event
    (timeclock-out))
  (when planner-task-before-typing-break
    (timeclock-in nil planner-task-before-typing-break nil)
    (setq planner-timeclock-current-task planner-task-before-typing-break
	  planner-timeclock-current-task-short planner-task-before-typing-break-short)))

(defun planner-show-current-task ()
  "Show my current task."
  (interactive)
  (if planner-timeclock-current-task
      (message "%s" planner-timeclock-current-task)
    (message "No current task")))

(defun add-to-end-of-mode-line (thing)
  (let ((rest mode-line-format))
    (while (cddr rest)
      (setq rest (cdr rest)))
    (rplacd rest (cons thing (cdr rest)))
    (force-mode-line-update)))

(defun jcgs-planner-key-help ()
  "Show the bindings under my planner key."
  (interactive)
  (message "%s" (substitute-command-keys "\\{jcgs-planner-map}")))

(defun planner-goto-today-and-current-task ()
  "Jump to the planning page for the open task, or to today if no task open, and if a task is open, put point on it."
  (interactive)
  (if planner-buffer-for-latest-task
      (switch-to-buffer-other-window planner-buffer-for-latest-task)
    (planner-goto-today))
  (when planner-timeclock-current-task
    (goto-char (point-min))
    (search-forward planner-timeclock-current-task (point-max) t)
    (beginning-of-line)))

(defun bury-buffer-if-planner (&optional buffer)
  "If the current buffer (or BUFFER if given) is visiting a planner file, bury it."
  (when (and planner-bury-buffers
	     (eq major-mode 'planner-mode))
    (bury-buffer buffer)))

(defun switch-to-buffer-burying-planner (buffer)
  (interactive
   (let* ((default (other-buffer))
	  (string (completing-read (format "Switch to buffer: (default %S) " default)
				   (mapcar 'list (buffer-list)))))
     (list (if (string= string "")
	       default
	     string))))
  (when (and planner-bury-buffers
	     (eq major-mode 'planner-mode)
	     (with-current-buffer buffer
	       (not (eq major-mode 'planner-mode))))
    (bury-buffer))
  (switch-to-buffer buffer))

(defun bury-previous-buffer-if-planner (previous-buffer)
  "Function to go on buffer-selection-hook to keep planner buffers out of the way."
  (when (and planner-bury-buffers
	     previous-buffer
	     (buffer-file-name)	      ; don't do it for minibuffer etc
	     (not (eq major-mode 'planner-mode)) ; don't do it when switching between planner buffers
	     (with-current-buffer previous-buffer
	       (eq major-mode 'planner-mode)))
    (message "switching from planner buffer %S to non-planner buffer %S; burying planner buffer"
	     previous-buffer (current-buffer))
    (bury-buffer previous-buffer)))

(defun start-deleting-whitespace-on-save ()
  "Put delete-trailing-whitespace onto local-write-file-hooks."
  (add-hook 'local-write-file-hooks
	    (lambda ()
	      (delete-trailing-whitespace)
	      nil)))

(defcustom planner-bury-buffers nil
	       "Whether to put buffers at the bottom of the list when done with them."
	       :type 'boolean
	       :group 'planner)

(provide 'jcgs-planner-stuff)

;;; end of jcgs-planner-stuff