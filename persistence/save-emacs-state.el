;;; Time-stamp: <2013-10-15 12:22:07 johnstu>

(provide 'save-emacs-state)
(require 'load-directory)

;;;; Things to be done:
;;; multiple contexts

(defvar emacs-save-restorers 
  (substitute-in-file-name "$COMMON/var/emacsrestore")
  "The directory to write restorer files in.
This variable should be used by saver modules to determine where to
write their state files into.")

(defun save-emacs-context ()
  "Save your emacs context.
First run the saver files in $/EMACS/savers1/*.el then those in
$EMACS/savers2/*.el. savers1 should save specialized things such as
RMAIL buffers, that the relevant generalized saver would get wrong,
and savers2 should do generic saves such as ordinary file buffers."
  (interactive)
  (mapcar 'load-directory
	  (mapcar 'find-subdirectory-from-path
		  '("savers1"
		    "savers2"))))

(if (not (boundp 'ok-to-reload))
    (setq ok-to-reload t))

(defun restore-emacs-context ()
  "Restore your emacs context."
  (interactive)
  (if ok-to-reload
      (load-directory emacs-save-restorers)))

(defun exit-saving-buffers ()
  "Exit emacs, saving buffers first."
  (interactive)
  (if (yes-or-no-p "Really quit emacs? ")
      (progn
	(if (fboundp 'type-break-mode) (type-break-mode -1))
	(set-buffer "*Messages*")
	(erase-buffer)
	(message "Shutting down at %s" (current-time-string))
	(setq message-log-max t)
	(save-emacs-context)
	(message "Shut down nearly complete at %s" (current-time-string))
	(set-buffer "*Messages*")
	(write-file "~/.shutdownmessages")
	(message "You bulk-loaded %d elisp files (%d bytes) this session"
		 (length load-directory-loaded) load-directory-bytes)
	(sit-for 1)
	(save-buffers-kill-emacs))))

(defun save-state-insert-header ()
  "Insert a suitable header."
  (insert ";;; Part of saved emacs state for " (user-real-login-name) "@" (system-name)
	  " saved at " (current-time-string) "\n"))

(defun save-list-to-file (file listvar fn &rest save-extra-vars)
  "Save, into FILE in emacs-save-restorers, the list in the variable LISTVAR. Use FN to insert each element.
Optional extra arguments SAVE-EXTRA-VARS name extra variables to save."
  (save-window-excursion
    (let* ((filename (expand-file-name file emacs-save-restorers))
	   (already (find-buffer-visiting filename)))
      (find-file filename)
      (erase-buffer)
      (save-state-insert-header)
      (let ((name (symbol-name listvar)))
	(message "Saving %s" name)
	(insert (format "(setq %s '(\n" name))
	(mapcar fn (symbol-value listvar))
	(insert ")\n")
	(while save-extra-vars
	  (when (boundp (car save-extra-vars))
	    (insert (format "  %s %S\n"
			    (car save-extra-vars)
			    (eval (car save-extra-vars)))))
	  (setq save-extra-vars (cdr save-extra-vars)))
	(insert ")\n")
	(basic-save-buffer)
	(if (not already)
	    (kill-buffer (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'old-make-backup-file-name (symbol-function 'make-backup-file-name))

(defun make-backup-file-name (fn)
  "John's customized make-backup-file-name.
Handles emacsrestore files specially, otherwise calls the
previous definition of this function."
  (cond ((string-match "emacsrestore" fn)
	 (expand-file-name (file-name-nondirectory fn)
			   (expand-file-name "prev"
					     (file-name-directory fn))))
	(t (old-make-backup-file-name fn))))

(defun as-plain-string (thing default)
  "Return a readable string representing THING or DEFAULT if no readable representation"
  (let ((str (format "  %S\n" thing)))
    (if (string-match "#<" str)
	default
      (remove-text-properties
       0 (length thing)
       '(face t)
       str)
      str)))

;;; end of save-emacs-state.el
