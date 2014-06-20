;;; Time-stamp: <2013-01-15 16:15:42 johnstu>

(defun make-local-hook (&rest args)
  "Obsolete with ARGS?"
 (apply 'make-local-variable args))

(defun js-vm-get-mail ()
  "Get my mail, using vm.
This saves the window configuration and puts you in a recursive edit, so
do \\[exit-recursive-edit] to return to whatever you were doing before.
Also shows diary and to-do list."
  (interactive)
  (save-window-excursion
    (if t
	(vm)
      (switch-to-buffer my-summary-name)
      (delete-other-windows)
      (message "in %S" (current-buffer))
      (vm-get-new-mail))
    ;; (split-window)
    ;; (diary)
    (let* ((diary-buffer (get-buffer "*Fancy Diary Entries*"))
	   (diary-window (and (bufferp diary-buffer)
			      (get-buffer-window diary-buffer))))
      (when (windowp diary-window)
	(select-window diary-window)
	;; (split-window-horizontally)
	(split-window)
	(select-window diary-window)
	(shrink-window-if-larger-than-buffer)
	(other-window 2)
	(plan)
	(while (not (equal (buffer-name) my-summary-name))
	  (other-window 1))))
    ;; 
    ;; (let ((summary-window (get-buffer-window (get-buffer my-summary-name))))
    ;;       (when (fboundp 'diary)
    ;; 	(diary)
    ;;       ;;(find-file todo-file-top)

    ;;       (when nil
    ;; 	(if nil
    ;; 	    (plan)
    ;; 	  (switch-to-buffer (get-buffer-create "*Things to do*"))
    ;; 	  (erase-buffer)
    ;; 	  (insert-file-contents todo-file-top)
    ;; 	  (goto-char (point-min))
    ;; 	  (while (re-search-forward "^.+: " (point-max) t)
    ;; 	    (replace-match ""))
    ;; 	  (goto-char (point-min))
    ;; 	  (delete-matching-lines "---")
    ;; 	  (goto-char (point-min))
    ;; 	  (delete-blank-lines)
    ;; 	  ))
    ;;       (when nil
    ;; 	(if (windowp summary-window)
    ;; 	    (select-window summary-window)
    ;; 	  (switch-to-buffer-other-window (get-buffer my-summary-name))))
    ;;       (when (and (not jcgs-has-run-feedmail-queue)
    ;; 		 (not jcgs-use-feedmail)
    ;; 		 (yes-or-no-p "Run outgoing feedmail queue? "))
    ;; 	(feedmail-run-the-queue)
    ;; 	(setq jcgs-has-run-feedmail-queue t)))
    (message (substitute-command-keys "\\[exit-recursive-edit] to return to previous display"))
    (recursive-edit))
  (if (fboundp 'sink-sinkables) (sink-sinkables)))

(defun vm-munge-address-for-tidy-display (address)
  "Strip various bits off ADDRESS."
  (cond
   ((not (stringp address))
    address)
   ((string-match "^Multiple recipients of " address)
    (substring address 25 nil))
   ((string-match "^anon[. ]?" address)
    (substring address 5 nil))
   (t address)))

(defun vm-summary-function-T (msg-struct)
  "Return who a message was To, allowing for various stuff"
  (let ((to (vm-to-of msg-struct)))
    (if (null to)
	(save-excursion
	  (goto-char (point-min))
	  (setq to
		(if (re-search-forward "^To: \\(.+\\)$" (point-max) t)
		    (buffer-substring (match-beginning 1) (match-end 1))
		  "unknown"))
	  ))
    (let ((new-to 
	   (vm-munge-address-for-tidy-display to)))
      (save-excursion (set-buffer (get-buffer-create "*vm header munging log*"))
		      (goto-char (point-max))
		      (insert "Munged \""
			      to
			      "\" into \""
			      new-to
			      "\"\n"))
      new-to)))

(defun vm-summary-function-F (msg-struct)
  "Return who a message was From, allowing for various stuff"
  (let ((to (vm-from-of msg-struct)))
    (if (null to)
	(save-excursion
	  (goto-char (point-min))
	  (setq to
		(if (re-search-forward "^From: \\(.+\\)$" (point-max) t)
		    (buffer-substring (match-beginning 1) (match-end 1))
		  "unknown"))
	  ))
    (let ((new-to 
	   (vm-munge-address-for-tidy-display to)))
      (save-excursion (set-buffer (get-buffer-create "*vm header munging log*"))
		      (goto-char (point-max))
		      (insert "Munged \""
			      to
			      "\" into \""
			      new-to
			      "\"\n"))
      new-to)))

(defun vm-maybe-quit ()
    "I don't usually really want to quit it!"
    (interactive)
    (if (y-or-n-p "Quit vm (otherwise just leave the buffer)? ")
	(vm-quit)
      (exit-recursive-edit)))

(provide 'jcgs-vm-stuff)

;;; end of jcgs-vm-stuff.el
