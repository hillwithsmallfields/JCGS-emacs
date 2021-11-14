;;; Time-stamp: <2021-11-14 18:31:28 jcgs>
;;; John's configuration stuff for vm (View Mail)

(provide 'setup-vm)

(message "In $SYNCED/emacs/requirable-setups/setup-vm.el, path is %S" load-path)

(when (and (boundp 'vm-host) vm-host)
  (let* ((load-vm-version 
	  (if t "7.19" "6.79")
	  )
	 (vm-dir (substitute-in-file-name
		  (format "$GATHERED/emacs/vm/vm-%s"
			  load-vm-version))))
    (add-lispdir vm-dir)
    (load-file (expand-file-name "vm.el" vm-dir)))

  (add-lispdir (expand-file-name "my-extensions-to-packages/vm/" user-emacs-directory))

  (require 'vm-delete-as-spam)

  (setq vm-mutable-frames nil
	bbdb-completion-type 'primary-or-name
	read-mail-command 'vm)

  (defvar john-message-dates nil
    "The message date headers I've received.")

  (defvar john-vm-was-in-buffer nil
    "Hack for debugging -- it gets lots of /usr/users/ldisk5/john/vm/john.")

  (defvar message-date-counts nil
    "Alist of message dates to number of messages received on that day.")

  (defun tally-message-date (date &optional how-many)
    "Count that a message was sent to me on DATE. Optionally HOW-MANY."
    (let ((pair (assoc date message-date-counts)))
      (if (null pair)
	  (setq pair (cons date 0)
		message-date-counts (cons pair message-date-counts)))
      (rplacd pair (+ (if how-many how-many 1)
		      (cdr pair)))))

  (defun display-message-date-tallies ()
    "Display message date tallies."
    (interactive)
    (with-output-to-temp-buffer "*message dates*"
      (princ (format "An average of %s messages are sent to you each day\n"
		     (/ (apply '+ (mapcar 'cdr message-date-counts))
			(length message-date-counts))))
      (mapcar '(lambda (date-pair)
		 (princ (car date-pair))
		 (princ ": ")
		 (princ (cdr date-pair))
		 (princ "\n"))
	      message-date-counts)))

  ;; part of stuff for tidying up dates that have appeared in more than one format
  (defun date-counts-collect-bunches (biglist)
    "Return BIGLIST processed to collect up bunches of dates."
    (mapcar (lambda (entry)
	      (if (stringp (car entry))
		  entry
		(cons (caar entry)
		      (apply '+ (mapcar 'cdr entry)))))
	    biglist))

  (defun strip-date (date)
    "Cut a message date string down to just the date."
    (if (string-match "\\([ 0-9][0-9] [JFMASOND][a-z][a-z] [0-9]?[0-9]?[0-9][0-9]\\)" date)
	(substring date (match-beginning 1) (match-end 1))
      date))

  (defvar last-seen-date nil
    "The date seen most recently in freshly arrived messages -- used for
any subsequent messages which do not give an identifiable date of
their own.")

  (defun john-vm-arrived-message-hook-fun ()
    "Process a newly-arrived message.
This tallies messages by their arrival date in a form which can be
displayed by \[display-message-date-tallies\]."
    ;; (setq john-vm-was-in-buffer (cons buffer-file-name john-vm-was-in-buffer))
    (if (string= buffer-file-name vm-primary-inbox)
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward "^Date: \\(.+\\)$" (point-max) t)
	      (let ((the-date (strip-date
			       (buffer-substring (match-beginning 1)
						 (match-end 1)))))
		(setq last-seen-date the-date)
		(tally-message-date the-date)
		(setq john-message-dates
		      (cons the-date
			    john-message-dates)))
	    (if last-seen-date (tally-message-date last-seen-date))))))


;;;; next marked message

  (defun vm-next-marked-message (&optional count retry signal-errors)
    "Like vm-next-message but applies to marked messages only."
    (interactive "p\np\np")
    (let ((last-command 'vm-next-command-uses-marks))
      (vm-next-message count retry signal-errors)))

;;;; address munging

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

  (if (and (boundp 'vm-host) vm-host)
      (require 'vm-save))

  (defun vm-auto-select-folder (mp auto-folder-alist)
    (message "In vm-auto-select-folder")
    (let ((result
	   (condition-case error-data
	       (catch 'match
		 (let (header alist tuple-list)
		   (setq alist auto-folder-alist)
		   (while alist
		     (setq header (vm-get-header-contents (car mp) (car (car alist))))
		     (if (null header)
			 ()
		       ;; (message "  Looking at outer %S for header %S" (car alist) header)
		       (setq tuple-list (cdr (car alist)))
		       (while tuple-list
			 ;; (message "    Looking at inner %S" (car tuple-list))
			 (if (let ((case-fold-search vm-auto-folder-case-fold-search))
			       (string-match (car (car tuple-list)) header))
			     ;; Don't waste time eval'ing an atom.
			     (if (atom (cdr (car tuple-list)))
				 (progn
				   (message "      matched atom %S from %S" (cdr (car tuple-list)) tuple-list)
				   (throw 'match (cdr (car tuple-list))))
			       (let* ((match-data (vm-match-data))
				      (vm-current-mail-buffer (or (and (boundp 'vm-current-mail-buffer)
								       vm-current-mail-buffer)
								  ;;vm-mail-buffer
								  (current-buffer)
								  ))
				      ;; allow this buffer to live forever
				      (buf (get-buffer-create " *vm-auto-folder*"))
				      (result))
				 ;; Set up a buffer that matches our cached
				 ;; match data.
				 (save-excursion
				   (set-buffer buf)
				   (widen)
				   (erase-buffer)
				   (insert header)
				   ;; It appears that get-buffer-create clobbers the
				   ;; match-data.
				   ;;
				   ;; The match data is off by one because we matched
				   ;; a string and Emacs indexes strings from 0 and
				   ;; buffers from 1.
				   ;;
				   ;; Also store-match-data only accepts MARKERS!!
				   ;; AUGHGHGH!!
				   (store-match-data
				    (mapcar
				     (function (lambda (n) (and n (vm-marker n))))
				     (mapcar
				      (function (lambda (n) (and n (1+ n))))
				      match-data)))
				   (setq result (eval (cdr (car tuple-list))))
				   (while (consp result)
				     (setq result (vm-auto-select-folder mp result)))
				   (if result
				       (progn
					 (message "      Throwing result %S" result)
					 (throw 'match result)))))))
			 (setq tuple-list (cdr tuple-list))))
		     (setq alist (cdr alist)))
		   nil ))
	     (error (error "error processing vm-auto-folder-alist: %s"
			   (prin1-to-string error-data))))))
      (message "vm-auto-select-folder returning %S" result)
      result))

  (defvar jvm-current-context "new/"
    "The context of the current folder, that is, which directory to save things to.
Defaults to \"new\" but may be set differently in folders in different directories.")

  (make-variable-buffer-local 'jvm-current-context)

  (defun jvm-directory-to-context (folder-file)
    "Give the directory in which to save messages from FOLDER-FILE."
    (cond
     ((string-match "vm/new/" folder-file) (setq jvm-current-context "hold/"))
     ((string-match "vm/hold/" folder-file) (setq jvm-current-context "keep/"))
     (t nil)))

  (defun jvm-set-folder-context ()
    "John's vm sorting setup hook."
    (let ((bfn (buffer-file-name)))
      (when bfn (jvm-directory-to-context bfn))))

  (defun jvm-set-folder-summary-context ()
    "John's vm summary sorting setup hook."
    (define-key vm-mode-map [f4] 'vm-next-marked-message)
    (let ((bfn (buffer-file-name vm-mail-buffer)))
      (when bfn (jvm-directory-to-context bfn))))

  (add-hook 'vm-visit-folder-hook 'jvm-set-folder-context)
  (add-hook 'vm-summary-mode-hook 'jvm-set-folder-summary-context)

  (setq jvm-last-mail-buffers nil)

  (defun jvm-folder-in-context (foldername)
    "Return FOLDERNAME in the appropriate context, such as new or keep."
    (setq jvm-last-contexter-buffer (current-buffer)
	  jvm-last-contexter-context jvm-current-context
	  )
    (push   vm-current-mail-buffer jvm-last-mail-buffers)
    (concat jvm-current-context foldername))

  ;; do stuff with vm-mime-reader-map-save-file

  (if (and (boundp 'vm-host) vm-host)
      (setq vm-folder-directory
	    ;; "/usr/users/ldisk/john/vm/"
	    "~/vm/"
	    vm-primary-inbox (expand-file-name "john" vm-folder-directory)
	    my-summary-name (concat (file-name-nondirectory vm-primary-inbox) " Summary")
	    vm-spool-files '(
			     ;; "pop.humph.com:110:pass:jcgs_cb1.com:*"
			     "imap:staffexchange1.ul.ie:143:inbox:login:john.sturdy:*"
					; "imap:exch-staff4.ul.ie:143:inbox:login:john.sturdy:*"
			     "/var/spool/mail/sturdyj"
			     "/var/mail/jcgs"
			     )
	    vm-inhibit-startup-message t
	    vm-circular-folders nil
	    vm-preview-lines nil
	    vm-auto-next-message nil
	    vm-confirm-new-folders t
	    vm-visit-when-saving 0
	    vm-delete-after-saving t
	    vm-delete-after-archiving t
	    vm-auto-folder-case-fold-search t
	    vm-keep-sent-messages t
	    vm-follow-summary-cursor t
	    vm-auto-decode-mime-messages t
	    vm-delete-after-bursting t
	    vm-visible-headers (if (member "X-Warning-Local" vm-visible-headers)
				   vm-visible-headers
				 (cons "X-Warning-Local" vm-visible-headers))
	    ;; vm-auto-displayed-mime-content-types ...something...
	    ;; vm-mime-type-converter-alist ...something...
	    vm-summary-format
	    (let* ((frame-width
		    (if nil
			(frame-width (selected-frame))
		      102))
		   (right-width (- frame-width 60))
		   (a-half (/ right-width 2)))
	      (message "vm widths: frame-width=%d right-width=%d a-half=%d" frame-width right-width a-half)
	      (concat
	       ;; "%n %*%a %-20.20UB %-7.7UT %2.2M/%2d %5.5c %I"
	       "%n %*%a %-20.20UF %-7.7UT %2.2M/%2d %5.5c %I"
	       (if nil (format "%%%d.%ds %%%d.%dL\n"
			       (- a-half) a-half a-half a-half)
		 (format "%%%d.%ds %%%d.%dL\n"
			 (- right-width) right-width a-half a-half))))
	    vm-frame-per-folder nil
	    vm-frame-per-composition nil
	    vm-move-after-deleting t
	    vm-move-after-undeleting nil
	    vm-group-by "subject" ;; "physical-order"
	    ))

  (when
      ;; at least one spool folder is a network host that can't be reached
      (eval				; can't apply a subr
       (cons 'or
	     (mapcar (lambda (spool)
		       (and
			(string-match "^imap:\\([^:]+\\):" spool)
			(not (host-is-reachable (match-string 1 spool)))))
		     vm-spool-files)))
    (setq vm-host nil))

  (require 'jcgs-vm-auto-folder-alist)

  (defun vm-config-copy-from-to-to ()
    (interactive)
    (let ((pair (assoc "To" vm-auto-folder-alist)))
      (when (null pair)
	(setq pair (cons "To" nil))
	(setq vm-auto-folder-alist (cons pair vm-auto-folder-alist)))
      (rplacd pair (cdr (assoc "From" vm-auto-folder-alist)))))


  (add-hook 'vm-arrived-message-hook 'john-vm-arrived-message-hook-fun)

  (defun vm-run-on-all-messages (function)
    "Run FUNCTION on all messages in this buffer.
FUNCTION is called with no arguments, with the message between (point-min)
and (point-max)."
    (vm-goto-message 1)
    (vm-run-on-following-messages function))

  (defun vm-run-on-following-messages (function)
    "Run FUNCTION on following messages in this buffer.
FUNCTION is called with no arguments, with the message between (point-min)
and (point-max)."
    (condition-case what
	(while t
	  (vm-next-message 1 nil t)
	  (funcall function))
      (end-of-folder (message "reached end of folder"))))

  ;; (require 'discussion)
  ;; (add-hook 'vm-arrived-message-hook 'discussion-process-whole-message-in-buffer)

  (if (and
       (boundp 'vm-host)
       vm-host
       (not (file-directory-p vm-folder-directory)))
      (setq vm-folder-directory (expand-file-name "~/vm/")))

  ;; (autoload 'vm "vm-startup" "The VM system." t)

  ;; (require 'mail-is-up)
  ;; (require 'cite-discussion)
  ;; (cite-discussion:setup-vm-keys)

  ;; (require 'jcgs-patch-vm-digest)

  ;; (require 'bbdb-config)
  ;; (bbdb-insinuate-vm)

  (defvar my-summary-name nil
    "The buffer containing my mailbox summary.")

  (defun vm-maybe-quit ()
    "I don't usually really want to quit it!"
    (interactive)
    (if (y-or-n-p "Quit vm (otherwise just leave the buffer)? ")
	(vm-quit)
      (exit-recursive-edit)))

  (when (boundp 'vm-mode-map)
    (define-key vm-mode-map "q" 'vm-maybe-quit)
    (define-key vm-summary-mode-map "q" 'vm-maybe-quit))
  )

(defun js-vm-get-mail ()
  "Get my mail, using vm.
This saves the window configuration and puts you in a recursive edit, so
do \\[exit-recursive-edit] to return to whatever you were doing before.
Also shows diary and to-do list."
  (interactive)
  (save-window-excursion
    (switch-to-buffer my-summary-name)
    (delete-other-windows)
    (vm-get-new-mail)
    (split-window)
    (diary)
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


;;; end of mode-setups/vm.el

