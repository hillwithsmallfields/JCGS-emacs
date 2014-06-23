;;;; find, load and configure vm
;;; Time-stamp: <2014-06-23 11:52:23 johstu01>

(add-to-list 'load-path (expand-file-name "email" user-emacs-directory))

(use-package vm
	     ;; "$GATHERED/emacs/vm/vm-7.19"
	     ;; "$GATHERED/emacs/vm/vm-8.0.11"
	     "$GATHERED/emacs/vm/vm-8.1.1"
	     (nil			; use default getter function
	      ;; "http://www.seanet.com/~kylemonger/vm/vm-7.19.tar.gz"
	      ;; "http://mirrors.linhub.com/savannah/viewmail/vm-8.0.11-581.tgz"
	      "http://download.savannah.gnu.org/releases/viewmail/vm-8.1.1.tgz"
	      ;; (shell-command "cd vm-7.19; make")
	      (shell-command "cd vm-8.1.1; make")
	      )
	     ((expand-file-name "email" user-emacs-directory)
	      (require 'jcgs-vm-stuff)
	      ([ C-f11 ] . js-vm-get-mail)
	      (js-vm-get-mail "jcgs-vm-stuff" "Get my mail, using vm.
This saves the window configuration and puts you in a recursive edit, so
do \\[exit-recursive-edit] to return to whatever you were doing before.
Also shows diary and to-do list." t)
	      ([ M-C-f11 ] . vm-continue-composing-message)
	      (vm "vm" "Run the VM mail reader" t)
	      ("~/vm" . vm-mode)
	      ("$COMMON/vm" . vm-mode))
	     (setq vm-mutable-frames nil
		   bbdb-completion-type 'primary-or-name
		   read-mail-command 'vm
		   vm-folder-directory (if (file-directory-p
					    (expand-file-name "~/vm/"))
					   (expand-file-name "~/vm/")
					 (substitute-in-file-name "$COMMON/vm/"))
		   vm-primary-inbox (expand-file-name "john" vm-folder-directory)
		   my-summary-name (concat (file-name-nondirectory vm-primary-inbox) " Summary")
		   vm-imap-messages-per-session 128
		   vm-spool-files '( ;; "imap:staffexchange1.ul.ie:143:inbox:login:john.sturdy:*"
				    "imap:localhost:143:inbox:login:johnstu:*" ; via a stunnel daemon that James set up
				    ;; "imap-ssl:lonpmail.citrite.net:993:inbox:login:johnstu:*" ; new one
				    ;; "imap-ssl:lonpmailmx01.citrite.net:993:inbox:login:johnstu:*"
				    ;; "imap:lonpmailmx01.citrite.net:143:inbox:login:citrite\\johnstu:*"
				    "imap:localhost:11143:inbox:login:john.cb1.com:*"


				    ;; "imap-ssl:imap.gmail.com:993:inbox:jcg.sturdy@gmail.com:*"

				    ;; SPOOLNAME can also be an IMAP maildrop.

				    ;;     An IMAP maildrop specification has the following format:

				    ;;        "imap:HOST:PORT:MAILBOX:AUTH:USER:PASSWORD"
				    ;;     or
				    ;;        "imap-ssl:HOST:PORT:MAILBOX:AUTH:USER:PASSWORD"
				    ;;     or
				    ;;        "imap-ssh:HOST:PORT:MAILBOX:AUTH:USER:PASSWORD"

				    ;;     The second form is used to speak IMAP over an SSL connection.
				    ;;     You must have the stunnel program installed and the variable
				    ;;     `vm-stunnel-program' naming it in order for IMAP over SSL to
				    ;;     work.

				    ;;     The third form is used to speak IMAP over an SSH connection.
				    ;;     You must have the ssh program installed and the variable
				    ;;     `vm-ssh-program' must name it in order for IMAP over SSH to
				    ;;     work.  SSH must be able to authenticate without a password,
				    ;;     which means you must be using .shosts authentication or
				    ;;     public key user authentication.

				    ;;     HOST is the host name of the IMAP server.

				    ;;     PORT is the TCP port number to connect to.  This should
				    ;;     normally be 143.  For IMAP over SSL, the standard port is
				    ;;     993.  There is no special port for IMAP over SSH.

				    ;;     MAILBOX is the name of the mailbox on the IMAP server.  Should
				    ;;     be "inbox", to access your default IMAP maildrop on the
				    ;;     server.

				    ;;     AUTH is the authentication method used to convince the server
				    ;;     you should have access to the maildrop.  Acceptable values
				    ;;     are "preauth", "login" and "cram-md5".  "preauth"
				    ;;     causes VM to skip the authentication stage of the protocol
				    ;;     with the assumption that the session was authenticated in some
				    ;;     external way.  "login", tells VM to use the IMAP LOGIN
				    ;;     command for authentication, which sends your username and
				    ;;     password in cleartext to the server.  "cram-md5" is a
				    ;;     challenge response system that convinces the server of your
				    ;;     identity without transmitting your password in the clear.
				    ;;     Not all servers support "cram-md5"; if you're not sure, ask
				    ;;     your mail administrator or just try it.

				    ;;     USER is the user name used with authentication methods that
				    ;;     require such an identifier.  "login" and "cram-md5"
				    ;;     use it currently.

				    ;;     PASSWORD is the secret shared by you and the server for
				    ;;     authentication purposes.  If the PASSWORD is "*", VM
				    ;;     will prompt you for the password the first time you try to
				    ;;     retrieve mail from maildrop.  If the password is valid, VM
				    ;;     will not ask you for the password again during this Emacs
				    ;;     session.

				    "/var/spool/mail/sturdyj"
				    "/var/mail/jcgs"
				    )
		   vm-inhibit-startup-message t
		   vm-circular-folders nil
		   vm-preview-lines nil
		   vm-auto-next-message nil
		   vm-confirm-new-folders t
		   vm-visit-when-saving 0
		   vm-use-toolbar nil
		   vm-delete-after-saving t
		   vm-delete-after-archiving t
		   vm-auto-folder-case-fold-search t
		   vm-keep-sent-messages t
		   vm-follow-summary-cursor t
		   vm-auto-decode-mime-messages t
		   vm-delete-after-bursting t
		   vm-mail-header-from (cond
					((string-match "xci-test\\|citrix" (system-name))
					 "john.sturdy@arm.com")
					(t
					 "jcg.sturdy@gmail.com"))
		   vm-mail-check-interval 900
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
		   )

	     (load-file "$GATHERED/emacs/vm/vm-8.1.1/lisp/vm-autoload.el")

	     (when (boundp 'vm-visible-headers)
	       (setq vm-visible-headers (if (member "X-Warning-Local" vm-visible-headers)
					    vm-visible-headers
					  (cons "X-Warning-Local" vm-visible-headers))))

	     (define-key mail-mode-map "\C-j" 'mail-split-line)
	     (bbdb-insinuate-vm)

	     (when (boundp 'vm-mode-map)
	       (define-key vm-mode-map "q" 'vm-maybe-quit)
	       (define-key vm-summary-mode-map "q" 'vm-maybe-quit)))

(defun mail-split-line ()
  "Split the current line at point."
  (interactive)
  (let ((prefix (save-excursion
		  (beginning-of-line 1)
		  (if (looking-at mail-citation-prefix-regexp)
		      (match-string 0)
		    ""))))
    (insert "\n" prefix (make-string (- (current-column) (length prefix)) ? ))))

(defun vm-delete-rest ()
  "Delete from here to the end of the folder."
  (interactive)
  (let ((vm-move-after-deleting nil)
	(vm-circular-folders nil))
    (while (progn (vm-delete-message 1)
		  (condition-case nil
		      (progn
			(vm-move-message-pointer 'forward)
			(message "moved OK")
			t)
		    (end-of-folder
		     (message "got end")
		     nil)))
      )))

(defun diary-from-vm ()
  "Maybe snarf diary entry from Outlook-generated or other message in VM."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if vm-presentation-buffer
      (set-buffer vm-presentation-buffer)
    (error "No presentation buffer"))
  (unwind-protect
      (save-window-excursion
	(save-excursion
	  (let* ((subject (mail-fetch-field "subject"))
		 (begin (save-excursion (rfc822-goto-eoh) (point)))
		 (end (point-max))
		 (vcalendar-begin (save-excursion
				    (goto-char begin)
				    (and (re-search-forward "^BEGIN:VCALENDAR$" end t)
					 (match-beginning 0))))
		 (description-position (save-excursion
					 (goto-char vcalendar-begin)
					 (if (re-search-forward "^DESCRIPTION;\\(LANGUAGE=[^:]+:\\)?"
								(point-max)
								t)
					     (cons (point)
						   (if (re-search-forward "^[^ ]" (point-max) t)
						       (match-beginning 0)
						     nil))
					   nil)))
		 (description (if (and description-position
				       (cdr description-position))
				  (replace-regexp-in-string
				   "\\\\n"
				   "\n"
				   (replace-regexp-in-string
				    "\n "
				    ""
				    (buffer-substring-no-properties
				     (car description-position)
				     (cdr description-position))))
				""))
		 ;; todo: extract useful information from description, and add to body
		 (body (buffer-substring-no-properties
			(or vcalendar-begin begin)
			(if vcalendar-begin
			    (save-excursion
			      (goto-char vcalendar-begin)
			      (1+ (re-search-forward "^END:VCALENDAR$" end t)))
			  end))))
	    (message "Description is %S" description)
	    (save-excursion
	      (diary-from-vm-internal subject body)))))))

(add-hook 'vm-mode-hook
	  (function
	   (lambda ()
	     (define-key vm-summary-mode-map "y" 'diary-from-vm))))

(autoload 'diary-make-entry "diary-lib")

(defun diary-date-string (year month day)
  "Make a date string from YEAR MONTH DAY, according to `calendar-date-style'."
  (cond
   ((eq calendar-date-style 'iso)
    (format "%04d/%02d/%02d" year month day))
   ((eq calendar-date-style 'european)
    (format "%02d/%02d/%04d" month day year))
   ((eq calendar-date-style 'american)
    (format "%02d/%02d/%04d" month day year))))

(defun outlook-encode-date-time-string (date-time-string offset-string)
  "Convert an Outlook-style date and time string to Emacs internal time format."
  (let* ((year (string-to-number (substring date-time-string 0 4)))
	 (month (string-to-number (substring date-time-string 4 6)))
	 (day (string-to-number (substring date-time-string 6 8)))
	 (hours (string-to-number (substring date-time-string 9 11)))
	 (minutes (string-to-number (substring date-time-string 11 13)))
	 (seconds (string-to-number (substring date-time-string 13 15)))
	 (offset-sign (if (= (aref offset-string 0) ?+) 1 -1))
	 (offset-hours (string-to-number (substring offset-string 1 3)))
	 (offset-minutes (string-to-number (substring offset-string 3 5)))
	 (encoded-time (encode-time seconds minutes hours day month year)))
    (message "year=%S month=%S day=%S hours=%S minutes=%S seconds=%S offset-sign=%S offset-hours=%S offset-minutes=%S encoded=%S" year month day hours minutes seconds offset-sign offset-hours offset-minutes encoded-time)
    encoded-time))

(defun diary-make-entry-encoded (event start &optional end)
  "Make an entry for EVENT from START to END."
  (let* ((string (format "%s %s" (format-time-string "%Y/%m/%d %H:%M" start) event)))
    (diary-make-entry string)))

(defun diary-from-vm-internal (subject body &optional test-only)
  "Snarf a diary entry from a message assumed to be from MS Outlook.
SUBJECT is used to describe the appointment, and BODY contains the formatted
appointment.
Optional arg TEST-ONLY non-nil means return non-nil iff the message contains an
appointment, don't make a diary entry."
  (handle-diary-from-vm subject body 'diary-make-entry-encoded 'diary-make-entry test-only))

(defun handle-diary-from-vm (subject body encoded-handler-function string-handler-function &optional test-only)
  "Process a diary entry from a message assumed to be from MS Outlook.
SUBJECT is used to describe the appointment, and BODY contains
the formatted appointment.  ENCODED-HANDLER-FUNCTION and
STRING-HANDLER-FUNCTION are the functions to which to pass the
appointment, depending on the form in which it is found.
Optional arg TEST-ONLY non-nil means return non-nil iff the
message contains an appointment, don't make a diary entry."
  ;; (message "body=%S" body)
  (if (string-match "\\`BEGIN:VCALENDAR$" body)
      (if test-only
	  t
	(let* ((tz-block-begin (string-match "^BEGIN:VTIMEZONE$" body))
	       (tz-block-end (and (string-match "^END:VTIMEZONE$" body) (match-end 0)))
	       (tz-block (substring body tz-block-begin tz-block-end))
	       (dst nil)
	       (tz-offset (and (string-match (if dst
						 "TZOFFSETFROM:\\(.+\\)"
					       "TZOFFSETTO:\\(.+\\)")
					     tz-block)
			       (match-string 1 tz-block)))
	       (event-block-begin (string-match "^BEGIN:VEVENT$" body))
	       (event-block-end (and (string-match "^END:VEVENT$" body) (match-end 0)))
	       (event-block (substring body event-block-begin event-block-end))
	       (event-date-start-time (and (string-match "DTSTART\\(?:;.+\\)?:\\(.+\\)" event-block)
					   (match-string 1 event-block)))
	       (event-date-end-time (and (string-match "DTEND\\(?:;.+\\)?:\\(.+\\)" event-block)
					 (match-string 1 event-block))))
	  (message "tz-block=%S event-block=%S" tz-block event-block)
	  (message "tz-offset=%S event-date-start=time=%S event-date-end-time=%S" tz-offset event-date-start-time event-date-end-time)
	  (let* ((encoded-start-time (outlook-encode-date-time-string event-date-start-time tz-offset))
		 (encoded-end-time (outlook-encode-date-time-string event-date-end-time tz-offset)))
	    (funcall encoded-handler-function subject encoded-start-time encoded-end-time))))
    (catch 'finished
      (dolist (format-string diary-outlook-formats)
	(setq fred (string-match (car format-string) body))
	(message "format-string=%S match=%S" format-string fred)
	(when (eq 0 fred)
	  (unless test-only
	    (save-excursion
	      (save-window-excursion
		;; Fixme: References to optional fields in the format
		;; are treated literally, not replaced by the empty
		;; string.  I think this is an Emacs bug.
		(funcall string-handler-function
			 (format (replace-match (if (functionp format-string)
						    (funcall format-string body)
						  format-string)
						t
						nil
						(match-string 0 body))
				 subject))
		(save-buffer))))
	  (throw 'finished t)))))
  nil)



;;; end of use-vm.el
