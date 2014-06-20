;;
;; VR Mode - integration of GNU Emacs and Dragon NaturallySpeaking.
;;
;; This version forked by John C G Sturdy <john@cb1.com>
;;
;; Copyright 1999 Barry Jaspan, <bjaspan@mit.edu>.  All rights reserved.
;; See the file COPYING.txt for terms of use.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-command "vr.exe" "*The \"vr.exe\" program to be
invoked as the VR mode sub-process.  This can be just the name, if the
program is in your PATH, or it can be a full path.")

(defvar vr-host nil "*The name of the computer running the VR Mode
process.  If nil, the process will be started on this computer.  See
also vr-port.")

(defvar vr-port 0 "*The port on which to connect to vr-host.  If
vr-host is nil, this can be zero to tell the VR Mode process to use
any available port.")

(defvar vr-win-class "Emacs" "*Class name of the Windows window for
which NaturallySpeaking will be active.")
(defvar vr-win-title "" "*Title of the Windows window for
which NaturallySpeaking will be active.")

(defvar vr-activation-list nil
  "*A list of buffer name patterns which VR Mode will voice activate.
Each element of the list is a REGEXP.  Any buffer whose name matches
any element of the list is voice activated.  For example, with

(setq vr-activation-list '(\"^\\*scratch\\*$\" \"\\.txt$\"))

the buffer named \"*scratch*\" and any buffer whose name ends with
\".txt\" will be voice-activated.  Note that the minibuffer is always
voice-activated (if vr-active-minibuffers is non-nil).")

(defvar vr-voice-command-list nil
  "*The list of Emacs interactive commands that can be invoked by
voice.  For example, with

(setq vr-voice-command-list
      '(find-file (\"split window\" . split-window-vertically)))

find-file will be invoked when \"find file\" is spoken and
split-window-vertically will be invoked when \"split window\" is
spoken.

See also vr-default-voice-command-list.")

(defvar vr-log-send nil "*If non-nil, VR mode logs all data sent to the VR
subprocess in the \" *vr*\" buffer.")

(defvar vr-log-read nil "*If non-nil, VR mode logs all data received
from the VR subprocess in the \" *vr*\" buffer.")

(defvar vr-log-heard nil "*If non-nil, VR mode logs all data received
at the higher level from the VR subprocess in the \" *vr*\" buffer.")

(defvar vr-default-voice-command-list
  '(
    ;; VR Mode commands
    ("activate buffer" . vr-add-to-activation-list)

    ;; general emacs commands
    ("quit" . [?\C-g])

    ;; keystrokes that often should not be self-inserted
    ("enter" . [?\C-j])
    ("tab" . [?\C-i])
    ("space" . [? ])

    ;; files
    find-file save-buffer ("save file" . save-buffer)
    find-file-other-window find-file-other-frame
    
    ;; buffers
    switch-to-buffer kill-buffer switch-to-buffer-other-window
    switch-to-buffer-other-frame
    
    ;; windows
    ("split window" . split-window-vertically)
    other-window delete-window delete-other-windows
    
    ;; frames
    
    ;; cursor movement
    beginning-of-line end-of-line beginning-of-buffer end-of-buffer
    forward-paragraph backward-paragraph
    scroll-up scroll-down ("page down" . scroll-up) ("page up" . scroll-down)
    
    ;; formatting
    fill-paragraph
    
    ;; modes
    auto-fill-mode
    )
  "*A list of common Emacs commands, designed to be added to
vr-voice-commands-list:

(setq (setq vr-voice-command-list
      '(vr-default-voice-command-list
	<your additional commands here>)))")

(condition-case e (symbol-value 'vr-default-voice-command-list)
  ('error nil))

(setq vr-voice-command-list
      '(vr-default-voice-command-list
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-mode nil
  "Non-nil turns on VR (Voice Recognition) mode.  DO NOT SET THIS
VARIABLE DIRECTLY.  Call M-x vr-mode instead.")

(defvar vr-internal-activation-list nil
  "The working copy of vr-activation-list.  Keeping it separate allows
re-starting VR Mode to undo vr-add-to-activation-list.")

(defvar vr-mode-line " VR"
  "String displayed in the minor mode list when VR mode is enabled.
In the dictation buffer, the format is VR:<micstate>.")
(make-variable-buffer-local 'vr-mode-line)
(if (not (assq 'vr-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vr-mode vr-mode-line)
				 minor-mode-alist)))

(defvar vr-mic-state "not connected"
  "String storing the microphone state for display in the mode line.")

(defvar vr-overlay nil
  "Overlay used to track changes to voice-activated buffers.")
(make-variable-buffer-local 'vr-overlay)

(defvar vr-select-overlay (make-overlay 1 1)
  "Overlay used to track and visually indicate the NaturallySpeaking
selection.")
(delete-overlay vr-select-overlay)
(overlay-put vr-select-overlay 'face 'region)
(if (eq window-system nil)
    (progn
      (overlay-put vr-select-overlay 'before-string "[")
      (overlay-put vr-select-overlay 'after-string "]")))

(defvar vr-process nil "The VR mode subprocess.")
(defvar vr-emacs-cmds nil)
(defvar vr-dns-cmds nil)

(defvar vr-reading-string nil "Storage for partially-read commands
from the VR subprocess.")

(defvar vr-buffer nil "The current voice-activated buffer, or nil.
See vr-activate-buffer and vr-switch-to-buffer.")

(defvar vr-ignore-changes nil "see comment in vr-overlay-modified")
(defvar vr-queued-changes nil "see comment in vr-overlay-modified")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-prefix-map nil "Prefix key used to access VR mode commands.")
(defvar vr-map nil)
(if vr-map
    nil
  (setq vr-map (make-sparse-keymap))
  (define-key vr-map "ws" 'vr-show-window)
  (define-key vr-map "wh" 'vr-hide-window)
  (define-key vr-map "B" 'vr-add-to-activation-list)
  (define-key vr-map "b" 'vr-switch-to-buffer)
  (define-key vr-map "m" 'vr-toggle-mic)
  (define-key vr-map "q" 'vr-quit)
  )

(if vr-prefix-map
    nil
  (setq vr-prefix-map (make-sparse-keymap))
  (define-key vr-prefix-map "\C-cv" vr-map))

(if (not (assq 'vr-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'vr-mode vr-prefix-map) minor-mode-map-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points for global hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-active-minibuffers t
  "Whether the minibuffers should be voice-activated.")

(defun vr-enter-minibuffer ()
  (if (and vr-active-minibuffers
	   vr-emacs-cmds)
      (vr-activate-buffer (current-buffer))))

(defun vr-post-command ()
  (add-hook 'post-command-hook 'vr-post-command)
  (if (overlayp vr-select-overlay)
      (delete-overlay vr-select-overlay))
  (if vr-emacs-cmds
      (progn
	;;(vr-log "post-command: %s\n" this-command)
	(vr-maybe-activate-buffer (current-buffer)))))

(defun vr-kill-buffer ()
  (if vr-emacs-cmds
      (progn
	;;(vr-log "kill-buffer: %s\n" (current-buffer))
	(vr-send-cmd (concat "kill-buffer " (buffer-name (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer activation control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-filter (pred in)
  (let (out el)
    (while in
      (setq el (car in))
      (setq in (cdr in))
      (if (funcall pred el)
	  (setq out (cons el out)))
      )
    out))
  
(defun vr-add-to-activation-list (buffer)
  "Adds BUFFER, which can be a buffer name or buffer, to the list of
buffers that are voice activated.  Called interactively, adds the
current buffer.

The only way to undo the effect of this function is to re-start VR
Mode."
  ;; If called interactively, vr-post-command will activate the
  ;; current buffer (so this function doesn't have to).
  (interactive (list (current-buffer)))
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (if (vr-activate-buffer-p buffer)
      nil
    (setq vr-internal-activation-list
	  (cons (concat "^" (regexp-quote buffer) "$")
		vr-internal-activation-list))))

(defun vr-activate-buffer-p (buffer)
  "Predicate indicating whether BUFFER matches any element of
vr-internal-activation-list.  BUFFER can be a buffer or a buffer name."
  (if (bufferp buffer)
      (setq buffer (buffer-name buffer)))
  (vr-filter (lambda (r) (string-match r buffer)) vr-internal-activation-list))

(defun vr-maybe-activate-buffer (buffer)
  (if (eq buffer vr-buffer)
      nil
    (if (vr-activate-buffer-p (buffer-name buffer))
	(vr-activate-buffer buffer)
      ;; if vr-buffer is already nil, don't bother deactivating again
      (if vr-buffer 
	  (vr-activate-buffer nil)))))

(defun vr-switch-to-buffer ()
  "Select the current VR mode target buffer in the current window."
  (interactive)
  (if (buffer-live-p vr-buffer)
      (switch-to-buffer vr-buffer)
    (error "VR target buffer no longer exists; use vr-activate-buffer")))

(defun vr-activate-buffer (buffer)
  "Sets the target BUFFER that will receive voice-recognized text.  Called
interactively, sets the current buffer as the target buffer."
  (interactive (list (current-buffer)))
  ;; (message "In vr-activate-buffer")
  (if (buffer-live-p vr-buffer)
      (save-excursion
	(set-buffer vr-buffer)
	;; somehow vr-buffer can be set to the minibuffer while
	;; vr-overlay is nil.
	(if (overlayp vr-overlay)
	    (delete-overlay vr-overlay))
	(setq vr-overlay nil)
	(kill-local-variable 'vr-mode-line)))
  (set-default 'vr-mode-line (concat " VR-" vr-mic-state))
  (setq vr-buffer buffer)
  (if buffer
      (save-excursion
	(set-buffer buffer)
	(setq vr-mode-line (concat " VR:" vr-mic-state))
	(vr-send-cmd (concat "activate-buffer " (buffer-name vr-buffer)))
	(if vr-overlay
	    nil
	  (setq vr-overlay (make-overlay (point-min) (point-max) nil nil t))
	  (overlay-put vr-overlay 'modification-hooks '(vr-overlay-modified))
	  (overlay-put vr-overlay 'insert-in-front-hooks '(vr-grow-overlay))
	  (overlay-put vr-overlay 'insert-behind-hooks '(vr-grow-overlay)))
	)
    (vr-send-cmd "deactivate-buffer")
    )
  (force-mode-line-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tracking changes to voice-activated buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vr-overlay-before-count 0 "see comment in vr-grow-overlay")

(defun vr-grow-overlay (overlay after beg end &optional len)
  ;; Make OVERLAY grow to contain range START to END.  If called "before"
  ;; twice before called "after", only call vr-overlay-modified once.
  ;; This happens when we type the first char in the buffer, because I
  ;; guess it is inserted both before and after the empty overlay.

  ;;(vr-log "Grow: %s %d %d %s\n" (if after "After: " "Before: ") beg end
  ;;	  (if after (int-to-string len) ""))
  (if after
      (progn
	(move-overlay overlay
		      (min beg (overlay-start overlay))
		      (max end (overlay-end overlay)))
	(setq vr-overlay-before-count (1- vr-overlay-before-count))
	(if (> vr-overlay-before-count 0)
	    (progn ;; (vr-log "   ignored duplicate grow\n")
	      nil)
	  (vr-overlay-modified overlay after beg end len)))
    (setq vr-overlay-before-count (1+ vr-overlay-before-count))))

(defun vr-overlay-modified (overlay after beg end &optional len)
  (if after
      ;; If vr-ignore-changes is not nil, we are inside the make-changes
      ;; loop.  Don't tell DNS about changes it told us to make.  And,
      ;; for changes we do need to tell DNS about (e.g. auto-fill),
      ;; queue them up instead of sending them immediately to avoid
      ;; synchronization problems.  make-changes will send them when
      ;; it is done. 
      ;;
      ;; This is not a foolproof heuristic.
      (if (or (and (eq vr-ignore-changes 'self-insert)
		   (eq len 0)
		   (eq (- end beg) 1)
		   (eq (char-after beg) last-command-char))
	      (and (eq vr-ignore-changes 'delete)
		   (> len 0)
		   (eq beg end)))
	  (progn ;;(vr-log "ignore: %d %d %d: \"%s\"\n" beg end len
		 ;;(buffer-substring beg end))
	    nil)
	;;(vr-log " After: %d %d %d: \"%s\"\n" beg end len
	;;(buffer-substring beg end))
	(let ((cmd (format "change-text \"%s\" %d %d %d %d %s"
			   (buffer-name (overlay-buffer overlay))
			   (1- beg) (1- end) len
			   (buffer-modified-tick)
			   (vr-string-replace (buffer-substring beg end)
					      "\n" "\\n"))))
	  (if vr-ignore-changes
	      (setq vr-queued-changes (cons cmd vr-queued-changes))
	    (vr-send-cmd cmd))))))

(defun vr-string-replace (src regexp repl)
  (let ((i 0))
    (while (setq i (string-match regexp src))
      (setq src (concat (substring src 0 i)
			repl
			(substring src (match-end 0))))
      (setq i (match-end 0))))
  src)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess communication.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-log-heard (&rest s)
  "Log a high-level input."
  (apply 'vr-log0 "*vr expr*" s))

(defun vr-log (&rest s)
  "Log a vr transaction."
  (apply 'vr-log0 " *vr*" s))

(defvar vr-log-to-file nil
  "Whether to write the log to a file.")

(defun vr-log0 (bufname &rest s)
  (let* ((buf (get-buffer-create bufname))
	 (win (get-buffer-window buf 't)))
    (save-excursion
      (set-buffer buf)
      (let ((old-end (point-max)))
	(goto-char old-end)
	(insert (apply 'format s))
	(if vr-log-to-file
	    (if (string= bufname " *vr*")
		(write-region old-end (point-max)
			      "~/.vr-log"
			      t
			      )))
	(if win
	    (set-window-point win (point-max)))
	))))

(if vr-log-to-file
    (vr-log "\fStarting vr session at " (current-time-string) "\n"))

(defun vr-sentinel (p s)
  (if (equal s "finished\n")
      (progn
	(if (processp vr-process)
	    (delete-process vr-process))
	(if (processp vr-emacs-cmds)
	    (delete-process vr-emacs-cmds))
	(if (processp vr-dns-cmds)
	    (delete-process vr-dns-cmds))
	(setq vr-process nil
	      vr-emcas-cmds nil
	      vr-dns-cmds nil))
    (error "VR process exited with status \"%s\"" s)))

;;; actions to take on vr commands received

(defun vr-action-listening (connect-to)
  (vr-connect "127.0.0.1" connect-to))

(defun vr-action-connected ()
  (vr-send-cmd (format "initialize %s|%s"
		       vr-win-class vr-win-title))
  )

(defun vr-action-initialize (option)
  (cond ((eq option 'succeeded)
	 (vr-startup))
	((eq option 'no-window)
	 (vr-mode 0)
	 (message "VR process: no window matching %s %s"
		  vr-win-class vr-win-title))
	(t
	 (vr-mode 0)
	 (message "VR process initialization: %s"
		  option))))

(defun vr-action-dict-state (s1 d1 d2 s2)
  (vr-log "State: %s %d %d:\n[%s]\n"
	  s1 d1 d2 s2))

(defun vr-action-frame-activated (wndstring)
  ;; This is ridiculous, but Emacs does not
  ;; automatically change its concept of "selected
  ;; frame" until you type into it.  So, we have the
  ;; subprocess send us the HWND value and explcitly
  ;; activate the frame that owns it.  This code
  ;; assumes that there will be a frame with a matching
  ;; HWND, which should always be true since this is
  ;; only invoked when the user selects a real window,
  ;; generating a WM_ACTIVATE message.
  ;;
  ;; The alternative monstrosity is having the
  ;; subprocess generate some no-op key sequence.
  (let ((wnd (int-to-string wndstring)))
    (select-frame
     (car (vr-filter
	   (lambda (f) (equal (cdr (assoc 'window-id
					  (frame-parameters f)))
			      wnd))
	   (visible-frame-list)))))
  (vr-maybe-activate-buffer (current-buffer)))

(defun symbol-name-safe (symbol)
  "A safer version of symbol-name."
  (cond
   ((symbolp symbol) (symbol-name symbol))
   ((numberp symbol) (int-to-string symbol))
   (t (error "I do not know how to turn %S into a symbol" symbol))))

(defvar vr-action-heard-command-hook nil
  "Hooks for vr-action-heard-command, in case we want to do something else with commands.
Functions on this hook should return non-nil if they want to allow others a chance.")

(defun vr-action-heard-command (cmd)
  (unless (run-hook-with-args-until-success 'vr-action-heard-command-hook cmd)
    ;;
    ;; We want to execute the command after this filter
    ;; function terminates, so add the key sequence to
    ;; invoke it to the end of unread-command-events.
    ;; Add the key binding, if any, so we don't get the
    ;; "you can run the command <cmd> by typing ..." message.
    ;;
    ;; This feels like a kludge...
    ;;
    (let ((kseq (or (and (vectorp cmd) cmd)
		    (where-is-internal cmd nil 'non-ascii)
		    (concat "\M-x" (symbol-name-safe cmd) "\n"))))
      (setq unread-command-events
	    (append unread-command-events
		    (listify-key-sequence kseq))))))

(defvar vr-mic-state-hooks nil
  "Functions to call with the new microphone state, each time it changes.")

(defun vr-action-mic-state (state)
  (run-hook-with-args 'vr-mic-state-hooks state)
  (cond ((eq state 'off)
	 (setq vr-mic-state "off"))
	((eq state 'on)
	 (setq vr-mic-state "on"))
	((eq state 'sleep)
	 (setq vr-mic-state "sleep")))
  (vr-activate-buffer vr-buffer))

(defun vr-action-get-buffer-info (a &optional tick) ; not optional in the distribution, but sometimes this gets called with just one argument

  (unless tick (message "vr-action-get-buffer-info called with one arg, %S" a))
    ;;
    ;; If mouse-drag-overlay exists in our buffer, it
    ;; overrides vr-select-overlay.
    ;;
    (let* ((mdo mouse-drag-overlay)
	   (sel-buffer (overlay-buffer mdo)))
      (if (eq sel-buffer vr-buffer)
	  (move-overlay vr-select-overlay
			(overlay-start mdo)
			(overlay-end mdo)
			sel-buffer)))
		   
    ;;
    ;; Send selection (or point) and viewable window.
    ;;
    (let ((sel-buffer (overlay-buffer vr-select-overlay)))
      (if (eq sel-buffer vr-buffer)
	  (progn
	    (vr-send-reply (1- (overlay-start vr-select-overlay)))
	    (vr-send-reply (1- (overlay-end vr-select-overlay)))
	    )
	(vr-send-reply (1- (point)))
	(vr-send-reply (1- (point)))
	))
    (vr-send-reply (1- (window-start)))
    (vr-send-reply (1- (window-end)))
    ;;
    ;; Then, send buffer contents, if modified.
    ;;
    (if (eq (buffer-modified-tick) tick)
	(vr-send-reply "0 not modified")
      (vr-send-reply (format "1 modified, tick = %d"
			     (buffer-modified-tick)))
      (setq vr-text (buffer-string))
      (vr-send-reply (length vr-text))
      (vr-send-reply vr-text))
    )

(defun vr-action-make-changes (start-1 num-chars text sel-start-1 sel-chars)
  "From voice input, make changes to a buffer"
  (message "In (vr-action-make-changes start=%d num=%d text=\"%s\" sel-start=%d sel-chars=\"%s\") vr-buffer=\"%s\"" start-1 num-chars text sel-start-1 sel-chars vr-buffer)
  (if (eq (current-buffer) vr-buffer)
      (progn
	(let ((start (1+ start-1))
	      (sel-start (1+ sel-start-1))
	      (vr-queued-changes nil))
	  (let ((vr-ignore-changes 'delete))
	    (delete-region start (+ start num-chars)))
	  (goto-char start)		; sometimes this takes it to the wrong place; presumably the voice system has not kept up with where point is
	  (let ((vr-ignore-changes 'self-insert)
		(i 0)
		(n (length text)))
	    (while (< i n)
	      (let ((last-command-char (aref text i)))
		(self-insert-command 1))
	      (incf i)))
	  (goto-char sel-start)
	  (delete-overlay mouse-drag-overlay)
	  (if (equal sel-chars 0)
	      (delete-overlay vr-select-overlay)
	    (move-overlay vr-select-overlay
			  sel-start (+ sel-start sel-chars)
			  (current-buffer)))
	  (vr-send-reply (buffer-modified-tick))
	  (vr-send-reply (length vr-queued-changes))
	  (mapcar 'vr-send-reply vr-queued-changes)))
    (progn
      (message "Tried to insert in %s but %s is the active voice buffer"
	       (current-buffer) vr-buffer)
      (vr-send-reply "-1"))))

(defvar vr-output-filter-actions
  '((listening  . vr-action-listening)
    (connected  . vr-action-connected)
    (initialize  . vr-action-initialize)
    (dict-state  . vr-action-dict-state)
    (frame-activated  . vr-action-frame-activated)
    (heard-command  . vr-action-heard-command)
    (mic-state  . vr-action-mic-state)
    (get-buffer-info  . vr-action-get-buffer-info)
    (make-changes  . vr-action-make-changes))
  "Actions that can be taken in vr-output-filter")

;;; the filter itself

(defun vr-output-filter (p s)
  (unless (stringp s)
    (when vr-log-read
      (vr-log "Non-string given to vr-output-filter")))
  (setq vr-reading-string (concat vr-reading-string s))
  (if vr-log-read
      (vr-log "vr-output-filter got \"%s\"\n" s))
  ;;(condition-case err
  (while (> (length vr-reading-string) 0)
    (let* ((parsed (read-from-string vr-reading-string))
	   (vr-request (car parsed))
	   (idx (cdr parsed))
	   (vr-cmd (car vr-request))
	   (vr-text nil))
      (if vr-log-read
	  (vr-log "-> %s\n" (substring vr-reading-string 0 idx)))
      (setq vr-reading-string (substring vr-reading-string (1+ idx)))
      (if vr-log-heard
	  (vr-log-heard "%S\n" vr-request))
      
      ;; (message "Looking for command %s in %S" vr-cmd vr-output-filter-actions)
      (let ((action (cdr (assoc vr-cmd vr-output-filter-actions))))
	(if action
	    (apply action (cdr vr-request))
	  (error "Unknown command from vr program: %s" vr-cmd)))))

  ;; sexp isn't complete yet.  Since all commands are currently
  ;; issued in a single write(), this "never" happens.  So for now,
  ;; re-throw the error; if this ever changes, we'll notice. :-)
  ;;('error (error err)))
  )

(defun vr-send-reply (msg)
  "Send reply MSG to the VR process."
  (if (and (processp vr-dns-cmds)
	   (eq (process-status vr-dns-cmds) 'open))
      (progn
	(if (integerp msg)
	    (setq msg (int-to-string msg)))
	(if vr-log-send
	    (vr-log "<- r %s\n" msg))
	(process-send-string vr-dns-cmds (vr-etonl (length msg)))
	(process-send-string vr-dns-cmds msg))
    (message "VR Mode DNS reply channel is not open!")))

(defun vr-send-cmd (msg)
  "Send command MSG to the VR process."
  (if vr-log-send
      (vr-log "<- pc %s\n" msg))
  (if (and (processp vr-emacs-cmds)
	   (eq (process-status vr-emacs-cmds) 'open))
      (progn
	(if vr-log-send
	    (vr-log "<- c %s\n" msg))
	(process-send-string vr-emacs-cmds (vr-etonl (length msg)))
	(process-send-string vr-emacs-cmds msg))
    (message "VR Mode emacs command channel is not open!")))

;; ewww
(defun vr-etonl (i)
  (format "%c%c%c%c"
	  (lsh (logand i 4278190080) -24)
	  (lsh (logand i 16711680) -16)
	  (lsh (logand i 65280) -8)
	  (logand i 255)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-quit ()
  "Turn off VR mode, and cause the VR mode subprocess to exit cleanly."
  (interactive)
  (vr-mode 0))

(defun vr-toggle-mic ()
  "Toggles the state of the Dragon NaturallySpeaking microphone:
off -> on, {on,sleeping} -> off."
  (interactive)
  (vr-send-cmd "toggle-mic"))

(defun vr-mic-on-p ()
  "Return whether the microphone is on."
  (string= vr-mic-state "on"))

(defun vr-mic (on)
  "Put the microphone ON (or off, if nil)."
  (let ((already-on ))
    (if (eq (not on)			; normalize to t or nil
	    (vr-mic-on-p))
	(vr-toggle-mic))))

(defun vr-show-window ()
  (interactive)
  (vr-send-cmd "show-window"))

(defun vr-hide-window ()
  (interactive)
  (vr-send-cmd "hide-window"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subprocess initialization, including voice commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-connect (host port)
  (condition-case e
      (progn
	(setq vr-emacs-cmds (open-network-stream "vr-emacs" nil
						 host port))
	(setq vr-dns-cmds (open-network-stream "vr-dns" nil host port))
	(set-process-filter vr-dns-cmds 'vr-output-filter)
	(if vr-process
	    (set-process-filter vr-process nil))
	t)
    ('error (progn
	      (vr-mode 0)
	      (message "VR Mode: cannot connect to %s:%d" host port)
	      nil))))

;; functionp isn't defined in Win 95 Emacs 19.34.6 (!??!?)
(defun vr-functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

(defun vr-strip-dash (symbol)
  (concat (mapcar (lambda (x) (if (eq x ?-) ?\  x)) (symbol-name symbol))))

;;;; vr command handling

(defvar vr-commands-registered nil
  "The VR commands registered")

(defun vr-define-command (sound command)
  "Define SOUND to invoke COMMAND."
  (push (cons sound 
	      (cond
	       ((stringp command) (intern command))
	       (t command)))
	vr-commands-registered)
  (vr-send-cmd
   (concat "define-command "
	   sound "|" command)))

(defun vr-command-help ()
  "List the vr commands."
  (interactive)
  (if vr-commands-registered
      (with-output-to-temp-buffer "*Help*"
	(let* ((commands vr-commands-registered)
	       (formatstring (format " %%%ds: %%s\n"
				     (apply 'max
					    (mapcar 'length
						    (mapcar 'car commands))))))
	  (while commands
	    (princ (format formatstring (car (car commands)) (cdr (car commands))))
	    (setq commands (cdr commands)))))
    (message "There are no commands registered")))

(defun vr-short-help-on-spoken-command (command-string)
  "Indicate what command COMMAND-STRING runs."
  (interactive (list (completing-read "Command words: " vr-commands-registered)))
  (let ((pair (assoc command-string vr-commands-registered)))
    (if pair
	(message "Saying \"%s\" runs %s" command-string (cdr pair))
      (message "\"%s\" is not a voice command"))))

(defun vr-find-command-function (command-string)
  (interactive (list (completing-read "Find function implementing command words: " vr-commands-registered)))
  (let ((pair (assoc command-string vr-commands-registered)))
    (if pair
	(find-function (cdr pair))
      (message "\"%s\" is not a voice command"))))

(defun vr-whereis (command)
  "Show the words which must be spoken to activate this COMMAND"
  (interactive "CFind spoken form for command: ")
  (let ((pair (rassq command vr-commands-registered)))
    (if pair
	(message "Say \"%s\" to do %S" (car pair) command)
      (message "No spoken form for %S" command))))

(defun vr-whereis-key (key-sequence)
  "Show the words which must be spoken to get the same effect as KEY-SEQUENCE"
  (interactive "KFind spoken form for key sequence ")
  (let ((command (key-binding key-binding)))
    (if command
	(let ((pair (rassq command vr-commands-registered)))
	  (if pair
	      (message "Say \"%s\" to do %S (%S)" (car pair) key-sequence command)
	    (message "No spoken form for %S (%S)" key-sequence command)))
      (message "No key binding for %S" key-sequence))))

(autoload 'vr-modal-command-help "vr-help"
  "List voice commands associated with this mode.
Called from a program,you can specify the mode." t)

(autoload 'vr-grouped-command-help "vr-help"
  "List voice commands in groups. Each group is those words specified in one call to vr-startup.
Called from a program,you can specify the symbol from which to get them." t)

(autoload 'vr-html-grouped-command-help "vr-help"
  "Generate HTML documentation for voice commands in groups.
Each group is those words specified in one call to vr-startup (which is called recursively
on symbols naming lists of symbols... naming commands).
Called from a program, you can specify the symbol from which to get them." t)

;;;; initialize

(defun vr-startup (&optional command-list)
  "Initialize any per-execution state of the VR Mode subprocess.
This is done when the VR Mode subprocess commands it."
  (vr-log "(vr-startup %S)" command-list)
  (if (null command-list)
      (progn
	(vr-log "using top-level list %S" vr-voice-command-list)
	(setq command-list vr-voice-command-list)))
  (while command-list
    (let ((command (car command-list)))
      (vr-log "adding voice command %S" command)
      ;; The order of the conditions is important, because a
      ;; symbol is treated differently if it is a list.
      (cond
       ((condition-case e
	    (consp
	     ;; use consp rather than listp, because nil means start with the top-level list
	     (symbol-value command))
	  ('error nil))
	(vr-log "Using symbol-value of command %S which is %S" command (symbol-value command))
	(vr-startup (symbol-value command)))
       ((symbolp command)
	(vr-define-command (vr-strip-dash command) (symbol-name command)))
       ;; note: (commandp <vector>) --> t
       ((and (consp command) (vectorp (cdr command)))
	(vr-define-command (car command) (cdr command)))
       ((and (consp command) (symbolp (cdr command)))
	(vr-define-command (car command) (symbol-name (cdr command))))
       (t
	(error "Unknown vr-voice-command-list element %s"
	       command))))
    (setq command-list (cdr command-list)))
  (vr-maybe-activate-buffer (current-buffer))
  (run-hooks 'vr-mode-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VR Mode entry/exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vr-on ()
  "Switch VR on."
  (interactive)
  (vr-mode 1))

(defun vr-off ()
  "Switch VR off."
  (interactive)
  (vr-mode -1))

;;; no this won't work -- crashed dragon.
;;; Just make it put the microphone on/off
;;; remember the old state somehow!
;;; (add-hook 'type-break-start-break-hook 'vr-off)
;;; (add-hook 'type-break-end-break-hook 'vr-on-maybe)

(defun vr-mode (arg)
  "Toggle VR mode.  With argument ARG, turn VR mode on iff ARG is
positive.

VR mode supports Dragon NaturallySpeaking dictation, Select 'N
Say(tm), and voice commands in Emacs buffers.  See README.txt for
instructions.

\\{vr-map}"
  (interactive "P")
  (setq vr-mode
	(if (null arg) (not vr-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if vr-mode
      ;; Entering VR mode
      (progn
	(setq vr-reading-string nil
	      vr-mic-state "not connected")
	(set-default 'vr-mode-line (concat " VR-" vr-mic-state))
	(setq vr-internal-activation-list vr-activation-list)
	(add-hook 'post-command-hook 'vr-post-command)
	(add-hook 'minibuffer-setup-hook 'vr-enter-minibuffer)
	(add-hook 'kill-buffer-hook 'vr-kill-buffer)
	(run-hooks 'vr-mode-setup-hook)

	(if vr-host
	    (vr-connect vr-host vr-port)
	  (setq vr-process (start-process "vr" " *vr*" vr-command
					  "-child"
					  "-port" (int-to-string vr-port)))
	  (set-process-filter vr-process 'vr-output-filter)
	  (set-process-sentinel vr-process 'vr-sentinel))
	)
    ;; Leaving VR mode
    (remove-hook 'post-command-hook 'vr-post-command)
    (remove-hook 'minibuffer-setup-hook 'vr-enter-minibuffer)
    (remove-hook 'kill-buffer-hook 'vr-kill-buffer)
    (vr-activate-buffer nil)
    (if vr-host
	(vr-sentinel nil "finished\n")
      (vr-send-cmd "exit"))
    )
  (force-mode-line-update)
  )

(provide 'vr-mode)

;;; end of vr-mode.el
