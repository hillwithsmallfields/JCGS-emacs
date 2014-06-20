;;;; rpn-edit.el -- reverse polish editing commands, aimed at voice command use
;;; Time-stamp: <2005-04-28 16:28:40 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; RPN editing, using the kill ring as a stack for strings, and
;; the mark-ring as a stack for places, and adding a number
;; stack. Meant for use through voice commands, as this is a way of
;; handling command arguments without trying to speak syntactical
;; punctuation! For old PostScript hackers, or HP calculator users,
;; this is fairly natural.

(provide 'rpn-edit)
(require 'cl)

(defconst interactive-line-to-rpn-alist
  '((?s . string)
    (?f . existing-filename)
    (?F . filename)
    (?d . point)
    (?m . mark)
    (?p . prefix)
    (?P . raw-prefix)
    (?M . string)
    (?n . number)
    (?N . number)
    (?b . existing-buffer)
    (?B . buffer)
    (?r . region))
  "Translations of interactive keys to rpn keys.")

(defun interactive-line-to-rpn (line)
  "Return the RPN control item corresponding to LINE
which comes from an interactive definition string."
  (cdr (assoc (aref line 0) interactive-line-to-rpn-alist)))

(defun interactive-to-rpn (istring)
  "Convert ISTRING to an rpn args list"
  ;; did have mapcar for this, but 'region spoils it by providing two args
  (let ((keys nil))
    (dolist (key (nreverse
		  (mapcar 'interactive-line-to-rpn
			  (split-string istring "\n"))))
      (cond
       ((eq key 'region)
	(progn
	  (push 'region-end keys)
	  (push 'region-beginning keys)))
       ((null key)
	(error "Cannot convert some element of %s to rpn descr" istring))
       (t (push key keys))))
    (list keys nil)))

(defun rpn-descr (command)
  "Return the stack arguments / results description for COMMAND."
  (if (symbolp command) (setq command (symbol-function command)))
  (cond
   ((and (consp command)
	 (eq (car command) 'lambda))
    (setq command (cdr (cdr command)))
    (if (stringp (car command))
	(setq command (cdr command)))
    (cond 
     ((eq (car (car command)) 'interactive)
      (interactive-to-rpn (cdr (car command))))
     ((eq (car (car command)) 'rpn)
      (cdr (car command)))
     (t nil)))
   ((and (byte-code-function-p command))
    (interactive-to-rpn (aref command 5)))
   (t nil)))

(defun rpn-arg-get-filename (type)
  "Get the next filename arg."
  (let ((name (current-kill 1)))
    (when (and (eq type 'existing-filename)
	       (not (file-exists-p name)))
      (error "Wanted name of existing file from kill ring."))
    name))

(defun rpn-arg-get-string (type-dummy)
  "Get the next string arg."
  (prog1
      (current-kill 0)
  (current-kill 1)))

(defun rpn-result-put-string (type-dummy string)
  "Push a string result."
  (kill-new string))

(defvar rpn-arg-mark-count nil
  "Whether we have yet used the mark, and if so, how many to go into the mark ring.")

(defun rpn-arg-get-next-mark (type-dummy)
  "Return the next mark."
  ;; the first time we're called for a particular rpn function,
  ;; we return the current mark; subsequent ones come from the mark ring
  (if rpn-arg-mark-count
      (prog1
	  (nth rpn-arg-mark-count mark-ring)
	(incf rpn-arg-mark-count))
    (setq rpn-arg-mark-count 0)
    (mark)))

(defvar rpn-number-stack nil
  "The stack of numbers.")

(defun rpn-arg-get-number (type-dummy)
  "Get a number from the stack."
  ;; todo: integrate with prefix arguments? (remember to display, if we do this)
  (pop rpn-number-stack))

(defun rpn-arg-put-number (type-dummy val)
  "Push a number onto the stack."
  (push val rpn-number-stack))

(defvar rpn-arg-buffer-list nil
  "Storage for the buffer list while buffers are popped off it.")

(defun rpn-arg-get-buffer (type-dummy)
  "Get the next buffer arg."
  (when (eq rpn-arg-buffer-list t) (setq rpn-arg-buffer-list (buffer-list)))
  (let ((buffer (pop rpn-arg-buffer-list)))
    (while (and rpn-arg-buffer-list
		(eq (aref (buffer-name buffer) 0) ? ))
      (setq buffer (pop rpn-arg-buffer-list)))
    buffer))

(defvar rpn-arg-getters
  '((string . rpn-arg-get-string)
    (filename . rpn-arg-get-filename)
    (existing-filename . rpn-arg-get-filename)
    (number . rpn-arg-get-number)
    (region-beginning . region-beginning)
    (region-end . region-end)
    (point . point)
    (mark . rpn-arg-get-next-mark)
    (buffer . rpn-arg-get-buffer)
    (existing-buffer . rpn-arg-get-buffer)
    (marker . nil)
    )
  "Alist from type names to how to pop something of that type from a stack.")

(defvar rpn-arg-putters
  '((string . rpn-arg-put-string)
    (filename . rpn-arg-put-string)
    (existing-filename . rpn-arg-put-string)
    (number . rpn-arg-put-number)
    (point . goto-char)
    (buffer . set-buffer)
    (existing-buffer . set-buffer)
    (mark . push-mark)
    )
  "Alist from type names to how to push something of that type onto a stack.")

(defun rpn-get-arg (arg-type)
  "Return the next arg of ARG-TYPE."
  (funcall (cdr (assoc arg-type rpn-arg-getters)) arg-type))

(defun rpn-put-arg (res-type value)
  "Store the next res of RES-TYPE, using VALUE."
  (funcall (cdr (assoc res-type rpn-arg-putters)) res-type value))

(defun rpn-call (command args-descr results-descr)
  "Call COMMAND using ARGS-DESCR and RESULTS-DESCR."
  (setq rpn-arg-buffer-list t
	rpn-arg-mark-count nil)
  (let* ((args (mapcar 'rpn-get-arg args-descr))
	 (raw-results (apply command args)))
    (cond
     ((null results-descr)
      nil)
     ((consp results-descr)
      (dolist (result-type results-descr)
	(rpn-put-arg result-type (pop raw-results))))
     ((symbolp results-descr)
      (rpn-put-arg results-descr raw-results))))
  (update-shown-stacks))

(defun rpn-call-interactively (command)
  "Call COMMAND using the stack for its arguments."
  (let ((rpn-descr (rpn-descr command)))
    (rpn-call command
	      (first rpn-descr)
	      (second rpn-descr))))

;;;; now some editing commands using the stacks

(defun as-one-line (string length-limit)
  "Make STRING into one line at most LENGTH-LIMIT long."
  (let* ((old-len (length string))
	 (new-len (min length-limit old-len))
	 (result (make-string (1+ new-len) 32))
	 (i 0)
	 (j 0))
    (while (and (< i old-len)
		(< j new-len))
      (if (= (aref string i) ?\n)
	  (progn
	    (aset result j ?\\)
	    (incf j)
	    (aset result j ?n))
	(aset result j (aref string i)))
      (incf i)
      (incf j))
    result))

(defvar stacks-popup-frame nil
  "*Whether to use a popup frame for rpn's show-stacks.")

(defvar stacks-frame-parameters 
  '((width . 80) (height . 8)
    (auto-raise t)
    (menu-bar-lines 1)
    (top . 0)
    (left . 480)
    (title . "stack")
    (modeline))
  "*Parameters for popup frame for sidebrain.")

(defvar stack-buffer-name  "*Stacks*"
  "The name of the stacks display buffer.")

(defun stack-window-height ()
  "Calculate the height of the stack window."
  (let* ((h (cdr (assoc 'height stacks-frame-parameters)))
	 (from-frame (and stacks-popup-frame
			  (integerp h))))
    (if from-frame
	h
      (window-height))))

(defun stack-window-width ()
  "Calculate the width of the stack window."
  (let* ((h (cdr (assoc 'width stacks-frame-parameters)))
	 (from-frame (and stacks-popup-frame
			  (integerp h))))
    (if from-frame
	h
      (window-width))))

(defun show-stacks ()
  "Show the current stack contents."
  (interactive)
  (let ((pop-up-frames stacks-popup-frame)
	(pop-up-frame-alist stacks-frame-parameters))
    (with-output-to-temp-buffer stack-buffer-name
      (dolist (number rpn-number-stack)
	(princ (format "%d " number)))
      (princ "\n")
      (let ((w (- (stack-window-width) 6))
	    (i (max 3 (- (stack-window-height) 3)))
	    (j 0))
	(while (> i 0)
	  (princ (format "%-2d: %s\n" j (as-one-line (current-kill j t) w)))
	  (setq i (1- i) j (1+ j)))))
    (display-buffer stack-buffer-name))
  (if stacks-popup-frame
      ;; todo: this dedicates the wrong window sometimes?
      ;; (set-window-dedicated-p (get-buffer-window stack-buffer-name) t)
      nil ; if does not like not having a then
    ))

(defun update-shown-stacks ()
  "Update the stacks display if they are visible."
  (when (get-buffer-window stack-buffer-name 'visible)
    (show-stacks)))

(defadvice kill-ring-save (after show-stack-changes)
  "Update the stack display after saving something to the kill ring."
  (update-shown-stacks))

(defadvice kill-region (after show-stack-changes)
  "Update the stack display after saving something to the kill ring."
  (update-shown-stacks))

(defadvice kill-new (after show-stack-changes)
  "Update the stack display after saving something to the kill ring."
  (update-shown-stacks))

(defadvice yank-pop (after show-stack-changes)
  "Update the stack display after saving something to the kill ring."
  (update-shown-stacks))

;; (ad-activate 'kill-ring-save)
;; (ad-activate 'kill-region)
(ad-activate 'kill-new)
(ad-activate 'yank-pop)

(defun rpn-edit-top ()
  "Edit the top item on the stack."
  (interactive)
  (let ((original (current-kill 0 t))
	(bufname "*edit top*"))
    (current-kill 1 nil)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create bufname))
      (erase-buffer)
      (insert original)
      (recursive-edit)
      (set-buffer bufname)
      (kill-new (buffer-string))
      (update-shown-stacks))))

(defun rpn-edit-top-from-blank ()
  "Edit a new top item for the stack."
  (interactive)
  (let ((bufname "*edit top*"))
    (current-kill 1 nil)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create bufname))
      (erase-buffer)
      (recursive-edit)
      (set-buffer bufname)
      (kill-new (buffer-string))
      (update-shown-stacks))))

(defun rpn-push-region (start end)
  "Push the region onto the stack."
  (interactive "r")
  (rpn-call 'buffer-substring
	    '(region-beginning region-end)
	    '(string)))

(defun rpn-pop-insert ()
  "Pop a string and insert it."
  (interactive)
  (rpn-call 'insert
	    '(string)
	    nil))

(defun rpn-search-forward ()
  "Search forward for the pattern on the top of the stack."
  (interactive)
  (re-search-forward (current-kill 0 t)))

(defun rpn-search-backward ()
  "Search backward for the pattern on the top of the stack."
  (interactive)
  (re-search-backward (current-kill 0 t)))

(defun rpn-exch ()
  "Exchange the top two elements of the string stack."
  (interactive)
  (rpn-call (function
	     (lambda (a b)
	       (list b a)))
	    '(string string)
	    '(string string)))

(defun rpn-dup ()
  "Duplicate the top element of the string stack."
  (interactive)
  (rpn-call (function
	     (lambda (a)
	       (list a a)))
	    '(string)
	    '(string string)))

(defun rpn-pop ()
  "Pop the top element of the string stack."
  (interactive)
  (rpn-call 'identity
	    '(string)
	    nil))

(defun rpn-replace-string ()
  "Replace-string on the top two elements of the string stack."
  (interactive)
  (rpn-call 'replace-string
	    '(string string)
	    nil))

(defun rpn-replace-regexp ()
  "Replace-regexp on the top two elements of the string stack."
  (interactive)
  (rpn-call 'replace-regexp
	    '(string string)
	    nil))

(defun rpn-delete-matching-lines ()
  "Delete lines that match the top element of the string stack."
  (interactive)
  (rpn-call 'delete-matching-lines
	    '(string)
	    nil))

(defun rpn-delete-non-matching-lines ()
  "Delete lines that do not match the top element of the string stack."
  (interactive)
  (rpn-call 'delete-non-matching-lines
	    '(string)
	    nil))

(defconst rpn-voice-commands
  '(show-stacks
    ("push region" . rpn-push-region)
    ("insert top" . rpn-pop-insert)
    ("exchange tops" . rpn-exch)
    ("dup top" . rpn-dup)
    ("pop top" . rpn-pop)
    ("edit top" . rpn-edit-top)
    ("blank top" . rpn-edit-top-from-blank)
    ("done" . exit-recursive-edit)
    ("search forward" . rpn-search-forward)
    ("search backward" . rpn-search-backward)
    ("replace string" . rpn-replace-string)
    ("replace regexp" . rpn-replace-regexp)
    ("remove matching lines" . delete-matching-lines)
    ("remove non matching lines" . delete-non-matching-lines)
)
  "The voice commands defined by the rpn-edit package.")
