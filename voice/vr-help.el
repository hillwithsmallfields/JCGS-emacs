;;;; vr-help.el -- extended help commands for vr-mode
;;; Time-stamp: <2007-06-28 13:27:59 jcgs>

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

(provide 'vr-help)

(defun vr-flatten-command-list (x &optional y)
  "Flatten X, expanding symbols to pairs as needed. Y is a cons used as an accumulator."
  (if (null x)
      x
    (progn
      (if (null y) (setq y (cons nil nil)))
      (while x
	(let ((x1 (car x)))
	  (message "X1=%S" x1)
	  (if (symbolp x1)
	      (progn
		(message "Symbol %S" x1)
		(if (commandp x1)
		    (progn
		      (message "Command %S" x1)
		      (rplacd y (cons
				 (cons (symbol-name x1)
				       x1)
				 (cdr y))))
		  (progn
		    (message "recursing %S" x1)
		    (vr-flatten-command-list (symbol-value x1) y))))
	    (rplacd y
		    (cons x1 y))))
	(setq x (cdr x)))
      (cdr y))))
    
(defun vr-modal-command-help (&optional mode)
  "List voice commands associated with this mode.
Called from a program,you can specify the mode."
  (interactive)
  (if (null mode) (setq mode major-mode))
  (let ((commands (vr-flatten-command-list (get mode 'voice-commands))))
    (if commands
	(with-output-to-temp-buffer (format "*Voice commands for %s*" mode)
	  (let ((formatstring (format " %%%ds: %%s\n"
				      (apply 'max
					     (mapcar 'length
						     (mapcar 'car commands))))))
	    (while commands
	      (let ((command (car commands)))
		(princ (format formatstring (car command) (cdr command))))
	      (setq commands (cdr commands)))))
      (error "No specific voice commands for %s" mode))))


(defvar vr-in-grouped-command-help nil
  "Controls setting up buffer in vr-grouped-command-help.")

(defun car-length (thing)
  "The length of cars of THING, with 0 for those that have no length."
  (if (consp thing)
      (let ((thecar (car thing)))
	(if (or (stringp thecar) (listp thecar))
	    (length thecar)
	  0))
    0))
		   
(defun vr-grouped-command-help (&optional symbol margin)
  "List voice commands in groups. Each group is those words specified in one call to vr-startup.
Called from a program,you can specify the symbol from which to get them."
  (interactive)
  (if (null symbol) (setq symbol 'vr-voice-command-list))
  (if (null margin) (setq margin 0))
  (if vr-in-grouped-command-help
      (let ((commands (symbol-value symbol))
	    (marginstring (cond
			   ((integerp margin) (make-string margin ? ))
			   ((stringp margin) margin)
			   (t ""))))
	(princ (format "%sCommands under %S:\n" marginstring symbol))
	(if commands
	    (let ((formatstring (format "%s%%%ds: %%s\n"
					marginstring
					(apply 'max
					       (mapcar 'car-length
						       commands)))))
	      (while commands
		(let ((command (car commands)))
		  (cond
		   ((condition-case e
			(consp
			 ;; use consp rather than listp, because nil means start with the top-level list
			 (symbol-value command))
		      ('error nil))
		    (vr-grouped-command-help command (if (integerp margin)
							 (+ margin 4)
						       margin))
		    )
		   ((symbolp command)
		    (princ (format formatstring (vr-strip-dash command) (symbol-name command))))
		   ;; note: (commandp <vector>) --> t
		   ((and (consp command) (vectorp (cdr command)))
		    (princ (format formatstring (car command) (cdr command))))
		   ((and (consp command) (symbolp (cdr command)))
		    (princ (format formatstring (car command) (symbol-name (cdr command)))))
		   (t
		    (error "Unknown vr-voice-command-list element %s"
			   command))))
		(setq commands (cdr commands))))
	  (error "No specific voice commands for %s" symbol))
	(princ "\n"))
    (let ((vr-in-grouped-command-help t))
      (with-output-to-temp-buffer (format "*Voice commands under %s*" symbol)
	(vr-grouped-command-help symbol margin)))))

(defun vr-html-command-help (words command)
  "Document the command done in response to WORDS, which is COMMAND."
  (let* ((symbol (if (stringp command) (intern command) command))
	 (documentation (if (commandp symbol) (documentation symbol) nil)))
    (insert (format "  <dt> <b>%s</b> <code>[%s]</code>\n" words command))
    (if (stringp documentation)
	(progn
	  (setq documentation (substring documentation 0 (string-match "\n" documentation)))
	  (insert (format "  <dd> %s\n" documentation)))
      (insert "  <dd>\n"))))

(defvar vr-commands-file  "~/www/vr-commands.html"
  "Where to put the commands page.")

(defun vr-html-grouped-command-help (&optional symbol level)
  "Document voice commands in groups.
Each group is those words specified in one call to vr-startup.
Called from a program, you can specify the symbol from which to get them."
  (interactive)
  (if (null symbol) (setq symbol 'vr-voice-command-list))
  (if (null level) (setq level 2))
  (if vr-in-grouped-command-help
      (let ((commands (symbol-value symbol)))
	(insert (format "<h%d>Commands under %S</h%d>\n" level symbol level))
	(insert "<dl>\n")
	(if t
	    (let ((group-doc (get symbol 'variable-documentation)))
	      (if (stringp group-doc)
		(insert "<p>" group-doc "</p>\n"))))
	(if commands
	    (let ()
	      (insert (format "<!-- starting %S -->" commands))
	      (while commands
		(let ((command (car commands)))
		  (cond
		   ((condition-case e
			(consp
			 ;; use consp rather than listp, because nil means start with the top-level list
			 (symbol-value command))
		      ('error nil))
		    (vr-html-grouped-command-help command (if (integerp level)
							      (1+ level)
							    level))
		    )

		   ((symbolp command)
		    (vr-html-command-help (vr-strip-dash command) (symbol-name command)))

		   ;; note: (commandp <vector>) --> t
		   ((and (consp command) (vectorp (cdr command)))
		    (vr-html-command-help (car command) (cdr command)))

		   ((and (consp command) (symbolp (cdr command)))
		    (vr-html-command-help (car command) (symbol-name (cdr command))))

		   (t
		    (error "Unknown vr-voice-command-list element %s"
			   command))))
		(setq commands (cdr commands)))
	      (insert (format "<!-- done %S -->" commands)))
	  (error "No specific voice commands for %s" symbol))
	(insert "</dl>\n"))
    (let ((vr-in-grouped-command-help t))
      (find-file vr-commands-file)
      (erase-buffer)
      (insert "<html>\n<head>\n<title>My emacs voice commands</title>\n</head>\n<body>\n<h1>My emacs voice commands</h1>\n\n")
      (insert "<p>This command list was generated at " (current-time-string)
	      " on " (system-name)
	      " by " user-full-name
	      " using emacs " emacs-version
	      ".</p>\n")
      (vr-html-grouped-command-help symbol level)
      (insert "</body>\n</html>\n")
      (basic-save-buffer))))

;;; end of vr-help.el
