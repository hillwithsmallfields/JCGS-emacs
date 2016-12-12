;;;; voicescript.el
;;; Time-stamp: <2004-12-15 09:21:35 jcgs>

;;; This is to handle words apparently entered as continuous text but actually
;;; representing a stream of commands.

;;; It is designed to go with rpn-edit.el, although either will
;;; work without the other being loaded (at the time of writing).

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

(provide 'voicescript)
(require 'command-phrases)
(require 'cl)

(defun vs-script ()
  "Begin a new script.
If not already in a script window, create and switch to one.
If already in one, open a scripting block.
See the documentation of voicescript-mode for details."
  (interactive)
  (unless (eq major-mode 'voicescript)
    (vs-create-script-buffer vs-script-level))
  (vs-on-new-line)
  (insert "{")
  (save-excursion
    (vs-on-new-line)
    (insert "}"))
  (vs-on-new-line))

(defun vs-new-script ()
  "Begin a new script.
Normally for use to write a script to edit a script that
you are currently entering."
  (interactive)
  (vs-create-script-buffer (1+ vs-script-level))
)

(defun vs-create-script-buffer (level)
  "Create a script buffer at LEVEL.
LEVEL is 0 for a top-level script, 1 for a script for editing level 0 scripts, etc."
  (let* ((window-config (current-window-configuration))
	 (buffer (get-buffer-create (format "VoiceScript-%d" level))))
    (set-buffer buffer)
    (erase-buffer)
    (voicescript-mode)
    (setq vs-pre-recording-window-config window-config
	  vs-script-level (1+ vs-script-level))
    (pop-to-buffer buffer)))

(defun vs-leave-script ()
  "Move to just after the current script."
  (interactive)
  (backward-up-list -1))

(defvar vs-editing nil
  "Whether we are currently editing the script.
If editing it, we take action on hearing editing commands;
otherwise they go into the script.")

(make-variable-buffer-local 'vs-editing)

(defun vs-edit-script ()
  "Switch from entering the script to editing it."
  (interactive)
  (setq vs-editing t))

(defun vs-enter-script ()
  "Switch from editing the script to entering it."
  (interactive)
  (setq vs-editing nil))

(defvar vs-pre-recording-window-config nil
  "The window configuration before recording commands.")

(make-variable-buffer-local 'vs-pre-recording-window-config)

(defvar vs-script-level 0
  "Number of script we are in.
This is the whole script, for use of scripts for editing scripts.")

(make-variable-buffer-local 'vs-script-level)

(defvar vs-live-commands
  '(vs-run vs-script vs-new-script vs-edit-script vs-enter-script)
  "Commands that should be run immediately when in voicescript-mode.
Other commands are entered into the script.")

(defun voicescript-heard-command-hook-function (cmd)
  "Intercept normal vr-mode command processing.
This is so that commands spoken into a script are put there rather
than acted on immediately, except for those needed to control the
scripting system."
  (if (eq major-mode 'voicescript-mode)
      (progn
	(message "Got %S while in voicescript-mode" cmd)
	(if (or vs-editing (memq cmd vs-live-commands))
	    nil
	  (insert
	   (car (rassoc cmd vr-commands-registered))
	   " ")
	  t))
    nil))

(add-hook 'vr-action-heard-command-hook 'voicescript-heard-command-hook-function)

(defvar voicescript-mode-map
  (make-keymap "VoiceScript")
  "Keymap for VoiceScript mode.")

(define-key voicescript-mode-map "\C-M-x" 'vs-run)

(defun voicescript-mode ()
  "Major mode for editing voicescript.

VoiceScript is a simple scripting language for driving emacs through
voice recognition. It is built on top of vr-mode, and, as a
programming language, is based on PostScript, a stack-based language
designed for use with printers and typesetters.

The main reason for using VoiceScript is that you will often want to
issue a stream of commands, but to have them recognized as commands,
you must pause before and after each. VoiceScript lets you enter them
as a continuous stream (which Dragon / vr-mode / emacs accumulates as
a piece of text, as it always does for continuous streams of words)
and then splits them into commands as defined by vr-mode.

Having got that far, it might as well have some simple scripting
facilities, and PostScript provides a way to do this with very little
further syntax.

Voicescript commands are any word sequences that can be understood as
commands defined through vr-mode, plus a few more that are specific to
the VoiceScript language for things such as loops."
  (interactive)
  (fundamental-mode)
  (setq major-mode 'voicescript-mode
	mode-name "VoiceScript"
	comment-column 48
	comment-start "%"
	comment-start-skip "%+ *"
	comment-end ""
	)
  (use-local-map voicescript-mode-map)
)

(defun vs-on-new-line ()
  "Make sure we are at the start of a new line."
  (let* ((here (point))
	 (non-blank (save-excursion
		      (beginning-of-line 1)
		      (re-search-forward "[^ \t]" here t))))
    (when non-blank
      ;; (end-of-line 1)
      (insert ?\n)
      (vs-indent-line))))

(defun vs-nesting-depth ()
  "Return the depth of the current point."
  (save-excursion
    (let ((depth 0)
	  (start (point))
	  char)
      (goto-char (point-min))
      (while (and (skip-chars-forward "^{}" start)
		  (< (point) start)
		  (setq char (char-after (point))))
	(cond
	 ((eq char ?{)
	  (incf depth))
	 ((eq char ?})
	  (decf depth)))
	(goto-char (1+ (point))))
      depth)))

(defun vs-indent-line ()
  "Indent the current line."
  (interactive)
  (indent-line-to (* 4 (vs-nesting-depth))))

(defstruct vs-context
  ;; A context is rather like PostScript's context; however, the
  ;; operand stack is spread out between the kill-ring, the mark-ring
  ;; and so on, as defined in rpn-edit.el
  execution-stack
  dictionary-stack)

(defun vs-run ()
  "Run the current script."
  (interactive)
  (unless (eq major-mode 'voicescript-mode)
    (error "Not in a VoiceScript"))
  (setq coph:parse-errors 0)
  (coph:ensure-tree)
  (let ((window-config vs-pre-recording-window-config)
	(context (make-vs-context))
	(script (coph:parse-script (point-min))))
    (set-window-configuration window-config)
    (if (= coph:parse-errors 0)
	(vs-eval script context)
      (error "Error in script"))))

(defvar vs-trace nil
  "Whether to record the actions taken by vs-eval.")

(defun vs-eval (script context)
  "Evaluate SCRIPT in CONTEXT."
  (if (eq (car script) 'voicescript)
      (setq script (cdr script))
    (error "Not a VoiceScript"))
  (catch 'voicescript-exit
    (let ((quote-pending nil)
	  (vs-trace-buffer (if vs-trace
			       (get-buffer-create "*VS trace*")
			     nil))
	  )
      (if vs-trace-buffer
	  (let ((thisbuff (current-buffer)))
	    (set-buffer vs-trace-buffer)
	    (erase-buffer)
	    (set-buffer thisbuff)))
      (while script
	(let ((x (car script)))
	  (if vs-trace-buffer
	      (let ((standard-output vs-trace-buffer))
		(print x)))
	  (cond
	   ((symbolp x)
	    (cond
	     (quote-pending
	      (setq quote-pending nil)
	      (push x (vs-context-execution-stack context)))
	     ((eq x 'symbol)
	      (setq quote-pending t))
	     (t (funcall x))))
	   ((numberp x) (push x rpn-number-stack))
	   ((stringp x) (kill-new x))
	   ((consp x)
	    ;; this will either be a (voicescript ... . . ...) or a (symbol ...);
	    ;; either way, it goes on the stack
	    (push x (vs-context-execution-stack context)))))
	(setq script (cdr script)))
      (if vs-trace-buffer
	  (pop-to-buffer vs-trace-buffer)))))

(defvar vs-command-list
  '(("begin script" . vs-script)
    ("new script" . vs-new-script)
    ("leave script" . vs-leave)
    ("edit script" . vs-edit-script)
    ("resume script" . vs-enter-script)
    ("run script" . vs-run))
  "Voice commands for the VoiceScript system.")

(defun vs-test ()
  "Test some VoiceScript stuff."
  (interactive)
  (let (
	(vr-commands-registered
	 '(("stop the train" . alarm-cord)
	   ("stop the world" . desparation)
	   ("stop thief" . security)
	   ("stop signal" . control)
	   ("start the party" . champagne)
	   ("start here" . directions))))
    (coph:ensure-tree)
    (set-buffer (get-buffer-create "*vs test*"))
    (erase-buffer)
    (voicescript-mode)
    (let ((source "{stop the train thingy whatsit {stop the world start the party} 999 stop thief /testing start here}"))
      (insert source)
      (let ((result (coph:parse-script (point-min))))
	(with-output-to-temp-buffer "*parse result*"
	  (princ
	   (format "Grammar: %S\n\nSource: %S\n\nResult: %S\n"
		   coph:phrase-tree
		   source
		   result)))))))

;;; end of voicescript.el
