;;;; flexi-choose-voice.el
;;; Time-stamp: <2006-01-25 18:16:28 jcgs>

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

(provide 'flexi-choose-voice)

(defvar flexi-choose-voice-name-number-pairs
  '((\0 . 0)
    (nought . 0)
    (zero . 0)
    (\1 . 1)
    (one . 1)
    (ein . 1)
    (un . 1)
    (une . 1)
    (uno . 1)
    (line-1 . 1)
    (\2 . 2)
    (two . 2)
    (zwei . 2)
    (zwo . 2)
    (tvaa . 2)
    (line-2 . 2)
    (\3 . 3)
    (three . 3)
    (drei . 3)
    (line-3 . 3)
    (\4 . 4)
    (four . 4)
    (line-4 . 4)
    (\5 . 5)
    (funf . 5)
    (five . 5)
    (line-5 . 5)
    (\6 . 6)
    (six . 6)
    (zes . 6)
    (line-6 . 6)
    (\7 . 7)
    (seven . 7)
    (sieben . 7)
    (line-7 . 7)
    (\8 . 8)
    (eight . 8)
    (acht . 8)
    (line-8 . 8)
    (\9 . 9)
    (nine . 9)
    (neun . 9)
    (line-9 . 9)
    (\10 . 10)
    (ten . 10)
    (line-10 . 10)
    (\11 . 11)
    (eleven . 11)
    (line-11 . 11)
    (\12 . 12)
    (twelve . 12)
    (line-12 . 12)
    (\13 . 13)
    (thirteen . 13)
    (line-13 . 13)
    (\14 . 14)
    (fourteen . 14)
    (line-14 . 14)
    (\15 . 15)
    (fifteen . 15)
    (line-15 . 15)
    (\16 . 16)
    (sixteen . 16)
    (line-16 . 16)
    (\17 . 17)
    (seventeen . 17)
    (line-17 . 17)
    (\18 . 18)
    (eightteen . 18)
    (line-18 . 18)
    (\19 . 19)
    (nineteen . 19)
    (line-19 . 19)
    (\20 . 20)
    (twenty . 20)
    (line-20 . 20)
    (\21 . 21)
    (twenty-one . 21)
    (line-21 . 21)
    (\22 . 22)
    (twenty-two . 22)
    (line-22 . 22)
    (\23 . 23)
    (twenty-three . 23)
    (line-23 . 23)
    (\24 . 24)
    (twenty-four . 24)
    (line-24 . 24)

    )
  "Name-number pairs for selection commands.
Some of them are available in several languages, in case it picks \"for\"
for the sound \"four\", etc.
A dozen brace of numbers should be enough.")

(mapcar (function
	 (lambda (pair)
	   (fset (car pair)
		 `(lambda ()
		    ,(format "If in a flexi-chooser, choose item %d, otherwise set the prefix arg to %d."
			     (cdr pair) (cdr pair))
		    (interactive)
		    (if (and
			 ;; this almost certainly is bound, unless something has gone wrong
			 ;; with loading these subsystems; I put this check in to reduce
			 ;; confusion when that does happen (when it did happen!)
			 (boundp 'in-completing-read-with-history-hack)
			     in-completing-read-with-history-hack)
			(choose-by-number ,(cdr pair))
		      (if (eq major-mode 'Info-mode)
			  (progn
			    (setq this-command-keys (int-to-string ,(cdr pair)))
			    (Info-nth-menu-item))
			  (setq prefix-arg ,(cdr pair))))))))
	flexi-choose-voice-name-number-pairs)

(require 'voice-command-generators)	; for dashes-to-spaces
(defvar vr-flexi-choose-commands
  (mapcar (lambda (pair)
	    (cons (dashes-to-spaces (symbol-name (car pair)))
		  (car pair)))
	  flexi-choose-voice-name-number-pairs)
  "Voice commands for flexi-choose")

;;;; Try to spot words being spoken into the minibuffer during selection

(defvar orig-minibuffer-contents ""
  "What was in the minibuffer when we saw it being set up.")

(defvar orig-minibuffer-contents-length 0
  "The length of what was in the minibuffer when we saw it being set up.")

(defun flexi-choose-voice-minibuffer-setup ()
  "Setup function for minibuffer."
  (if (and (boundp 'in-completing-read-with-history-hack)
	   in-completing-read-with-history-hack)
      (progn
	;; (make-variable-buffer-local 'after-change-function)
	(add-hook 'after-change-functions 'flexi-choose-voice-minibuffer-change nil t)
	(setq ;; after-change-function 'flexi-choose-voice-minibuffer-change
	      orig-minibuffer-contents (buffer-substring-no-properties
					(point-min) (point-max))
	      orig-minibuffer-contents-length (1+ (length orig-minibuffer-contents)))
	;; (message "Setting up minibuffer for flexi-choose-voice, contains %s at %d chars" orig-minibuffer-contents (length orig-minibuffer-contents))
	)))

(defvar flexi-choose-voice-minibuffer-change-needs-chars 4
  "*If at least this many characters have been added to the initial string in the minibuffer,
search for a match for the added text.")

(defvar flexi-choose-voice-minibuffer-act-on-detecting-word t
  "*Whether to throw every interesting word we find.
Might prove annoying, because of initial substrings.")

(defun flexi-choose-voice-minibuffer-change (start end length)
  "Note a change in the minibuffer."

  ;; maybe should advise minibuffer-complete-word instead

  (if (and (boundp 'in-completing-read-with-history-hack)
	   in-completing-read-with-history-hack)
      (let ((text (flexi-choose-voice-minibuffer-extra-text)))
	;; todo: this is obviously work in progress, was I trying to make it exit on getting something complete?
	;; (message "extra on adding character is %s" (flexi-choose-voice-minibuffer-extra-text))
	(when (and flexi-choose-voice-minibuffer-act-on-detecting-word
		   (stringp text)
		   (member (downcase text)
			   interesting-words))
	  (throw 'filter (list text))))))

(defun flexi-choose-voice-minibuffer-extra-text ()
  "Return whatever extra text been added to the minibuffer.
If the original text has been changed, rather than just added to, return nil.
Likewise, if fewer than flexi-choose-voice-minibuffer-change-needs-chars characters
have been added, return nil."
  (if (and (>= (buffer-size) (+ flexi-choose-voice-minibuffer-change-needs-chars orig-minibuffer-contents-length))
	   (string= (buffer-substring-no-properties (point-min) (min orig-minibuffer-contents-length (point-max)))
		    orig-minibuffer-contents))
      (buffer-substring-no-properties orig-minibuffer-contents-length (point-max))
    nil))

(add-hook 'minibuffer-setup-hook 'flexi-choose-voice-minibuffer-setup)

(defun minibuffer-completing-space ()
  "Function for space char in minibuffers with completion"
  (interactive)
  (if (and (boundp 'in-completing-read-with-history-hack)
	   in-completing-read-with-history-hack)
      (let ((extra (flexi-choose-voice-minibuffer-extra-text)))
	(if (stringp extra)
	    (throw 'filter (list extra))
	  (minibuffer-complete-and-exit)))
    (minibuffer-complete-word)))

(defun minibuffer-completing-return ()
  "Function for return char in minibuffers with completion"
  (interactive)
  (if (and (boundp 'in-completing-read-with-history-hack)
	   in-completing-read-with-history-hack)
      (let ((extra (flexi-choose-voice-minibuffer-extra-text)))
	;; (message "Got extra text %s" extra)
	(if (stringp extra)
	    (throw 'filter (list extra))
	  (minibuffer-complete-and-exit)))
    (minibuffer-complete-and-exit)))

(defun setup-flexi-choose-minibuffer ()
  (interactive)
  ;; (require 'pedals) ; something goes wrong when I try to do it in this order
  (define-key minibuffer-local-must-match-map " " 'minibuffer-completing-space)
  (define-key minibuffer-local-must-match-map "\r" 'minibuffer-completing-return)
  (define-key minibuffer-local-must-match-map
    ;; pedal-menu ; not yet set up
    [ kp-end ]
    'minibuffer-completing-return))

(setup-flexi-choose-minibuffer)

(defun choose-word-directly (word)
  "Choose WORD directly from current value of all-choices, as defined in choose-in-steps."
  (interactive "sWord: ")
  (message "Called choose-word-directly with %s from %S" word all-choices))

(defun yes-voice ()
  "Return yes to a prompter."
  (interactive)
  (if (not (string-match "Minibuf" (buffer-name)))
      (error "Not in the minibuffer."))
  (erase-buffer)
  (insert "yes")
  (call-interactively (key-binding "\n")))

(defun no-voice ()
  "Return no to a prompter."
  (interactive)
  (if (not (string-match "Minibuf" (buffer-name)))
      (error "Not in the minibuffer."))
  (erase-buffer)
  (insert "no")
  (call-interactively (key-binding "\n")))

;;;; Our top-level commands

(defvar vr-flexi-choose-top-commands
  '(("file tree" . flexi-find-file)
    ("send email" . flexi-mail-to)
    ("web tree" . flexi-raise-web-directory)
    ("screen tree" . flexi-select-screen-setup)
    ("load context" . flexi-load-context)
    ("find library" . flexi-choose-library)
    ("buffer tree" . flexi-switch-to-buffer)
    ("recent buffers" . flexi-switch-to-recent-buffer)
    ("yes to that" . yes-voice)
    ("no to that" . no-voice)
    )
  "Voice commands for invoking flexi-choose")

(defvar vr-choices-per-step nil
  "The number of choices to be available at each step under voice operation of handsfree.
If not a number, half the window height is used, but limited by vr-choices-per-step-limit.")

(defvar vr-choices-per-step-limit 24
  "The maximum that can be presented at one step if going by the frame height.
We must limit this to the highest number of which we know the pronunciation.")

;;; end of flexi-choose-voice.el
