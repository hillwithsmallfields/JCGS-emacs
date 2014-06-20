;;;; insert-voice.el
;;; Time-stamp: <2006-11-15 22:47:10 jcgs>
;;; Make voice insertion entries, possibly including some based on the handsfree insertion menu

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

(provide 'insert-voice)
(require 'handsfree)
(require 'voice-command-generators)

(defun insert-text-quotes ()
  "Insert opening and closing quotes, and position point between them."
  (interactive)
  (if (not (or (bolp)
	       (looking-at "[>(]")))
      (just-one-space))
  (insert "``''")
  (backward-char 2))

(defvar vr-insert-commands
  '(("parentheses" . insert-parentheses)
    ;; leave-parentheses
    ("text quotes" . insert-text-quotes)
    other-window-file-name
    other-window-directory-name
    ("linefeed" . newline-and-indent)
    ("latest" . yank)
    ("yank-menu" . electric-yank-menu)
    ("expand" . dabbrev-expand)
    hyphenate
    )
  "Voice commands for insertions.")

(defun safe-char-p (ch)
  (or (and (<= ?a ch) (<= ch ?z))
      (and (<= ?A ch) (<= ch ?Z))))

(defvar vr-character-pronunciations
  '((?* . " star ")
    (?- . " dash ")
    (?% . " percent ")
    (?[ . " open square bracket ")
    (?] . " close square bracket ")
    (?{ . " open curly bracket ")
    (?} . " close curly bracket ")
    (?( . " open bracket")
    (?) . " close bracket")
    (?& . " ampersand ")
    (?! . " exclamation mark ")
    (?` . " back quote ")
    (?< . " less than ")
    (?> . " greater than ")
    (?= . " equal ")
    (?. . " dot ")
    (?, . " comma ")
    (?; . " semicolon ")
    (?: . " colon ")
    (?? . " question mark ")
    (34 . " double quote ")
    (39 . " single quote ")
    (?+ . " plus ")
    (?/ . " slash ")
    )
  "How to pronounce various characters.")

(defun pronounceable-char (char)
  "Convert CHAR to something pronounceable"
  (let ((new (cdr (assoc char vr-character-pronunciations))))
    (if new
	new
      (char-to-string char))))

(defun pronounceable-name (name)
  "Convert NAME to something pronounceable"
  (mapconcat 'pronounceable-char
	     name
	     ""))

(defun voice-commands-from-menu-tree (tree symbol)
  "Generate voice commands from TREE putting them onto SYMBOL."
  (cond
   ((vectorp tree)
    (let* ((namestring (aref tree 0))
	   (safestring (pronounceable-name namestring))
	   (name (intern (concat "vr-insert-" 
				 safestring)))
	   (action `(lambda ()
		      ,(format "Insert a %s construct." namestring)
		      (interactive)
		      ,(aref tree 1))))
      (message "Would like to define %S to do %S" name action)
      (fset name action)
      (set symbol (cons (cons (dashes-to-spaces safestring)
			      name)
			(eval symbol)))))
   ((consp tree)
    (while tree
      (voice-commands-from-menu-tree (car tree) symbol)
      (setq tree (cdr tree))))
   (t nil)))

(defvar vr-insert-menu-commands nil
  "Insertion menu commands, from the handsfree insertion menu")

(if (and nil (null vr-insert-menu-commands))
    (voice-commands-from-menu-tree 
     handsfree-insert-menu-items
     'vr-insert-menu-commands))

;;; end of insert-voice.el
