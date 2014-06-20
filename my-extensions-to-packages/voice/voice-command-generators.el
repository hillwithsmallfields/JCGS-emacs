;;;; voice-command-generators.el
;;; Time-stamp: <2004-12-15 09:21:35 jcgs>
;;; Generate voice commands based on various things

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

(provide 'voice-command-generators)

(defun dashes-to-spaces (string)
  "Return STRING with any dashes converted to spaces."
  (substitute ?  ?- string :test 'eq))

(defun voice-command-family-setup (pattern symbol)
  "Set up a family of voice commands matching PATTERN, naming the set SYMBOL."
  (set symbol (mapcar (function (lambda (name)
				  (let ((namestring (symbol-name name)))
				    (string-match pattern namestring)
				    (cons (dashes-to-spaces
					   (substring namestring
						     (match-beginning 1)
						     (match-end 1)))
					  name))))
		      (apropos-internal pattern 'commandp))))

;; abandoned for now -- horribly complex! might look for some other code to reuse for this
(defun voice-commands-from-keymap (keymap symbol)
  "Generate voice commands from the commands in KEYMAP, putting result on SYMBOL."
  ;; (set symbol nil)
  (cond 
   ((keymapp keymap)
    (cond ((vectorp (second keymap))
	   )
	  ((stringp (second keymap))
	   ;; menu
	   (voice-commands-from-keymap (cdr (cdr keymap)))
	   )
	  ((consp (second keymap))
	   (voice-commands-from-keymap (cdr keymap))
	   )
	  (t
	   (error "unknown keymap entry type for voice-commands-from-keymap"))))
   ((listp keymap)
    (dolist (entry keymap)
      ))))

;;; end of voice-command-generators.el
