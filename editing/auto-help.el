;;;; auto-help.el -- provide help automatically, as the cursor moves around
;;; Time-stamp: <2005-08-05 21:40:31 jcgs>

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

(provide 'auto-help)

(defvar auto-help-last-symbol nil
  "The last symbol for which auto-help was given.")

(defvar auto-help-active t
  "*Whether auto-help is active.")

(defvar auto-help-full nil
  "*Whether auto-help should pop up a help window. Otherwise, it uses the echo area.")

(defun auto-help ()
  "If the cursor has moved to a different thing for which we have some help information, display the new help."
  (condition-case problem
      (let ((current (intern-soft (thing-at-point 'symbol))))
	;; (message "Autohelp got %S" current)
	(if (and (symbolp current)
		 (not (eq current auto-help-last-symbol)))
	    (let ((doc (or (documentation-property current 'variable-documentation)
			   (and (functionp current)
				(documentation current)))))
	      (if doc
		  (if auto-help-full
		      (with-output-to-temp-buffer "*Help*"
			(princ doc))
		    (message "%s" (substring doc 0
					     (string-match "\n" doc))))))))
    (error (message "Error in auto-help: %S" problem)
	   (backtrace))))

;;; end of auto-help.el
