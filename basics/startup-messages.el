;;; Time-stamp: <2006-01-29 21:48:14 jcgs>

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

(defun preserve-startup-messages ()
  "Preserve the startup messages."
  (set-buffer (get-buffer-create "*Startup Messages*"))
  (insert-buffer "*Messages*")
  (goto-char (point-min))
  (let ((problems nil))
    (if (and (boundp 'emacs-major-version)
	     (>= emacs-major-version 20))
	(while (re-search-forward "Problem in .+$" (point-max) t)
	  (setq problems (cons (match-string-no-properties 0) problems))
	  (put-text-property (match-beginning 0) (match-end 0) 'face (cons 'background-color "yellow"))))
    (setq message-log-max 2000)
    (if problems
	(with-output-to-temp-buffer "*Startup Problems*"
	  (setq problems (nreverse problems))
	  (while problems
	    (princ (car problems))
	    (princ "\n")
	    (setq problems (cdr problems)))))))

;;; end of startup-messages.el
