;;;; smart-repeat.el -- enhanced command-history commands
;;; Time-stamp: <2007-06-28 13:38:52 jcgs>

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

(defun smart-repeat-complex-command ()
  (interactive)
  "Like repeat-complex-command, but may skip the first one if it would do nothing."
  (let* ((rest command-history)
	 (from 1))
    (while (and rest
		(let* ((last-command (car rest))
		       (last-command-function (car last-command))
		       (last-command-first-arg (cadr last-command)))
		  (or (and (eq last-command-function 'switch-to-buffer)
			   (string= last-command-first-arg
				    (buffer-name)))
		      (and (memq last-command-function '(find-file find-file-other-window))
			   (string= last-command-first-arg
				    (buffer-file-name)))) ))
      (setq from (1+ from)
	    rest (cdr rest)))
    (repeat-complex-command from)))

(provide 'smart-repeat)

;;; end of smart-repeat.el
