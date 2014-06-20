;;;; journal-at-exit.el -- stuff for keeping a diary as you quit emacs
;;; Time-stamp: <2005-02-01 15:00:30 john>

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

(provide 'journal-at-exit)
(require 'journal)

(defun journal-at-exit ()
  "If you have not used the journal code this session, ask for a journal entry.
Meant for calling from the exit sequence."
  (if (not journal-made-entry)
      (if (y-or-n-p "Make journal entry? ")
	  (progn
	    (require 'journal)
	    (call-interactively 'journal-new-day)
	    (message
	     (substitute-command-keys
	      "Make journal entry, \\[exit-recursive-edit] to finish"))
	    (recursive-edit)
	    (basic-save-buffer)))))
