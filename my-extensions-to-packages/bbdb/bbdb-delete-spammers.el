;;;; bbdb-delete-spammers.el -- remove entries for spammers
;;; Time-stamp: <2012-03-08 17:25:18 johnstu>

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

(require 'bbdb-iterators)
(provide 'bbdb-delete-spammers)

(defun bbdb-delete-spammers ()
  "Delete records whose notes field just says spam."
  (interactive)
  (bbdb:apply-to-records-defining 
   (lambda (record)
     (let ((name (bbdb-record-name record))
	   (notes (bbdb-record-getprop record 'notes)))
       (message "notes: \"%s\"" notes)
       (if (and (string-match "spam" notes)
		(not (bbdb-record-deleted-p record)))
	   (progn 
	     (message "Spammer: %s" name)
	     (bbdb-delete-record-internal record)
	     (bbdb-record-set-deleted-p record t)
	     t)
	 nil
	 )))
   'notes))

;;; end of bbdb-delete-spammers.el
