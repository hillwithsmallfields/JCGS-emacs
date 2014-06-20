;;;; delete-as-spam.el -- delete an email, and mark it as spam
;;; Time-stamp: <2005-02-25 08:18:47 john>

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

(provide 'vm-delete-as-spam)

(defun vm-delete-as-spam ()
  "Delete the current message, marking its sender as a spammer."
  (interactive)
  (let ((record (or (bbdb/vm-update-record t) (error ""))))
    (bbdb-record-putprop record
			 'notes
			 "spam"))
  (vm-delete-message 1))

;;; end of vm-delete-as-spam.el
