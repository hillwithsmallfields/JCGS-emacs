;;;; with-saved-messages.el -- run a command and save the messages it produces
;;; Time-stamp: <2006-03-31 11:58:30 jcgs>

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

(provide 'with-saved-messages)

(defun with-saved-messages (flag-var command buffer-name)
  "With FLAG-VAR set to t, run COMMAND, saving its messages in BUFFER-NAME."
  (interactive "SFlag variable: 
CCommand: 
sBuffer for messages: ")
  (save-excursion
    (set-buffer "*Messages*")
    (erase-buffer))
  (let ((old-value (symbol-value flag-var))
	(message-log-max t))
    (set flag-var t)
    (call-interactively command)
    (set flag-var old-value)
    (save-excursion
      (set-buffer (get-buffer-create buffer-name))
      (erase-buffer)
      (insert-buffer-substring "*Messages*")
      (goto-char (point-min)))))

;;; end of with-saved-messages.el
