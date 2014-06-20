;;;; mailrc-mode.el -- major mode for editing .mailrc files
;;; Time-stamp: <2004-12-04 13:16:49 jcgs>

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

;;; By John Sturdy <john@cb1.com>

(provide 'mailrc-mode)

(defvar mailrc-mode-map (make-sparse-keymap "Mailrc")
  "Keymap for mailrc-mode")

(define-key mailrc-mode-map "\M-m" 'mailrc-add-address-to-mail)

(defun mailrc-reset ()
  "Make the mail aliases list be re-made before its next use."
  (setq mail-aliases t)
  nil)

(defun mailrc-mode ()
  "The beginnings of a mode for mail alias files.
For now, the main this is to reset the mail aliases on writing the file."
  (interactive)
  (fundamental-mode)
  (setq major-mode 'mailrc-mode
	mode-name "MailRC")
  (use-local-map mailrc-mode-map)
  (add-hook 'local-write-file-hooks 'mailrc-reset))

(defun mailrc-add-address-to-mail ()
  "Add the address on the current line to the message being composed in *mail*"
  (interactive)
  (if (not (get-buffer "*mail*"))
      (save-window-excursion
	(compose-mail)))
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at "alias +\\([^ ]+\\) +")
	(let ((alias (match-string-no-properties 1)))
	  (message "Got %s" alias)
	  (save-window-excursion
	    (set-buffer "*mail*")
	    (mail-to)
	    (beginning-of-line 1)
	    (if (looking-at "^To: .+$")
		(progn
		  (mail-to)
		(insert ", ")))
	    (mail-to)
	    (insert alias))
	  )
      (error "Not on an alias line"))))

;;; end of hooks/mailrc.el
