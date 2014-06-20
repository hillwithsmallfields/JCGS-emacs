;;;; mail-insert.el -- handy things to insert in outgoing mail
;;; Time-stamp: <2004-12-04 13:16:49 jcgs>

;;; By John Sturdy <john@cb1.com>

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

(provide 'mail-inserts)

(defvar mail-insert:publicized-homepage-URL
  "http://www.cb1.com/~john/John.html"
  "The URL I wish to announce as my homepage in outgoing mail.")

(defun mail-insert:homepage-URL ()
  "Insert my homepage URL."
  (interactive)
  (insert mail-insert:publicized-homepage-URL))

(defun mail-insert:offline-message (file)
  "Fill in this message buffer with a message prepared in FILE
which should have a blank line between header and body."
  (interactive "fInsert message from file: ")
  (insert-file-contents file nil nil nil t)
  (goto-char (point-min))
  (if (re-search-forward "^$")
      (progn
	(insert mail-header-separator))))

;; Add wisdom to an outgoing message:

(defun mail-add-wisdom ()
  "Add some wisdom to the message in the current buffer."
  (interactive)
  (require 'aw)
  (save-excursion
    (goto-char (point-max))
    (let ((wisdom (aw:best-quote-about-region
		   (point-min) (point-max)
		   "proverbs")))
      (if wisdom
	  (let* ((tag-match (string-match "\\(\\[[0-9:]+\\]\\) +" wisdom))
		 (tag-text (if tag-match (substring wisdom
						    (match-beginning 1)
						    (match-end 1))))
		 )
	    (if tag-match (setq wisdom (substring wisdom (match-end 1))))
	    (if tag-match
		(insert "From Proverbs " tag-text ":\n"))
	    (insert wisdom)
	    )
	(message "No wisdom found about this")))))

;;; end of mail-inserts.el
