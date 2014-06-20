;;;; mail-directory -- set up mail buffers for sending photos
;;; Time-stamp: <2007-08-22 15:57:32 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: not sure
;; Keywords: email

;; This file is NOT part of GNU Emacs.

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

(provide 'mail-directory)
(require 'cl)

(defun mail-directory (dir to preamble)
  "Mail files in DIR to TO."
  (interactive "DDirectory: 
sMail to: 
sPreamble to filename in subject lines: ")
  (dolist (file (directory-files dir t))
    (unless (file-directory-p file)
      (mail nil
	    to
	    (concat preamble (file-name-nondirectory file))
	    nil
	    "jcgs@cb1.com")
      (goto-char (point-max))
      (insert-file-contents file)
      (mail-send))))

(defvar mail-photo-from-dir-files nil
  "Directory to send photos from.")

(defvar mail-photo-from-dir-to-address nil
  "Who to send photos to.")

(defun mail-photo-from-dir ()
  "Mail a photo to someone, from a directory of photos.
Lets you work through a sequence of them.
Meant for use from vm's mail sending buffers."
  (interactive)
  (unless mail-photo-from-dir-files
    (setq mail-photo-from-dir-to-address (read-from-minibuffer "Send to: ")
	  mail-photo-from-dir-files (directory-files
				     (read-file-name "Send pictures from directory: ")
				     t
				     "\\.jpe?g$")))
  (let ((file (car mail-photo-from-dir-files)))
    (mail-to) (insert mail-photo-from-dir-to-address)
    (mail-subject) (insert (file-name-nondirectory file))
    (goto-char (point-max))
    (vm-mime-attach-file file "image/jpeg")
    (setq mail-photo-from-dir-files
	  (cdr mail-photo-from-dir-files)))
  (vm-mail-send-and-exit nil)
  )

;;; end of mail-directory.el
