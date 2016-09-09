;;; scp-on-save.el --- Copy a file to another machine each time it is saved  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: convenience, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I couldn't get tramp to run on my work setup, and it wasn't quite
;; what I wanted anyway, as I want to have a local file as the master
;; copy and just slave it to the remote.

;;; Code:

(defvar jcgs/scp-target-host nil
  "The host to copy this file to.")

(make-variable-buffer-local 'jcgs/scp-target-host)

(defvar jcgs/scp-target-directory nil
  "The directory to copy this file to on `jcgs/scp-target-host'.")

(make-variable-buffer-local 'jcgs/scp-target-directory)

(defun jcgs/scp-on-save-function ()
  "Copy a file to a remote directory each time it is saved.
Meant for use on `after-save-hook', and established by
`jcgs/scp-on-save-setup'."
  (message "Copying %s to %s:%s" 
	   (buffer-file-name)
	   jcgs/scp-target-host
	   jcgs/scp-target-directory)
  (shell-command (format "scp -q %s %s:%s"
			 (buffer-file-name)
			 jcgs/scp-target-host
			 jcgs/scp-target-directory))
  (message "Copied %s to %s:%s" 
	   (buffer-file-name)
	   jcgs/scp-target-host
	   jcgs/scp-target-directory))

(defun jcgs/scp-on-save-setup (host directory)
  "Set up scp-on-save to HOST and DIRECTORY for the file in this buffer."
  (interactive "sCopy to host: \nsCopy to directory on %s: ")
  (setq jcgs/scp-target-host host
	jcgs/scp-target-directory directory)
  (add-hook 'after-save-hook 'jcgs/scp-on-save-function t t))

(defun jcgs/scp-on-save-cancel ()
  "Cancel scp-on-save for this buffer."
  (interactive)
  (remove-hook 'after-save-hook 'jcgs/scp-on-save-function t))

(provide 'scp-on-save)
;;; scp-on-save.el ends here
