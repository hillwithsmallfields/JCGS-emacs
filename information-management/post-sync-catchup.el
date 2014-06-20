;;; post-sync-catchup.el --- pick up files that might have been transferred to/from phone

;; Copyright (C) 2013  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience

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

;; I copy files between my linux phone and desktop / laptop computers,
;; while leaving an Emacs session running.  This file re-loads buffers
;; visiting files that this process is likely to change.  It can be
;; run on my phone's emacs or my desktop / laptop's emacs.

;; TODO: If the N900's DBUS will tell me the phone's MMC is dismounted as a USB gadget, make that trigger a post-sync-catchup

;;; Code:

(defvar post-sync-catchup-sync-command "fromphone"
  "Command to run to sync from the other machine.")

(defvar post-sync-catchup-script-files-pattern "\\$PHONE/common/\\(\\S-+\\)"
  "Pattern for finding the files that the sync script changes.
The first recorded match string is used.")

(defvar post-sync-catchup-tree (substitute-in-file-name "$COMMON")
  "Directory tree which the filenames to sync are to be resolved.")

(defun post-sync-catchup ()
  "Reload files that might have been changed by syncing with my phone.
This analyzes the script for the command run by
`post-sync-catchup-sync-command' to find what might have
changed."
  (interactive)
  (save-window-excursion
    (let* ((files nil)
	   (sync-script-file
	    (car (split-string (shell-command-to-string
				(format "which %s"
					post-sync-catchup-sync-command))
			       "\n" t)))
	   (already-visiting (find-buffer-visiting sync-script-file)))
      (save-excursion
	(find-file sync-script-file)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward post-sync-catchup-script-files-pattern
				    (point-max) t)
	    (let ((files-pattern (expand-file-name
				  (substitute-in-file-name
				   (match-string-no-properties 1))
				  post-sync-catchup-tree)))
	      (dolist (file (split-string
			     (shell-command-to-string
			      (format "ls %s" files-pattern))
			     "\n" t))
		(message "post-sync-catchup read %S from script" file)
		(push file files)))))
	(unless already-visiting
	  (kill-buffer already-visiting)))
      (dolist (file files)
	(let ((file-buffer (find-buffer-visiting file)))
	  (when file-buffer
	    (message "post-sync-catchup updating %S from %s at %s" file-buffer file (current-time-string))
	    (with-current-buffer file-buffer
	      (revert-buffer t t))))))))

(provide 'post-sync-catchup)
;;; post-sync-catchup.el ends here
