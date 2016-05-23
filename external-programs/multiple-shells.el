;;; Time-stamp: <2016-05-23 12:33:18 johstu01>
;;; originated 95/11/09?

;; Start shells on various machines and in various directories.

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

(provide 'multiple-shells)

(defun make-named-shell (name &optional directory command)
  "Make a shell called NAME, optionally cd it to DIRECTORY and give it COMMAND."
  (interactive "sShell name: 
DDirectory to start %s in: ")
  (if (and (or (null directory)
	       (file-directory-p directory))
	   (not (get-buffer name)))
      (progn
	(sleep-for 2)
	(let ((new-shell-buffer (shell)))
	  (rename-buffer name)
	  (if directory
	      (let ((full-dir (expand-file-name
			       (substitute-in-file-name directory))))
		(process-send-string
		 (get-buffer-process new-shell-buffer)
		 (format "cd %s\n" (convert-standard-filename full-dir)))
		(cd full-dir)))
	  (if command
	      (process-send-string
	       (get-buffer-process new-shell-buffer)
	       command)))))
  (switch-to-buffer (get-buffer name)))

(defun make-shell-for-directory-if-present (directory &optional name command)
  "Make a shell for DIRECTORY, called NAME, if DIRECTORY exists.
If COMMAND is given, send it that command too."
  (unless (or (null name)
	      (get-buffer name))
    (setq directory (substitute-in-file-name directory))
    (when (file-directory-p directory)
      (make-named-shell (if name
			    name
			  (format "-%s-"
				  (file-name-nondirectory directory)))
			directory
			command))))

;;; end of external-programs/multiple-shells.el
