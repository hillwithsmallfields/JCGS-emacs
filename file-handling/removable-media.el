;;;; removable-media.el -- find files that may have been put on another drive
;;; Time-stamp: <2015-04-06 09:36:32 jcgs>

;; Handle files on removable drives (such as USB keys)

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

(provide 'removable-media)

;;;###autoload
(defun find-file-not-found-try-any-drive ()
  "Try to find the file that should have been found, by trying other drives (for WinEmacs / NTemacs)."
  (if (string-match "\\([a-z]\\):\\(.+\\)$" buffer-file-name)
      (let ((old-drive (string-to-char (match-string 1 buffer-file-name)))
	    (name (match-string 2 buffer-file-name))
	    (drive ?c))
	(catch 'done
	  (while (<= drive ?z)
	    (let ((fullname (format "%c:%s" drive name)))
	      (if (and (not (eq drive old-drive))
		       (file-readable-p fullname))
		  (progn
		    (find-file fullname)
		    (setq buffer-file-name fullname)
		    (throw 'done t))))
	    (setq drive (1+ drive)))
	  nil))))

(add-hook 'find-file-not-found-hooks 'find-file-not-found-try-any-drive)

(defvar removable-files-directories nil
  "Alist of directories kept on USB keys etc.
The car of each pair is the name of a top-level directory of a drive,
and the cdr is the pathname at which that directory has been mounted.
Must be searched for as we don't know in advance what the drive letter
will be (on WinEmacs, anyway).
Should normally be accessed via find-removable-files-directory.")

(defvar bad-drives '(?f)
  "Drive letters not to try.")

;;;###autoload
(defun find-removable-files-directory (directory)
  "Find and return the path to a removable drive with DIRECTORY at its top level.
DIRECTORY can be several elements of a path, rather than a single element."
  (let ((pair (assoc directory removable-files-directories)))
    (if pair
	(cdr pair)
      (message "Looking for removable media with top-level directory %s" directory)
      (let ((found
	     (catch 'found
	       (cond
		((memq system-type '(gnu/linux))
		 (let ((mount-points (append (directory-files "/mnt" t)
					     (directory-files "/media" t))))
		   (while mount-points
		     (let ((mount-point (car mount-points)))
		       (message "%s? %S" mount-point (string-match "\\.$" mount-point))
		       (unless (string-match "\\.$" mount-point)
			 (let ((mounted-dir (expand-file-name directory mount-point)))
			   (message "(%s?)" mounted-dir)
			   (when (file-directory-p mounted-dir)
			     (throw 'found mounted-dir)))))
		     (setq mount-points (cdr mount-points)))))
		((memq system-type '(windows-nt))
		 (let* ((drive ?d)
			(dir-format (format "%%c:/%s/" directory)))
		   (while (<= drive ?z)
		     (unless (member drive bad-drives)
		       (let ((dir (format dir-format drive)))
			 ;; (message "%s?" dir)
			 (if (file-directory-p dir)
			     (throw 'found dir))))
		     (setq drive (1+ drive))))))
	       nil)))
	(when found
	  (setq removable-files-directories
		(cons (cons directory
			    found)
		      removable-files-directories)))
	found))))

(defun mount-removable-top-level-directory (removable-top-level-directory)
  "Find a usb key with REMOVABLE-TOP-LEVEL-DIRECTORY at its top level, and run elisp it offers."
  (interactive "sFind and mount directory: ")
  (let ((drive-path (find-removable-files-directory removable-top-level-directory)))
    (if drive-path
	(let ((drive-elisp (expand-file-name "mount-drive.el"
					     drive-path)))
	  (if (file-readable-p drive-elisp)
	      (progn
		(message "Loading demountable drive elisp %s" drive-elisp)
	      (load-file drive-elisp))
	    (message "No elisp file for demountable drive: %s" drive-elisp)))
      (message "Could not find device holding %s" removable-top-level-directory))))

;;; end of removable-media.el
