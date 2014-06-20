;;; Time-stamp: <2013-09-18 14:15:44 johnstu>
;;; old time stamp: <2006-02-01 13:40:34 jcgs>
;;; old time stamp: <94/12/28 18:33:23 john>

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

(provide 'buffer-file-sync)

(defun search-both-ways (string)
  "Search both ways for STRING.
Move to, and return, the nearest match."
  (let ((preceding (save-excursion (search-backward string (point-min) t)))
	(succeeding (save-excursion (search-forward string (point-max) t)
				    (match-beginning 0))))
    (if (not preceding)
	(if (not succeeding)
	    nil
	  (goto-char succeeding)
	  succeeding)
      (if (not succeeding)
	  (progn (goto-char preceding)
		 preceding)
	;; todo: compare them
	(goto-char (if (< (- succeeding (point))
			  (- (point) preceding))
		       succeeding
		     preceding))))))

(defun up-to-date-file (buffer &optional no-ask)
  "Make sure that BUFFER has the same timestamp its file on disk.
If it doesn't, offer to revert it from the disk version.
Optional argument NO-ASK says not to ask, but revert anyway."
  (interactive "P")
  (set-buffer buffer)
  (let ((file-name (buffer-file-name buffer)))
    (when (and (stringp file-name)
	       (not (string-match "/scpc:.+@.+:/.*" file-name))
	       (file-exists-p file-name)
	       (not (verify-visited-file-modtime buffer))
	       (or no-ask
		   (y-or-n-p (format "Revert %s from %s? "
				     (buffer-name buffer)
				     file-name))))
      (message  "Reverted %s from %s"
		(buffer-name buffer)
		file-name)
      ;; todo: retain position as well as possible
      (let ((column (current-column))
	    (line (line-number-at-pos))
	    (line-text (buffer-substring-no-properties (line-beginning-position)
						       (line-end-position)))
	    (previous-line-text (buffer-substring-no-properties (line-beginning-position 0)
								(line-end-position 0)))
	    (next-line-text (buffer-substring-no-properties (line-beginning-position 2)
							    (line-end-position 2))))
	(revert-buffer t t t)
	(cond
	 ((search-both-ways line-text))
	 ((search-both-ways previous-line-text)
	  (forward-line 1))
	 ((search-both-ways next-line-text)
	  (forward-line -1))
	 (t (goto-line line)))
	(move-to-column column)))))

(defun up-to-date-all-buffers (&optional no-ask)
  "Make sure that all file buffers have the same times as their files on disk.
If they doesn't, offer to revert them from the disk version.
Optional argument NO-ASK says not to ask, but revert anyway."
  (interactive "P")
  (dolist (buffer (buffer-list))
    (up-to-date-file buffer no-ask)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; When you've moved a tree ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-buffer-directory (buffer directory)
  (interactive "bBuffer:
DNew directory for %s: ")
  (set-buffer buffer)
  (setq default-directory
	(if (= ?/ (aref directory (1- (length directory))))
	    directory
	  (concat directory "/")))
  (if buffer-file-name
      (let ((buffer-file-last-name
	     (file-name-nondirectory buffer-file-name)))
	(setq buffer-file-name
	      (expand-file-name buffer-file-last-name)))))

(defun set-all-buffers-directories (olddir newdir)
  (interactive "DOld directory: 
DNew directory: ")
  (setq olddir
	(if (= ?/ (aref olddir (1- (length olddir))))
	    olddir
	  (concat olddir "/"))
	newdir
	(if (= ?/ (aref newdir (1- (length newdir))))
	    newdir
	  (concat newdir "/")))
  (let ((bufs (buffer-list))
	(olddir-length (length olddir)))
    (while bufs
      (set-buffer (car bufs))
      (if buffer-file-name
	  (cond
	   ((string= default-directory olddir)
	    (set-buffer-directory (current-buffer)
				  newdir))
	   ((and (> (length default-directory) olddir-length)
		 (string= (substring default-directory 0 (length olddir))
			  olddir))
	    (set-buffer-directory 
	     (current-buffer)
	     (concat newdir
		     (substring default-directory (length olddir)))))))
      (setq bufs (cdr bufs)))))

(defun revert-quickly (anyway)
  "Revert a buffer, with y-or-n-p instead of yes-or-no-p.
With a prefix arg, don't even ask that."
  (interactive "*p")
  (revert-buffer
   t					; don't try auto-save file
   (or anyway (y-or-n-p "Pick up file version? "))))

(defun prepare-to-move ()
  "Make filenames start with the machine name, according to Harlequin
conventions. Do this for all visited files. This is for use with
buffer-list-saving packages, when you want to start using a different
machine as your main host, and want to make all filenames as absolute
as possible. All files in \"^/usr\" are prepared for moving."
  (interactive)
  (let ((the-buffers (buffer-list))
	(the-base (format "/%s" (system-name))))
    (if (not (file-directory-p the-base))
	(setq the-base (concat "/nfs" the-base)))
    (if (not (file-directory-p the-base))
	(message "No base directory found: %s" the-base));
    (while the-buffers
      (set-buffer (car the-buffers))
      (setq the-buffers (cdr the-buffers))
      (let ((this-file-name (buffer-file-name)))
	(if (and this-file-name
	     (string-match "^/usr" this-file-name))
	    (let ((new-file-name (expand-file-name this-file-name the-base)))
	      (if (file-exists-p new-file-name)
		  (find-alternate-file new-file-name)
		(y-or-n-p (format "Could not find %s! y or n to continue"
				  new-file-name)))))))))

;;; end of buffer-file-sync.el
