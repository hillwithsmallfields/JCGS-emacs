;;; photo-sorting.el --- utilities for sorting my photo collection

;; Copyright (C) 2009  John C G Sturdy

;; Author: John C G Sturdy <john.sturdy@ul.ie>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(defun weed-photo-get-best-name (photos)
  "Get the best of PHOTOS.
The best is the file with the most specific (longest) name."
  (let ((best-so-far (car photos)))
    (dolist (photo (cdr photos))
      (when (> (length (car photo))
	       (length (car best-so-far)))
	(setq best-so-far photo)))
    best-so-far))

(defun weed-photo-get-best-picture (photos)
  "Get the best of PHOTOS.
The best is the largest file."
  (let ((best-so-far (car photos)))
    (dolist (photo (cdr photos))
      (when (> (cdr photo)
	       (cdr best-so-far))
	(setq best-so-far photo)))
    best-so-far))

(defun weed-photo-matching (pattern block)
  "Return a list of the photos matching PATTERN in BLOCK."
  (let ((result nil))
    (dolist (photo block)
      (when (string-match pattern (car photo))
	(push photo result)))
    result))

(defvar debug-photo-sorting nil)

(defun weed-photo-block (photo-block)
  "Weed the photos in PHOTO-BLOCK."
  (let* ((finals (weed-photo-matching "\\`photos/" photo-block))
	 (best-final (weed-photo-get-best-name finals))
	 (pre-tidies (weed-photo-matching "\\`photos-before-auto-tidy" photo-block))
	 (raws (weed-photo-matching "\\`raw-photos/" photo-block))
	 (best-raw (weed-photo-get-best-picture raws))
	 (raw-pre-tidies (weed-photo-matching "\\`raw-photos-before-auto-tidy/" photo-block)))
    (when debug-photo-sorting
      (message "finals %S" finals)
      (message "pre-tidies %S" pre-tidies)
      (message "raws %S" raws)
      (message "raw-pre-tidies %S" raw-pre-tidies)
      (message "-"))

    (when raws
      (when (cdr raws)
	(when debug-photo-sorting
	  (message "more than one raw photo: %S; best is %S" raws best-raw)))
      (dolist (final finals)
	(when debug-photo-sorting
	  (message "both final and raw for %S %S" final raws))
	(when (> (cdr best-raw)
		 (cdr final))
	  (message "final %S should be replaced by better raw %S" final best-raw)
	  (copy-file (car best-raw)
		     (car final)
		     t
		     t
		     t))
	)
      )

    ))

(defun weed-photo-collection ()
  "Weed out duplicate and reduced copies of photos."
  (interactive)
  (find-file "~/photo-details-sorted")
  (goto-char (point-min))
  (let ((current-date (buffer-substring (point) (+ (point) 19)))
	this-line-date
	this-block-photos)
    (while (not (eobp))
      (setq this-line-date (buffer-substring (point) (+ (point) 19)))
      (if (string= this-line-date current-date)
	  (if (looking-at "^[0-9]\\{4\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} \\([0-9]+\\) \\(.+\\)")
	      (push (cons (match-string 2)
			  (string-to-number (match-string 1)))
		    this-block-photos)
	    (error "Malformed line"))
	(weed-photo-block this-block-photos)
	(setq current-date this-line-date
	      this-block-photos nil))
      (beginning-of-line 2))))

(defun photos-link-subdirectories (dir)
  "Make links in subdirectories of DIR to photos in DIR, interactively."
  (interactive "DDirectory to classify: ")
  (let* ((files (directory-files dir t))
	 (subdirs nil))
    (dolist (file files)
      (when (and (file-directory-p file)
		 (not (string-match "\\.\\'" file)))
	(push (cons (aref (file-name-nondirectory file) 0)
		    file)
	      subdirs))
      )
    (message "subdirs are: %S" subdirs)
    (dolist (file files)
      (when (string-match "\\.jpg" file)
	(find-file file)
	(catch 'done
	  (while t
	    (let ((subdir (cdr (assoc (read-char) subdirs))))
	      (when subdir
		(make-symbolic-link file
				    (expand-file-name (file-name-nondirectory file)
						      subdir)
				    t)
		(throw 'done t)))))
	(kill-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some bits for camera card file culling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar camera-card "/mnt/camera/DCIM"
  "The camera card directory.")

(defun camera-card-photos ()
  "Return the contents of the camera card."
  (apply 'append
	 (mapcar (function
		  (lambda (dir)
		    (directory-files dir t "\\.JPG" t)))
		 (directory-files camera-card t))))

(defun camera-card-photos-on-date (year month day)
  "Return the photos on the camera card that were taken on YEAR MONTH DAY."
  (delete-if-not (function
		  (lambda (photo)
		    (let* ((file-timestamp (decode-time
					    (nth 5
						 (file-attributes photo)))))
		      (and (= (nth 5 file-timestamp) year)
			   (= (nth 4 file-timestamp) month)
			   (= (nth 3 file-timestamp) day)))))
		 (camera-card-photos)))

(defun camera-card-cull-date-against-directory (year month day directory)
  "Delete photos on the card taken on YEAR MONTH DAY that are not also in DIRECTORY.
This is for when you've culled photos from a directory on the computer, and now want
to remove the corresponding photos on the camera card."
  (interactive "nYear: 
nMonth: 
nDay: 
DDirectory: ")
  (let ((on-card (camera-card-photos-on-date year month day))
	(on-computer (mapcar 'upcase
			     (directory-files directory nil nil t)))
	(keep nil)
	(cull nil))
    (dolist (photo on-card)
      (if (member (file-name-nondirectory photo)
		  on-computer)
	  (push photo keep)
	(push photo cull)))
    (with-output-to-temp-buffer "*Photos to keep and to cull*"
      (princ (format "Keep: %S\nCull: %S\n" keep cull)))
    (when (yes-or-no-p "Delete files? ")
      (mapcar 'delete-file cull))))

(defun photos-in-tree (dir)
  "List the photos in DIR and its subdirectories.
Result is an alist of short name to full name."
  (let ((result nil))
    (dolist (file (directory-files dir t nil t))
      ;; (message "got %S" file)
      (cond
       ((string-match "\\.$" file) nil)
       ((file-directory-p file)
	(setq result (append result (photos-in-tree file))))
       ((string-match "\\.jpg$" file)
	(setq result (cons (cons (file-name-nondirectory file)
				 file)
			   result)))))
    result))

(defun match-raw-and-sorted-photos ()
  "Match my two photo areas."
  (interactive)
  (let ((sorted (photos-in-tree "/gallery/jcgs/photos"))
	(raw (photos-in-tree "/gallery/jcgs/raw-photos"))
	(n-same 0))
    ;; (dolist (file sorted) (message "sorted: %S" file))
    ;; (dolist (file raw) (message "raw: %S" file))
    (dolist (file sorted)
      (when (and (setq m (assoc (car file) raw))
		 (= (nth 7 (file-attributes (cdr file)))
		    (nth 7 (file-attributes (cdr m)))))
	(message "%s is also %s" file m)
	(setq n-same (1+ n-same))
	)
      )
    (message "%d the same" n-same)))

(provide 'photo-sorting)
;;; photo-sorting.el ends here
