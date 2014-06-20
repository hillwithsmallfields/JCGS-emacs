;;;; photocard-move.el
;;; Time-stamp: <2012-12-25 22:49:43 jcgs>
;;; Move photos between photocard copy directories

(defun card-photos (directory)
  "Return the photos in DIRECTORY.
Looks in directories under DIRECTORY/dcim if present."
  (let ((dcim (expand-file-name "dcim" directory)))
    (when (file-directory-p dcim) (setq directory dcim)))
  (if (file-directory-p directory)
      (let ((photos nil))
	(dolist (file (directory-files directory t nil t))
	  (cond
	   ((file-directory-p file)
	    (setq photos (append photos (directory-files file t "\\.jpg" t))))
	   ((string-match "\\.jpg" file)
	    (push file photos))))
	(sort photos 'string-lessp))
    nil))

(defun card-photos-last-photo-name (directory)
  "Return the name of the last photo in DIRECTORY."
  (let ((existing (card-photos directory)))
    (if existing
	(let ((last-one (car (last existing))))
	  (message "last one is %S" last-one)
	  last-one)
      (error "No photos there"))))

(defun card-photos-new-photo-name (directory &optional last-given for-photo)
  "Create a name for the next photo to go into DIRECTORY.
If optional LAST-GIVEN is non-nil, it is taken as the last existing photo;
this is to avoid re-scanning the directory when creating a series of names.
Optional FOR-PHOTO is the picture for which a name is to be made; it may be
examined to get its timestamp, if the destination directory uses a time-based
filename format."
  (when (and (stringp last-given)
	     (not (string= (file-truename directory)
			   (file-truename (file-name-directory last-given)))))
    (error "Directory of last-given %s is not %s" last-given directory))
  (let* ((last (or last-given
		   (card-photos-last-photo-name directory)))
	 (last-nondir (file-name-nondirectory last)))
    (cond
     ((string-match "p\\([0-9]+\\)\\.jpg" last-nondir)
      (let* ((number-string (match-string 1 last-nondir))
	     (number (1+ (string-to-number number-string)))
	     (chars (length number-string)))
	(message "number-string=%S number=%S chars=%S" number-string number chars)
	(expand-file-name (format (format "p%%0%dd.jpg" chars) number)
			  directory)))
     ((string-match "\\([0-9]\\{8\\}\\)_\\([0-9]\\{3\\}\\)\\.jpg")
      (let ((date-string (match-string 1))
	    (within-day-string (match-string 2)))
	(if (and (stringp for-photo)
		 (file-exists-p for-photo))
	    (let* ((from-photo (shell-command-to-string
				(format "exif -m --tag=0x9003 %s" for-photo)))
		   ;; "2010:11:14 09:09:29"
		   (year (substring from-photo 0 4))
		   (month (substring from-photo 5 7))
		   (day (substring from-photo 8 10))
		   (pictures-that-day-so-far
		    (directory-files directory-files t
				     (format "%s%s%s_\\[0-9\\]\\{3\\}\\.jpg"
					     year month day))))
	      (expand-file-name (format "%s%s%s_%03d.jpg"
					year month day
					(if pictures-that-day-so-far
					    (1+ (string-to-number
						 (substring (car (last pictures-that-day-so-far))
							    9 12)))
					  1))
				    directory)))))
     (t (error "Do not know how to make new filenames in %s" directory)))))

(defvar image-dired-other-card-directory nil
  "The other directory, for moving photos to.")

(defvar image-dired-last-photo-moved nil
  "The name of the last photo moved.")

(defun image-dired-move-to-other-directory (&optional change-directory)
  "Move the selected image to another directory.
Optional prefix argument CHANGE-DIRECTORY re-prompts for where to move to."
  (interactive "P")
  (when (or change-directory
	    (null image-dired-other-card-directory))
    (setq image-dired-other-card-directory
	  (read-directory-name "Move photo to directory: "
			       image-dired-other-card-directory)))
  (let* ((old-name (image-dired-original-file-name))
	 (new-name (card-photos-new-photo-name image-dired-other-card-directory
					       (if change-directory
						   nil
						 image-dired-last-photo-moved)
					       old-name)))
    (when (y-or-n-p (format "Want to move %s to %s? " old-name new-name))
      (rename-file old-name new-name)
      (setq image-dired-last-photo-moved new-name))))

(eval-after-load "image-dired"
  '(define-key image-dired-thumbnail-mode-map "`" 'image-dired-move-to-other-directory))

(defun photocard-move-propagate-deletions (computer-directory camera-directory)
  "Propagate deletions from COMPUTER-DIRECTORY to CAMERA-DIRECTORY."
   (interactive "DPropagate deletions from computer directory:
DPropagate deletions to camera directory: ")
  (dolist (file (directory-files camera-directory t "\\.jpg"))
    (unless (file-exists-p (expand-file-name (file-name-nondirectory file)
					     computer-directory))
      (delete-file file))))

;;;; photocard-move.el ends here

