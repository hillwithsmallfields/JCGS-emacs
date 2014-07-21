;;;; find, load and configure emms
;;; Time-stamp: <2014-07-08 09:52:00 johstu01>

(use-package emms
	     "~/library/emacs/emms/emms-3.0/"
	     nil
	     ((emms "emms-playlist-mode"
		    "Switch to the current emms-playlist buffer, use
emms-playlist-mode and query for a directory tree to add to the
playlist."
		    t)
	      (require emms-setup
		       emms-mark
		       emms-mode-line
		       emms-volume
		       emms-lyrics
		       ))
	     ;; (emms-standard)
	     (emms-all)
	     (emms-default-players)

	     (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
		   ;; emms-player-ogg123-parameters (if (file-exists-p "/dev/audio1")
		   ;; 						     (list "-o" "dev:plughw:1,0")
		   ;; 						   nil)
		   ;; emms-track-description-function .....
		   ;; emms-playlist-default-major-mode 'emms-mark-mode
		   )

	     (when (and (stringp emms-source-file-default-directory)
			(file-directory-p emms-source-file-default-directory))
	       (emms-add-directory-tree emms-source-file-default-directory))
	     (cond
	      ((file-directory-p "~/music")
	       (emms-add-directory-tree "~/music"))
	      ((file-directory-p "/work/johstu01/music")
	       (emms-add-directory-tree "/work/johstu01/music")))
	     (cond
	      ((file-directory-p "~/language-audio/")
	       (emms-add-directory-tree "~/language-audio/"))
	      ((file-directory-p "/work/johstu01/language-audio/")
	       (emms-add-directory-tree "/work/johstu01/language-audio/")))
	     (emms-mode-line 1)
	     (emms-lyrics 1)
	     (setq emms-lyrics-display-on-minibuffer t
		   emms-lyrics-scroll-p nil)
	     (global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
	     (global-set-key (kbd "C-c -") 'emms-volume-mode-minus)
	     (global-set-key [ M-f12 ] 'emms)
	     )

(defun rename-track (old new)
  "Rename track from OLD to NEW.
Some adjustment of the name may be done."
  (interactive "fRename track:
sRename to: ")
  (while (string-match " " new)
    (setq new (replace-match "_" t t new)))
  (let* ((old-name (file-name-nondirectory old))
	 (number (if (string-match "^[0-9]+-" old-name)
		    (match-string-no-properties 0 old-name)
		  "")))
    (rename-file old (expand-file-name
		      (concat
		       number
		       (file-name-sans-extension new)
		       "."
		       (file-name-extension old))
		      (file-name-directory old)))))

(defun rename-tracks (directory)
  "Interactively rename tracks in DIRECTORY.
This is meant to go from the numbered but otherwise anonymous
tracks produced by cdda2ogg, to named and numbered tracks."
  (interactive "DRename tracks in directory: ")
  (dolist (old  (directory-files directory nil "^[0-9]" nil))
    (rename-track (expand-file-name old directory)
		  (read-from-minibuffer
		   (format "Rename %s to: " old)))))

(defun multiple-CD-renumber-tracks (directory &optional allow-skip)
  "Renumber tracks in DIRECTORY.
With optional ALLOW-SKIP, allow a gap in the CD numbering.

To start with, the files should all be in one diretory, and the
files from the first cd should all be called *_1.ogg, and so on.
The track numbers are at the start of the file name.  This is the
output produced by cdda2ogg if given a suitable name suffix."
  (interactive "DRenumber tracks in directory:
P")
  (let* ((previous-disc-last-file (string-to-int
			    (car
			     (nreverse
			      (directory-files directory
					       nil
					       "_1.ogg$"
					       nil)))))
	 (disc-number 2)
	 (file-list nil)
	 this-disc-last-number)
    (while (or allow-skip
	       (setq file-list (nreverse
				(directory-files directory
						 nil
						 (format "_%d.ogg$" disc-number)
						 nil))))
      (if (null file-list)
	  (setq allow-skip nil)
	(dolist (file file-list)
	  (when (string-match "^\\([0-9][0-9]\\)" file)
	    (let* ((original-number (string-to-int (match-string 1 file)))
		   (old-name (expand-file-name file directory))
		   (temp-name (expand-file-name
			      (replace-match (int-to-string (+ previous-disc-last-file
							       original-number))
					     t nil file 1)
			      directory)))
	      (setq this-disc-last-number original-number)
	      (message "Renaming %s to %s" old-name temp-name)
	      (rename-file old-name temp-name)))))
      (setq disc-number (1+ disc-number)
	    previous-disc-last-file (+ previous-disc-last-file
				       this-disc-last-number)))
    ;; Now remove the markers saying which CD each file came from
    (dolist (file (directory-files directory t ".ogg$"))
      (when (string-match "\\(.+\\)_[0-9].ogg$" file)
	(let ((new-name (concat (match-string 1 file) ".ogg")))
	  (message "Renaming %s to %s\n" file new-name)
	  (rename-file file new-name))))))

(defun multiple-CD-raise-from-subdirs (subdir topdir name-suffix number)
  "Raise files from SUBDIR to TOPDIR using SUFFIX and NUMBER.
Helper function for multiple-CD-combine-subdirs."
  (let ((files (directory-files subdir t)))
    (dolist (file files)
      (cond
       ((string-match "\\.\\'" file)
	nil)
       ((file-directory-p file)
	(setq number (multiple-CD-raise-from-subdirs file topdir
						     name-suffix number)))
       ((string-match "\\.ogg" file)
	(rename-file file
		     (expand-file-name (format "%03d-%s" number name-suffix)
				       topdir))
	(setq number (1+ number)))
       (t nil))))
  number)

(defun multiple-CD-combine-subdirs (directory name-suffix)
  "Combine subdirectories into DIRECTORY using NAME-SUFFIX."
  (interactive "DDirectory to combine subdirs into: 
sName suffix: ")
  (multiple-CD-raise-from-subdirs directory directory name-suffix 1))

(defun renumber-files-in-directory (directory addendum)
  "To filenames in DIRECTORY add ADDENDUM to the first number in each name."
  (interactive "DRenumber files in directory:
nNumber to add to each number: ")
  (let ((file-list (directory-files directory t nil nil)))
    (dolist (filename file-list)
      (let ((file (file-name-nondirectory filename)))
	(when (string-match "^\\([0-9][0-9]\\)" file)
	  (let* ((original-number (string-to-int (match-string 1 file)))
		 (new-name (expand-file-name
			    (replace-match (int-to-string (+ addendum
							     original-number))
					   t nil file 1)
			    directory))
		 (old-name (expand-file-name file directory)))
	    (message "Renaming %s to %s" old-name new-name)
	    (rename-file old-name new-name)))))))

(add-lispdir (expand-file-name "music/" user-emacs-directory))
(autoload 'lyric-mode "lyric-mode"
  "Major mode for editing lyric files.
Commands are provided to start and stop a music player, and to insert
timestamp tags.

When not playing, the space and return keys insert space and
newline; when playing, they insert a tag and move to the next
line, respectively, letting you move rapidly through a
ready-typed text to add a tag to each line." t)
(add-to-list 'auto-mode-alist (cons "\\.lrc" 'lyric-mode))

;; end of use-emms.el
