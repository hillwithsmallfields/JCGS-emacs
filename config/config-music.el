;;; config-music.el --- anything to do with emacs and music  -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019, 2020, 2025  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Set up lilypond, might move emms setup into here too, anything like that

;;; Code:

(autoload 'LilyPond-mode "lilypond-mode"
  "Major mode for editing LilyPond music files.

This mode knows about LilyPond keywords and line comments, not about
indentation or block comments.  It features easy compilation, error
finding and viewing of a LilyPond source buffer or region." t)

(add-to-list 'auto-mode-alist (cons "\\.ly" 'LilyPond-mode))

(defun jcgs/lilypond-compile ()
  "My wrapper around running Lilypond."
  (interactive)
  (LilyPond-command-lilypond)
  (let* ((lilypond-pdf (concat (file-name-sans-extension (LilyPond-get-master-file))
                      ".pdf")))
    (walk-windows (function
                   (lambda (window)
                     (when (eq (buffer-file-name (window-buffer window))
                               lilypond-pdf)
                       (revert-buffer t t))))))
  (LilyPond-command-current-midi))

(defun jcgs/lilypond-mode-load-function ()
  "My Lilypond extras."
  (define-key LilyPond-mode-map "\C-c\C-a" 'jcgs/lilypond-compile))

(add-to-list 'after-load-alist '(lilypond-mode jcgs/lilypond-mode-load-function))

;;   :config (progn
;;             (emms "emms-playlist-mode"
;; 		  "Switch to the current emms-playlist buffer, use
;; emms-playlist-mode and query for a directory tree to add to the
;; playlist."
;; 		  t)
;; 	    (require emms-setup
;; 		     emms-mark
;; 		     emms-mode-line
;; 		     emms-volume
;; 		     emms-lyrics
;; 		     )
;; 	    ;; (emms-standard)
;; 	    (emms-all)
;; 	    (emms-default-players)

;; 	    (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
;; 		  ;; emms-player-ogg123-parameters (if (file-exists-p "/dev/audio1")
;; 		  ;; 						     (list "-o" "dev:plughw:1,0")
;; 		  ;; 						   nil)
;; 		  ;; emms-track-description-function .....
;; 		  ;; emms-playlist-default-major-mode 'emms-mark-mode
;; 		  )

;; 	    (when (and (stringp emms-source-file-default-directory)
;; 		       (file-directory-p emms-source-file-default-directory))
;; 	      (emms-add-directory-tree emms-source-file-default-directory))
;; 	    (cond
;; 	     ((file-directory-p "~/music")
;; 	      (emms-add-directory-tree "~/music"))
;; 	     ((file-directory-p "/work/johstu01/music")
;; 	      (emms-add-directory-tree "/work/johstu01/music")))
;; 	    (cond
;; 	     ((file-directory-p "~/language-audio/")
;; 	      (emms-add-directory-tree "~/language-audio/"))
;; 	     ((file-directory-p "/work/johstu01/language-audio/")
;; 	      (emms-add-directory-tree "/work/johstu01/language-audio/")))
;; 	    (emms-mode-line 1)
;; 	    (emms-lyrics 1)
;; 	    (setq emms-lyrics-display-on-minibuffer t
;; 		  emms-lyrics-scroll-p nil)
;; 	     (global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
;; 	     (global-set-key (kbd "C-c -") 'emms-volume-mode-minus)
;; 	     (global-set-key [ M-f12 ] 'emms)
;; 	     )

(use-package emms
  :straight '(emms :fetcher github :repo "git@github.com:emacsmirror/emms.git")
  :config (progn
            (emms-add-directory-tree "~/Music")
            (require 'emms-player-simple)
            (push emms-player-ogg123 emms-player-list)))

;;;; renaming tracks as I import CDs

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

;;;; merging multi-volume CD collections

(defun multiple-CD-renumber-tracks (directory &optional allow-skip)
  "Renumber tracks in DIRECTORY.
With optional ALLOW-SKIP, allow a gap in the CD numbering.

To start with, the files should all be in one diretory, and the
files from the first cd should all be called *_1.ogg, and so on.
The track numbers are at the start of the file name.  This is the
output produced by cdda2ogg if given a suitable name suffix."
  (interactive "DRenumber tracks in directory:
P")
  (let* ((previous-disc-last-file (string-to-number
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
	    (let* ((original-number (string-to-number (match-string 1 file)))
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
	  (let* ((original-number (string-to-number (match-string 1 file)))
		 (new-name (expand-file-name
			    (replace-match (int-to-string (+ addendum
							     original-number))
					   t nil file 1)
			    directory))
		 (old-name (expand-file-name file directory)))
	    (message "Renaming %s to %s" old-name new-name)
	    (rename-file old-name new-name)))))))

;;;; autoloads

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

;;;; import from other formats

(defun convert-tracks-to-ogg (directory &optional force)
  "Convert the tracks in DIRECTORY to ogg format.
With optional FORCE, do this even if the output file already exists."
  (interactive "DConvert tracks in directory: 
P")
  (dolist (file (directory-files directory t nil t))
    (when (and (not (file-directory-p file))
	       (not (string-match "\\.ogg$" file)))
      (let ((output (concat (file-name-sans-extension
			     (file-name-sans-extension file))
			    ".ogg")))
	(when (or force
		  (not (file-exists-p output)))
	  (message "Converting %s to %s"
		   file output)
	  (shell-command (format "oggenc -o %s %s"
				 output
				 file)))))))

;;;; start drowning out annoying conversations

(defvar drown-out-tracks
  ["In Extremo - Horizont"
   "In Extremo - Kuess Mich"
   "Procol Harum - Long gone geek"
   "Various - Traditional dance and Thomas Hardy music"
   "Assorted - Gathering Peascods"
   "Assorted - In feuers hitz"
   "Mostly Slovak orchestras - Vivaldi Spring from Four Seasons"
   "Mostly Slovak orchestras - Tchaikovsky Danse Espagnole from Swan Lake"
   "Mostly Slovak orchestras - Glinka Overture from Ruslan and Ludmilla"
   "Mostly Slovak orchestras - Mozart Presto from Salzburg Symphony"
   "Mostly Slovak orchestras - Tchaikovsky Polonaise from Yevgeni Onegin"
   "Mostly Slovak orchestras - Schubert Moment Musical in F minor"]
  "Tracks that drown out annoying background sounds.
These should all start full volume very quickly, without a quiet lead-in.")

(defvar drown-out-forcibly-tracks
  ["In Extremo - Horizont"
   "In Extremo - Kuess Mich"]
  "Tracks that drown out annoying background sounds.
These should all start full volume very quickly, without a quiet lead-in.")
  
(defun drown-out-annoying-conversations (&optional force)
  "Play a track that drowns out annoying background sound.
Optional argument FORCE means do it more forcibly."
  (interactive "P")
  (save-window-excursion
    (emms)
    (when emms-player-playing-p
      (emms-stop))
    (setq pre-drowning-track (and emms-playlist-selected-marker
				  (marker-position emms-playlist-selected-marker)))
    (goto-char (point-min))
    (let ((tracks (if force
		      drown-out-forcibly-tracks
		    drown-out-tracks)))
    (if (search-forward (aref tracks
			      (random (length tracks)))
			(point-max) t)
	(progn
	  (emms-playlist-select (line-beginning-position))
	  (emms-start))
      (error "Could not find blanking track")))))

(defun brandenburgs ()
  "Start playing the Brandenburg concertos."
  (interactive)
  (save-window-excursion
    (emms)
    (when emms-player-playing-p
      (emms-stop))
    (goto-char (point-min))
    (if (search-forward "Brandenburg"
			(point-max) t)
	(progn
	  (emms-playlist-select (line-beginning-position))
	  (emms-start))
      (error "Could not find Brandenburgs"))))

;; end of use-emms.el

(provide 'config-music)
;;; config-music.el ends here
