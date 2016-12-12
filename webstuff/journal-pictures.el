;;;; journal-pictures.el -- check that I've used all the pictures provided
;;; Time-stamp: <2007-01-31 16:04:42 jcgs>

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

(provide 'journal-pictures)

(defun html-file-image-list (file)
  "Return a list of the images used in FILE."
  (save-window-excursion
    (find-file file)
    (save-excursion
      (let ((images nil))
	(goto-char (point-min))
	(while (re-search-forward "src=\"\\([^\"]+\\)\"" (point-max) t)
	  (setq images (cons (match-string-no-properties 1) images)))
	(bury-buffer)
	images))))

(defun html-directory-image-list (directory)
  "Return a list of the images used in DIRECTORY."
  (let ((images-lists (mapcar 'html-file-image-list
			      (directory-files directory t "\\.html$" t)))
	(images nil))
    (while images-lists
      (let ((file-images (car images-lists)))
	(while file-images
	  (unless (member (car file-images) images)
	    (setq images (cons (car file-images) images)))
	  (setq file-images (cdr file-images))))
      (setq images-lists (cdr images-lists)))
    images))

(defun directory-photos (directory)
  "Return a list of the probably photographs in DIRECTORY."
  (directory-files directory nil
		   "\\.[jJ][pP][eE]?[gG]$"))

;;;###autoload
(defun directory-unused-photos (directory)
  "List the unused photos in DIRECTORY."
  (interactive "DList unused photos in directory: ")
  (let ((available (directory-photos directory))
	(used (html-directory-image-list directory))
	(unused nil))
    (while available
      (unless (member (car available) used)
	(setq unused (cons (car available) unused)))
      (setq available (cdr available)))
    (when (interactive-p)
      (with-output-to-temp-buffer (format "*Unused pictures in %s*" directory)
	(let ((out (sort unused 'string<)))
	  (while out
	    (princ (car out))
	    (princ "\n")
	    (setq out (cdr out))))))
    unused))

;;;###autoload
(defun jpeg-time-taken (file)
  "Work out the date and time that the picture in FILE was taken.
Return them in emacs time format."
  (interactive "fFind date of file: ")
  (save-window-excursion
    (let ((visiting (find-buffer-visiting file)))
      (find-file file)
      (let ((result (or (cdr (assoc 'ifd0-tiff-tag-date-time-last-modified-encoded
				    jpeg-details))
			(nth 5 (file-attributes file)))))
	(unless visiting
	  (kill-buffer nil))
	(message "Image %s appears to be dated %s" file (current-time-string result))
	result))))

(defvar journal-unsuitable-photo-names "\\(DSC\\)\\|\\(Image\\)"
  "Regexp matching photo names that need renaming, e.g. ones in the form they come from the camera.")

;;;###autoload
(defun directory-place-unused-photos (directory)
  "Place the unused photos in DIRECTORY."
  (interactive "DPlace unused photos in directory: ")
  (let ((photos (directory-unused-photos directory)))
    (while photos
      (let* ((photo (expand-file-name (car photos) directory))
	     (date (jpeg-time-taken photo))
	     (decoded-date (decode-time date)))
	(if (null date)
	    (message "Could not find date of %s" photo)
	  (when (string-match journal-unsuitable-photo-names photo)
	    (let* ((default-name (format "%04d-%02d-%02d--%02d-%02d-%02d.jpg"
					 (nth 5 decoded-date)
					 (nth 4 decoded-date)
					 (nth 3 decoded-date)
					 (nth 2 decoded-date)
					 (nth 1 decoded-date)
					 (nth 0 decoded-date)))
		   (new-name (read-from-minibuffer (format "Rename %s to (default %s): " photo default-name))))
	      (when (string= new-name "")
		(setq new-name default-name))
	      (setq new-name (expand-file-name (file-name-nondirectory new-name)
					       (file-name-directory photo)))
	      (while (file-exists-p new-name)
		(setq new-name (concat (file-name-sans-extension new-name) "-a" (file-name-extension new-name))))
	      (rename-file photo new-name)
	      (setq photo new-name)))
	  (when t
	    (journal-emacs-time (journal-current-journal directory) decoded-date)
	    (if t
		(let ((short-name (file-name-nondirectory photo)))
		  (insert "<!-- automatically placed image --><img src=\""
			  short-name
			  "\" align=\"left\" alt=\"["
			  (if (string-match "--\\([0-9]+\\)-\\([0-9]+\\)-" short-name)
			      (format "At %02d:%02d"
				      (string-to-int (match-string 1 short-name))
				      (string-to-int (match-string 2 short-name)))
			    (subst-char-in-string ?_ 32
						  (subst-char-in-string ?- 32 short-name)
						  t))
			  "]\">"))
	      (message "Would put %s here" photo)
	      (sit-for 2)))))
      (setq photos (cdr photos)))))

;;;###autoload
(defun journal-rename-picture ()
  "Rename the picture near point."
  (interactive)
  (if (and (search-backward "<img" (point-min) t)
	   (re-search-forward "src=\"\\([^\"]+\\)\"" (point-max) t))
      (let* ((old-name (match-string-no-properties 1))
	     (new-name (save-match-data
			 (read-from-minibuffer (format "New name for %s: " old-name)))))
	(replace-match new-name t t nil 1)
	(rename-file (expand-file-name old-name)
		     (expand-file-name new-name)))))

;;; end of journal-pictures.el
