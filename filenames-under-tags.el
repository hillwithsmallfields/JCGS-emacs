;;;; filenames-under-tags.el
;;; Time-stamp: <03/03/23 14:03:42 jcgs>

(provide 'filenames-under-tags)

(defvar all-tag-files nil
  "All the file names under the tag files")

(defun all-tag-files (&optional force)
  "Return all the file names under the tag files"
  (if (and all-tag-files (not force))
      all-tag-files
    (setq all-tag-files 
	  (let ((files-in-files nil)
		(cont nil))
	    (while (visit-tags-table-buffer cont)
	      (setq files-in-files (cons (mapcar 'expand-file-name (tags-table-files)) files-in-files)
		    cont t))
	    (apply 'concatenate 'list files-in-files)))))

(defvar tag-file-short-names-alist nil
  "Alist of last part to full filenames equivalent to all-tag-files")

(defun tag-file-short-names-alist (&optional force)
  "Returns Alist of last part to full filenames equivalent to all-tag-files"
  (if (and tag-file-short-names-alist (not force))
      tag-file-short-names-alist
    (setq tag-file-short-names-alist
	  (let ((files (all-tag-files force))
		file
		(result nil))
	    (while files
	      (setq file (car files)
		    result (cons (cons (file-name-nondirectory file) file)
				 result)
		    files (cdr files)))
	    result))))

(defun locate-tagged-file (name)
  "Return the full name of a file called NAME amongst the files listed in the current tag files.
If none, return nil; thus, suitable for putting on find-file-at-point-hooks."
  (let ((found-name (assoc name (tag-file-short-names-alist))))
    (if found-name
	(let ((found-file-name (cdr found-name)))
	  (if (file-exists-p found-file-name)
	      found-file-name
	    nil))
      nil)))

(add-hook 'find-file-at-point-hooks 'locate-tagged-file)

;;; end of filenames-under-tags.el
