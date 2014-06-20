;; For systems without "find"

(defun pseudo-delete-file (file)
  (message "would delete %S" file))

(defun remove-backup-files (dir)
  (interactive "DDelete backups under directory: ")
  (mapcar 'delete-file
	  (directory-files dir t ".+~$" t))
  (mapcar (lambda (file)
	    (if (and (file-directory-p file)
		     (not (string-match "\\.$" file)))
		(remove-backup-files file)))
	  (directory-files dir t)))

