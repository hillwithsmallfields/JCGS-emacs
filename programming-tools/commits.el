;;; commits.el --- work through multiple commits
;;;; Time-stamp: <2016-03-25 21:47:09 jcgs>

(defun commits-files-all (files)
  "Check out all commits of FILES."
  (interactive
   (let ((file nil)
	 (files nil))
     (while (not (zerop (length (setq file (read-file-name "File: " nil nil)))))
       (push file files))
     (list files)))
  (dolist (file files)
    (let* ((file-dir (file-name-directory file))
	   (old-name (file-name-nondirectory file))
	   (new-name (expand-file-name (concat "orig-" old-name) file-dir)))
      (message "File %s --> %s" file new-name)
      (copy-file file new-name)))
  (let* ((log-buffer (get-buffer-create "*git log*")))
    (set-buffer log-buffer)
    (erase-buffer)
    (shell-command (concat "git log " (mapconcat 'identity files " "))
		   log-buffer)
    (goto-char (point-min))
    (while (re-search-forward "^commit \\([0-9a-f]+\\)$" (point-max) t)
      (let ((commit (match-string 1)))
	(re-search-forward "^Date:\\s-+\\(.+\\)$")
	(let ((date-string (format-time-string "%Y-%m-%dT%H:%M:%S"
					       (date-to-time
						(match-string 1)))))
	  (message "Commit %s at %s" commit date-string)
	  (shell-command (format (concat "git checkout " commit " " (mapconcat 'identity files " "))))
	  (dolist (file files)
	    (let* ((file-dir (file-name-directory file))
		   (old-name (file-name-nondirectory file))
		   (new-name (expand-file-name (concat date-string "-" old-name) file-dir)))
	      (message "File %s --> %s" file new-name)
	      (copy-file file new-name))))))))
