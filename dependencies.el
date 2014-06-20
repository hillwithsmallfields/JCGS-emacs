;;;; dependencies.el -- work out what requires / provides what
;;; Time-stamp: <2004-11-01 11:48:24 john>

(defvar providers nil
  "Alist of provide statements to filenames")

(defvar requirers nil
  "Alist of require statements to filenames.")
  
(defun analyze-dependencies ()
  "Analyze elisp require / provide"
  (interactive)
  (setq providers nil
	requirers nil)
  (mapcar 'analyze-dependencies-directory load-path))

(defun analyze-dependencies-directory (dir)
  "Look through DIR and its subdirs for requires and provides"
  (let ((files (directory-files dir t nil t)))
    (while files
      (let ((file (car files)))
	(message "%s" file)
	(cond
	 ((string-match "\\.el$" file)
	  (analyze-dependencies-file file))
	 ((and (file-directory-p dir)
	       (not (string-match "/\\.+$" file)))
	  (analyze-dependencies-directory file))
	 (t nil)))
      (setq files (cdr files)))))

(defun analyze-dependencies-file (file)
  "Look through FILE for requires and provides."
  (message "Looking in %s file dependencies" file)
  (let* ((already-in (find-buffer-visiting file)))
    (find-file file)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(require '\\([^)]+\\))" (point-max) t)
	(let* ((feature (intern (match-string-no-properties 1)))
	       (pair (assoc feature requirers)))
	  (if pair
	      (rplacd pair (cons file (cdr pair)))
	    (setq requirers
		  (cons (list feature file)
			requirers)))))
      (goto-char (point-min))
      (while (re-search-forward "(provide '\\([^)]+\\))" (point-max) t)
	(let* ((feature (intern (match-string-no-properties 1)))
	       (pair (assoc feature providers)))
	  (if pair
	      (rplacd pair (cons file (cdr pair)))
	    (setq providers
		  (cons (list feature file)
			providers))))))
    (if (not already-in) (kill-buffer nil))))
