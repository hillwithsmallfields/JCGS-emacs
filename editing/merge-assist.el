;;;; merge-assist.el --- handle files with merge markers
;;; Time-stamp: <2016-02-25 16:46:11 johstu01>

(defun merge-assist-first ()
  "Take the first choice in each merge."
  (interactive)
  (merge-assist-all t))

(defun merge-assist-second ()
  "Take the seccond choice in each merge."
  (interactive)
  (merge-assist-all nil))

(defun merge-assist-all (which)
  "Take WHICH choice in each merge."
  (goto-char (point-min))
  (while (re-search-forward "<<<<<<<.*" (point-max) t)
    (let ((start-all (1- (match-beginning 0)))
	  (start-first (1+ (point))))
      (if (re-search-forward "=======.*")
	  (let* ((end-first (1- (match-beginning 0)))
		 (first-text (buffer-substring start-first end-first))
		 (first-line-number (line-number-at-pos))
		 (start-second (1+ (point))))
	    (if (re-search-forward ">>>>>>>.*" (point-max) t)
		(let* ((end-second (1- (match-beginning 0)))
		       (second-text (buffer-substring start-second end-second))
		       (end-all (1+ (point))))
		  (delete-region start-all end-all)
		  (insert (if which
			      first-text
			    second-text)))
	      (error "Could not find end marker corresponding to starter at line %d" first-line-number)))
	(error "Could not find middle marker corresponding to starter at line %d" first-line-number)))))

;;; merge-assist.el ends here
