;;; Time-stamp: <2005-02-01 14:59:37 john>

(provide 'savebuffer)
(require 'filenames-in-env)

(defvar savebuffer-save-me t
  "Whether to save the current buffer. By default, t.
Individual buffers may set it to nil to indicate they don't want
to be saved by savebuffer")

(make-variable-buffer-local 'savebuffer-save-me)

(defun savebuffer-mark-buffer-for-no-save (buffer)
  "Mark BUFFER as being not to be saved."
  (save-window-excursion
    (set-buffer buffer)
    (message "Marking buffer %s not to be saved" buffer)
    (setq savebuffer-save-me nil)))

(defun save-one-buffer-configuration (buffer &optional prefix)
  "Insert a save description for BUFFER in the current buffer."
  (if (and (save-window-excursion (set-buffer buffer) savebuffer-save-me)
	   (buffer-file-name buffer))
      (let* ((saver-buffer (prog1 (current-buffer) (set-buffer buffer)))
	     (directory (expand-file-name default-directory))
	     (is-library (and
			  (string-match "\\.elc?" buffer-file-name)
			  (or (member directory load-path)
			      (member (substring directory 0 -1) load-path))))
	     (finder (if is-library 'find-library 'find-file))
	     (b-filename 
	      (if is-library
		  (let ((nondir (file-name-nondirectory buffer-file-name)))
		    (substring nondir 0 (string-match "\\.el" nondir)))
		(unsubstitute-in-file-name buffer-file-name)))
	     (b-point (point))
	     (b-mark 
	      (condition-case errorvar
		  (mark)
		(mark-inactive nil)))
	     (b-min (point-min))
	     (b-max (point-max))
	     (b-mode major-mode))
	(if (not (eq buffer saver-buffer))
	    (progn
	      (message "Recording state of buffer %s%s"
		       buffer
		       (if is-library
			   ", from library"
			 ""))
	      ;; (setq b-filename (file-truename b-filename))
	      (if (and (= b-min 1) (> b-max (buffer-size)))
		  (setq b-min 0 b-max 0))
	      (set-buffer saver-buffer)
	      (insert (format
		       "%s(restorebuffer '%s %s '%s %d %d %d %s nil %s)\n"
		       (if prefix prefix "")
		       (if is-library 'find-library 'find-file)
		       (prin1-to-string b-filename)
		       b-mode
		       b-min
		       b-max
		       b-point
		       (prin1-to-string b-mark)
		       "(buffer-file-name)")))))))

;;; end of savebuffer.el
