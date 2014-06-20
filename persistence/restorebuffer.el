;;; Time-stamp: <2013-10-15 12:22:07 johnstu>

(provide 'restorebuffer)
(require 'load-directory)		; implicit requirement
(when (< emacs-major-version 23)
  (add-to-list 'load-path (expand-file-name "startup" user-emacs-directory))
  (require 'library-path))			; for find-library

(if (not (boundp 'no-files))
    (setq no-files nil))

(if (not (boundp 'no-load-matches))
    (setq no-load-matches nil))

(setq failed-library-finds nil)

(defun restorebuffer-show-dirs ()
  "Show what the path etc are while restoring files from previous session."
  (message "common while restoring buffers is %S" (getenv "COMMON"))
  (message "home while restoring buffers is %S" (getenv "HOME"))
  (message "load-path while restoring buffers is %S" load-path))

(defvar restorebuffer-pre-restore-hooks nil
  "Functions to run just before restoring a buffer.
The filename is passed as an argument.")

(defun restorebuffer (finder filename mode
			     min max point mark
			     &optional already-failed while-loading)
  "Restore a buffer. Part of my resumption system."
  (run-hook-with-args 'restorebuffer-pre-restore-hooks filename)
  (if (eq finder 'find-file)
      (setq filename (substitute-in-file-name filename)))
  (if (not no-files)
      (if (and (or (null no-load-matches)
		   (not (string-match no-load-matches filename)))
	       (or (not (eq finder 'find-file))
		   (file-exists-p filename)))
	  (condition-case error-var
	      (progn
		(message "Finding %s with %S and giving it mode %S" filename finder mode)
		(funcall finder filename)
		(if (fboundp mode)
		    (funcall mode))
		(if (> max 0)
		    (narrow-to-region min max))
		(goto-char point)
		(if mark (set-mark mark)))
	    (error (progn (message "Problem in reloading %s: %s" filename error-var))))
	(if (not already-failed)
	    (condition-case error-var
		(save-window-excursion
		  (let ((avoiding-explanation
			 (if no-load-matches
			     (format " (avoiding \"%s\")" no-load-matches)
			   "")))
		    (find-file (expand-file-name "~/.rstorfail.el"))
		    (widen)
		    (goto-char (point-max))
		    (insert (format "(restorebuffer '%s \"%s\" '%s %d %d %d %s t); %s%s%s\n"
				    finder
				    filename
				    mode
				    min max point (prin1-to-string mark)
				    (current-time-string)
				    avoiding-explanation
				    (if while-loading
					(format " while loading \"%s\""
						while-loading)
				      "")))
		    (basic-save-buffer)
		    (message "Could not find \"%s\" with %S%s"
			     filename
			     finder
			     (if (eq finder 'find-library)
				 (format ", load-path=%S" load-path)
			       "")
			     )
		    (if (eq finder 'find-library)
			(push (cons filename load-path)
			      failed-library-finds))
		    ))
	      (error (progn (message "Problem in marking failure of reloading %s: %s" file error-var) (sit-for 2))))))))

;;; end of restorebuffer.el
