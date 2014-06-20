;;; Time-stamp: <2001-02-07 15:31:30 jcgs>

(provide 'save-kill)

(defun save-kill-ring ()
  "Save the kill ring."
  (interactive)
  (kill-ring-defontify)
  (require 'save-emacs-state)
  (save-window-excursion
    (let ((find-file-hooks nil)
	  (i 0)
	  (find-file-not-found-hooks nil)
	  (emacs-lisp-mode-hook nil))
      (find-file (expand-file-name "killring.el"
				   emacs-save-restorers))
      (erase-buffer)
      (message "Saving kill ring...")
      (insert ";;;; Kill ring saved at " (current-time-string)
	      " by " (user-login-name) "\n")
      (insert "(setq kill-ring '(")
      (mapcar '(lambda (kill-item)
		 (message "Saving kill ring... %d" i)
		 (setq i (1+ i))
		 (insert 
		  (prin1-to-string kill-item) "\n  "))
	      kill-ring)
      (insert "))\n")
      (insert (format "(setq kill-ring-max %d)\n" kill-ring-max))
      ;; (insert "(if (fboundp 'yank-menu-prepare-buffer) (yank-menu-prepare-buffer kill-ring))n")
      (basic-save-buffer)
      (kill-buffer (current-buffer))
      (message "Saving kill ring... done"))))

;;; end of save-kill.el
