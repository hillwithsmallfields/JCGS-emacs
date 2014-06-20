;;;; Code to add a bbdb entry to .mailrc
;;; Time-stamp: <2001-06-15 09:44:55 jcgs>

(provide 'bbdb-add-mailrc)

(defun bbdb-add-entry-to-mailrc ()
  "Add the current entry to your .mailrc"
  (interactive)
  (save-window-excursion
    (set-buffer "*BBDB*")
    (let ((name (bbdb-record-name (bbdb-current-record t)))
	  (nets (bbdb-record-net (bbdb-current-record)))
	  )
      (setq name (substitute ?. ?  name))
      (message "name is %s, nets is %S" name nets)
      (let ((pattern (format "alias +%s +" name)))
	(find-file "~/.mailrc")
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward pattern (point-max) t)
	      (message "Already in .mailrc")
	    (progn
	      (goto-char (point-max))
	      (insert "\nalias " name " " (car nets) "\n"))))))))

