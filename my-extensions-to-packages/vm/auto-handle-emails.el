;;; Time-stamp: <2004-11-01 11:46:30 john>

(provide 'auto-handle-emails)
(require 'vm-message)

(defun process-multiple-emails (fn &optional label-to-add force)
  "Starting with the current message, work through remaining messages executing FN."
  (unless (eq major-mode 'vm-mode) (error "This only works in vm buffers"))
  (let ((more t))
    (catch 'no-more-messages
      (while more
	(let ((labels (vm-labels-of (car vm-message-pointer))))
	  (message "Message labels are %S" labels)
	  (when (or force
		    (null label-to-add)
		    (not (member-string= label-to-add labels)))
	    (goto-char (point-min))
	    (message "Calling %S on message" fn)
	    (when (and (funcall fn)
		       label-to-add)
	      (vm-add-message-labels label-to-add 1))))
	(let ((old-ptr vm-message-pointer))
	  (vm-next-message)
	  (if (eq vm-message-pointer old-ptr)
	      (throw 'no-more-messages t))
	  )))))

(defun foo ()
  (interactive)
  (process-multiple-emails
   (function (lambda ()
	       (if (re-search-forward "From: \\(.+\\)" (point-max) t)
		   (message "Seems to have a from line suggesting %S" (match-string 1))
		 (message "Could not locate from line"))
	       ))
   nil))
