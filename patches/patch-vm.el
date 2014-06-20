;;;; vm-patches.el
;;; Time-stamp: <2006-01-18 10:23:12 jcgs>

;;; redefinitions of vm functions

(when (and (boundp 'vm-host)
	   vm-host)

(require 'vm-mime)

;;;; redefine vm-mime-attach-file to remember which directory we last attached something from

(defvar vm-mime-attach-last-directory nil
  "The last directory from which we attached a file interactively.")

(defun vm-mime-attach-file (file type &optional charset description
			    no-suggested-filename)
  "Attach a file to a VM composition buffer to be sent along with the message.
The file is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, FILE, is the name of the file to attach.  Second
argument, TYPE, is the MIME Content-Type of the file.  Optional
third argument CHARSET is the character set of the attached
document.  This argument is only used for text types, and it is
ignored for other types.  Optional fourth argument DESCRIPTION
should be a one line description of the file.  Nil means include
no description.  Optional fifth argument NO-SUGGESTED-FILENAME non-nil
means that VM should not add a filename to the Content-Disposition
header created for the object.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use vm-mime-attach-mime-file to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (charset nil)
	 description file default-type type)
     (if (null vm-send-using-mime)
	 (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
     (setq file (vm-read-file-name "Attach file: " vm-mime-attach-last-directory nil t)
	   vm-mime-attach-last-directory (file-name-directory file)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (if (vm-mime-types-match "text" type)
	 (setq charset (completing-read "Character set (default US-ASCII): "
					vm-mime-charset-completion-alist)
	       charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (if (string-match "^[ \t]*$" description)
	 (setq description nil))
     (list file type charset description nil)))
  (if (null vm-send-using-mime)
      (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
  (if (file-directory-p file)
      (error "%s is a directory, cannot attach" file))
  (if (not (file-exists-p file))
      (error "No such file: %s" file))
  (if (not (file-readable-p file))
      (error "You don't have permission to read %s" file))
  (and charset (setq charset (list (concat "charset=" charset))))
  (and description (setq description (vm-mime-scrub-description description)))
  (vm-mime-attach-object file type charset description nil))


)

;;; end of vm-patches.el
