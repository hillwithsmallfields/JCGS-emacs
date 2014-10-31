;;;; config-personal.el --- mount my personal journal etc
;;; Time-stamp: <2014-10-31 08:37:13 jcgs>

(defun personal-setup (&optional manual)
  "Set up my personal files volume.
Optional argument MANUAL indicates running this manually."
  (interactive (list t))
  (unless (file-directory-p "/mnt/crypted/jcgs/personal")
    (error "Personal files volume not mounted"))
  (load-file "/mnt/crypted/jcgs/personal/mount-drive.el")
  (when manual
    (personal-drive-after-init-function)))

(when (file-directory-p "/mnt/crypted/jcgs/personal")
  (personal-setup))

;;; config-personal.el ends here
