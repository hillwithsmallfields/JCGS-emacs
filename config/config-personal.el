;;;; config-personal.el --- mount my personal journal etc
;;; Time-stamp: <2013-12-10 22:49:03 jcgs>

(defun personal-setup (&optional manual)
  "Set up my personal files volume."
  (interactive (list t))
  (unless (file-directory-p "/mnt/crypted/personal")
    (error "Personal files volume not mounted"))
  (load-file "/mnt/crypted/personal/mount-drive.el")
  (when manual
    (personal-drive-after-init-function)))

(when (file-directory-p "/mnt/crypted/personal")
  (personal-setup))

;;; config-personal.el ends here
