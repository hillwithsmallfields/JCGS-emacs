;;;; sysadmin.el -- useful stuff for running system
;;; Time-stamp: <2009-02-03 10:47:21 jcgs>

(provide 'sysadmin)

(defvar dmesg-buffer "*System messages*"
  "The name of the system messages buffer.")

(defun dmesg ()
  "Run dmesg and display the results."
  (interactive)
  (set-buffer (get-buffer-create dmesg-buffer))
  (erase-buffer)
  (shell-command "dmesg" dmesg-buffer)
  (pop-to-buffer dmesg-buffer))

  
