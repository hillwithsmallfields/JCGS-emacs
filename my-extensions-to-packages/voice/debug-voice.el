;;;; debug-voice.el -- Some debugging code for voice
;;; Time-stamp: <2005-01-07 16:50:45 John.Sturdy>

(provide 'debug-voice)

(defun vr-clear-log ()
  "Clear the vr log"
  (interactive)
  (save-window-excursion
    (set-buffer (get-buffer-create " *vr*"))
    (erase-buffer)))

(defun vr-log-in-frame ()
  "Show the vr log in a frame of its own"
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create " *vr*")))

(defvar keys nil)

(defun record-key ()
  (push (this-command-keys) keys))

(defun dump-keys ()
  (interactive)
  (switch-to-buffer-other-window "*keys*")
  (erase-buffer)
  (dolist (key (reverse keys))
    (insert (format "%S %s\n" key (key-description key)))))

(defun start-recording-keys ()
  (interactive)
  (add-hook 'pre-command-hook 'record-key))

(defun stop-recording-keys ()
  (interactive)
  (remove-hook 'pre-command-hook 'record-key)
  (dump-keys))

;;; end of debug-voice.el
