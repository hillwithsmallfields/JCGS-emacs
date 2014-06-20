;;;; room-log -- note the temperature and humidity
;;; Time-stamp: <2003-11-03 14:59:22 john>

(defun room-log (temperature humidity)
  (interactive "nTemperature: 
nHumidity: ")
  (save-window-excursion
    (find-file "~/room-log")
    (goto-char (point-min))
    (search-forward "'(" (point-max) t)
    (insert (format "(\"%s\" %d %d)\n" (current-time-string) temperature humidity))
    (basic-save-buffer)
    (bury-buffer)))
