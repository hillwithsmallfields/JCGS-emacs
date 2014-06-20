;;;; fix.el -- fix
;;; Time-stamp: <2006-05-26 19:44:22 john>

(provide 'fix)

;; here a quickly written tool to fix that kind of errors

(defun join-failing-text-lines ()
  "Quick tool to rearange splitted lines from .el-files "
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([A-Za-zäöüÄÖÜß]+\\).+$" nil t 1)
      (beginning-of-line)
      (if (y-or-n-p "Join lines?")
          (join-line)
        (end-of-line)))))



;;; end of fix.el
