;;;; fill in missing functions not present on a particular Emacs
;;; Time-stamp: <04/11/04 22:56:03 jcgs>

(if (not (fboundp 'match-string-no-properties))
    (defun match-string-no-properties (n)
      "Patched-in substitute for the real thing."
      (message "match-string-no-properties %d" n)
      (buffer-substring-no-properties (match-beginning n) (match-end n))))

;;; end of version-patches.el

