;;; tmp.el --- temporary bits for startup

(defun url-test ()
  "Try out url fetching."
  (interactive)
  (let ((stack-trace-on-error t))
    (url-retrieve-synchronously "http://www.cb1.com/~john/index.html")))

(unless (boundp 'use-package-skip-these)
  (setq use-package-skip-these nil))

;;; tmp.el ends here
