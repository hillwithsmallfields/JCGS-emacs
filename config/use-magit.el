;;;; find, load and configure magit
;;; Time-stamp: <2020-11-11 20:56:07 jcgs>

(require 'jcgs-use-package)

(jcgs/use-package magit
	     "$GATHERED/emacs/magit/magit-1.0/magit-1.0.0"
	     "https://github.com/downloads/magit/magit/magit-1.0.0.tar.gz"
	     ((magit-status "magit" "The main entry point for magit." t))
	     (define-key magit-mode-map (kbd "u") 'magit-goto-parent-section))

;;; use-magit.el ends here
