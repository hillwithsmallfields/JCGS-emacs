;;; Time-stamp: <2014-09-28 19:38:39 jcgs>

;; I don't want the extra frame for ediff, ever!

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
				    (if (> (frame-width) 150)
					(split-window-horizontally arg)
				      (split-window-vertically arg))))
