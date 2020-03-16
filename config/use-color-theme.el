;;;; use-color-theme.el -- acquire and use colour themes
;;; Time-stamp: <2020-03-16 14:23:09 jsturdy>

(when window-system
  (use-package color-theme
	       "$GATHERED/emacs/color-theme"
	       "http://download.gna.org/color-theme/color-theme-6.6.0.tar.gz"
	       ((color-theme-select "color-theme" nil t))))

;;; end of use-color-theme.el
