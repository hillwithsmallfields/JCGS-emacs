;;;; use-color-theme.el -- acquire and use colour themes
;;; Time-stamp: <2020-11-11 20:56:08 jcgs>

(when window-system
  (require 'jcgs-use-package)
  (jcgs-use-package color-theme
	       "$GATHERED/emacs/color-theme"
	       "http://download.gna.org/color-theme/color-theme-6.6.0.tar.gz"
	       ((color-theme-select "color-theme" nil t))))

;;; end of use-color-theme.el
