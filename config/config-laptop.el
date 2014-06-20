;;; config-laptop.el --- setup for my laptop/netbook

;; Copyright (C) 2010  John Sturdy

(when (string-match "ezra" (system-name))
  (setq battery-mode-line-format "[%b%p%%,%d°C,%t]")
  (display-battery-mode))

;;; config-laptop.el ends here
