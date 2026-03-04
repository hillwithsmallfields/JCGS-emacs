;;; config-laptop.el --- setup for my laptop/netbook

;; Copyright (C) 2010, 2025  John Sturdy

(when (member (system-name) '("elijah" "elisha"))
  (setq battery-mode-line-format "[%b%p%%,%d°C,%t]")
  (display-battery-mode))

;;; config-laptop.el ends here
