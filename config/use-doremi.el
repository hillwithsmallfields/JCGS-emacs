;;; use-doremi.el --- load and configure DoReMi

;; frame-fns
;; frame-cmd
;; ring+

;; not yet usable, because it needs frame-fns.el
;; (use-package doremi
;; 	     "$GATHERED/emacs/doremi/"
;; 	     "http://www.emacswiki.org/cgi-bin/wiki/doremi.el"
;; 	     ((require 'doremi-cmd)
;; 	      (doremi-color-themes "doremi-cmd" "autoloaded" t)
;; 	      (doremi-bookmarks "doremi-cmd" "autoloaded" t)
;; 	      (doremi-buffers "doremi-cmd" "autoloaded" t)
;; 	      (doremi-marks "doremi-cmd" "autoloaded" t)
;; 	      (doremi-global-marks "doremi-cmd" "autoloaded" t)
;;  	      (doremi-font-size "doremi-frm" "autoloaded" t)
;; 	      (doremi-font "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-color-name "doremi-frm" "autoloaded" t)
;; 	      (doremi-frame-width "doremi-frm" "autoloaded" t)
;; 	      (doremi-frame-height "doremi-frm" "autoloaded" t)
;; 	      (doremi-frame-horizontally "doremi-frm" "autoloaded" t)
;; 	      (doremi-frame-vertically "doremi-frm" "autoloaded" t)
;; 	      (doremi-frame-configs "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-red "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-green "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-cyan "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-magenta "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-yellow "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-hue "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-saturation "doremi-frm" "autoloaded" t)
;; 	      (doremi-bg-value "doremi-frm" "autoloaded" t)
;; 	      (doremi-all-frames-bg "doremi-frm" "autoloaded" t)
;; 	      (doremi-increment-background-color "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-red "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-green "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-blue "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-cyan "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-magenta "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-yellow "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-hue "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-saturation "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg-value "doremi-frm" "autoloaded" t)
;; 	      (doremi-fg "doremi-frm" "autoloaded" t)
;; 	      (doremi-all-frames-fg "doremi-frm" "autoloaded" t)
;; 	      (doremi-increment-foreground-color "doremi-frm" "autoloaded" t)
;; 	      (doremi-undo-last-frame-color-change "doremi-frm" "autoloaded" t)
;; 	      (doremi-undo-last-face-change "doremi-frm" "autoloaded" t)
;; 	      (doremi-all-faces-bg "doremi-frm" "autoloaded" t)
;; 	      (doremi-increment-face-bg-color "doremi-frm" "autoloaded" t)
;; 	      (doremi-all-faces-fg "doremi-frm" "autoloaded" t)
;; 	      (doremi-increment-face-fg-color "doremi-frm" "autoloaded" t)
;; 	      (doremi-increment-color-component "doremi-frm" "autoloaded" t)
;; 	      (doremi-toggle-wrap-color "doremi-frm" "autoloaded" t))
;;            )

(setq doremi-up-key 'kp-divide
      doremi-down-key 'S-kp-divide
      doremi-boost-up-key 'kp-end
      doremi-boost-down-key 'S-kp-1
      doremi-boost-scale-factor 8)

;;; end of use-doremi.el
