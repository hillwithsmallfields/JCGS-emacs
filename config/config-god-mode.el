;;;; Configuration for god=mode
;;; Time-stamp: <2023-04-23 19:25:13 jcgs>

;; Copyright 2018, 2023, John C. G. Sturdy

;; Author: John C. G. Sturdy <jcg.sturdy@gmail.com>
;; Maintainer: John C. G. Sturdy <jcg.sturdy@gmail.com>
;; Created: 2018
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

(when nil
  (let ((god-mode-directory (substitute-in-file-name "$OPEN_PROJECTS/god-mode")))
    (when (file-exists-p (expand-file-name "god-mode.el"
					   god-mode-directory))
      (add-to-list 'load-path god-mode-directory)

      (require 'god-mode)

      (defun jcgs/god-mode-indicator ()
        (cond (god-local-mode
	       (progn
	         (set-face-background 'mode-line "red4")
	         (set-face-foreground 'mode-line "gray")
	         (set-face-background 'mode-line-inactive "gray30")
	         (set-face-foreground 'mode-line-inactive "red")))
	      (t
	       (progn
	         (set-face-background 'mode-line-inactive "gray30")
	         (set-face-foreground 'mode-line-inactive "gray80")
	         (set-face-background 'mode-line "gray75")
	         (set-face-foreground 'mode-line "black")))))

      (add-hook 'god-mode-enabled-hook #'jcgs/god-mode-indicator)
      (add-hook 'god-mode-disabled-hook #'jcgs/god-mode-indicator))))

;;; end of config-god-mode.el
