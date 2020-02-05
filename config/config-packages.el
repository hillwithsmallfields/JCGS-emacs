;;; config-packages.el --- set up my packages downloads  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018, 2019  John Sturdy

;; Time-stamp: <2019-11-22 20:00:47 jcgs>

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See:
;; - https://www.emacswiki.org/emacs/ELPA
;; - http://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
;; - http://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html

;;; Code:

(setq package-user-dir (substitute-in-file-name "$EHOME/emacs-packages"))
(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir))
(require 'package)
(add-to-list 'package-archives (cons "marmalade" "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making my choice available ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-package (package)
  "Ensure that PACKAGE is installed."
  (condition-case error
      (if (package-installed-p package)
	  (message "Package %s already installed" package)
	(message "Installing package %s" package)
	(package-install package))
    (error (message "Problem installing package %s: %s" package error))))

(defvar jcgs/packages '(
			alert
			dash
			dash-functional
			deferred
			f
			firefox-controller
			flx
			flx-ido
			git
			gntp
			god-mode
			golden-ratio
			google
			google-contacts
			google-maps
			google-translate
			ht
			htmlize
			imenu-list
			let-alist
			log4e
			markdown-mode
			moz
			moz-controller
			multi
			mustache
			names
			oauth2
			org
			org2blog
			org-alert
			org-beautify-theme
			org-clock-convenience
			org-dashboard
			org-doing
			org-dropbox
			org-fstree
			org-gcal
			org-jira
			org-journal
			org-outlook
			org-page
			org-password-manager
			org-pomodoro
                        org-ql
			org-random-todo
			org-tracktable
			org-webpage
			popup
			popwin
			request
			request-deferred
			s
			scad-mode
			scad-preview
			scala-mode
			scala-mode2
			scala-outline-popup
			scheme-complete
			scheme-here
			simple-httpd
			sotlisp
			ssh
			stumpwm-mode
			sudo-ext
			sx
			symbols-mode
			thumb-through
			tomatinho
			unbound
			undo-tree
			uuid
			uuidgen
			web-server
			which-key
			window-jump
			window-layout
			window-purpose
			wonderland
			xml-rpc
			yaml-mode
			yasnippet
			yatemplate
			)
  "The packages I want installed.
Derived from `package-activated-list'.")

;;;;;;;;;;;;;;;;;;;;;;
;; version patching ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/config-packages-after-init-function ()
  "Patch some packages."
  (interactive)
  (mapcar 'require-package jcgs/packages)
  (dolist (base '("org-20160411/org.el"
		  "org-20160411/org-agenda.el"
		  "org-20160411/org-compat.el"))
    (let* ((file (expand-file-name base package-user-dir))
	   (elc (concat file "c")))
      (if (file-exists-p elc)
	  (progn
	    (message "Patching with %s" elc)
	    (load-file elc))
	(when (file-exists-p file)
	  (message "Patching with %s" file)
	  (load-file file))))))

;; (add-hook 'after-init-hook 'jcgs/config-packages-after-init-function)

(provide 'config-packages)

;;; config-packages.el ends here
