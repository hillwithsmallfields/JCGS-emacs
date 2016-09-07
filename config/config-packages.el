;;; config-packages.el --- set up my packages downloads  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Sturdy

;; Time-stamp: <2016-09-06 15:51:13 johstu01>

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
	(install-package package))
    (error (message "Problem installing package %s: %s" package error))))

(defvar jcgs/packages '(firefox-controller popwin moz god-mode golden-ratio google google-contacts oauth2 google-maps google-translate moz-controller moz oauth2 org-alert alert log4e gntp dash s org-beautify-theme org-clock-convenience org org-dashboard org-doing org-dropbox names dash org-fstree org-gcal org alert log4e gntp request-deferred request deferred org-jira org-journal org-outlook org-page git f dash s dash s dash org htmlize mustache dash s ht simple-httpd ht org-password-manager s org org-pomodoro alert log4e gntp org-random-todo alert log4e gntp org-tracktable org-webpage web-server dash org htmlize mustache dash s ht ht org2blog xml-rpc org popwin request-deferred request deferred s scad-preview scad-mode scala-mode scala-outline-popup flx-ido flx scala-mode2 popup dash scheme-complete scheme-here simple-httpd sotlisp ssh stumpwm-mode sudo-ext sx let-alist markdown-mode symbols-mode thumb-through tomatinho unbound undo-tree uuid uuidgen web-server which-key window-jump window-layout window-purpose imenu-list let-alist wonderland multi dash-functional dash dash xml-rpc yaml-mode yatemplate yasnippet)
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

(add-hook 'after-init-hook 'jcgs/config-packages-after-init-function)

(provide 'config-packages)

;;; config-packages.el ends here
