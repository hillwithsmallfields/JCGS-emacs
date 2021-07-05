;;;; use-psgml.el -- load and configure psgml
;;; Time-stamp: <2021-02-12 20:11:42 jcgs>

;; Copyright (C) 2007, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007

;; This file is NOT part of GNU Emacs.

;; (fmakunbound 'xml-mode)			; force loading of new one

(require 'jcgs-use-package)

(jcgs/use-package psgml
	     "$GATHERED/emacs/psgml-1.3.2"
	     "http://www.lysator.liu.se/~lenst/about_psgml/"
	     ((sgml-mode "psgml" "Major mode to edit SGML files." t)
	      (xml-mode "psgml" "Major mode to edit XML files." t)
	      ("\\.xml$" . xml-mode)
	      ("\\.xmlex$" . xml-mode)
	      )
	     (setq sgml-markup-faces
		   '((start-tag . font-lock-function-name-face)
		     (end-tag . font-lock-function-name-face)
		     (comment . font-lock-comment-face)
		     (pi . bold)
		     (sgml . bold)
		     (doctype . bold)
		     (entity . font-lock-type-face)
		     (shortref . font-lock-function-name-face))
		   sgml-set-face t)
	     (setq-default sgml-indent-data t)
	     ;; Some convenient key definitions:
	     (define-key sgml-mode-map "\C-c\C-x\C-e" 'sgml-describe-element-type)
	     (define-key sgml-mode-map "\C-c\C-x\C-i" 'sgml-general-dtd-info)
	     (define-key sgml-mode-map "\C-c\C-x\C-t" 'sgml-describe-entity))

(provide 'use-psgml)

;;; use-psgml.el ends here
