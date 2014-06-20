;;;; gentext-html.el in support of generic-text.el -- generic things that get passed to TeX, LaTeX, html, troff, texinfo etc
;;; Time-stamp: <2007-08-20 10:42:26 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


(provide 'gentext-html)
(require 'gentext-models)
(require 'text-sentences)
(require 'html-helper-mode)

(defmodal new-paragraph html-helper-mode (&optional surround)
  (interactive "P")
  (tempo-template-html-paragraph surround))

(defmodalalias generic-text-end-sentence (html-helper-mode html-mode) end-sentence)
(defmodalalias generic-text-end-question (html-helper-mode html-mode) end-question)
(defmodalalias generic-text-end-exclamation (html-helper-mode html-mode) end-exclamation)
(defmodalalias generic-text-title (html-helper-mode html-mode) tempo-template-html-title)
(defmodalalias generic-text-paragraph (html-helper-mode html-mode) tempo-template-html-paragraph)
(defmodalalias generic-text-insert-item html-helper-mode html-helper-smart-insert-item)
(defmodalalias generic-text-ordered-list (html-helper-mode html-mode) tempo-template-html-ordered-list)
(defmodalalias generic-text-unordered-list (html-helper-mode html-mode) tempo-template-html-unordered-list)
(defmodalalias generic-text-definition-list (html-helper-mode html-mode) tempo-template-html-definition-list)
;; (require 'hhm-table)			; from same dir as html-helper-mode
;; (defmodalalias generic-text-table (html-helper-mode html-mode) tempo-template-html-table,-no-border)
;; (defmodalalias generic-text-table-row (html-helper-mode html-mode) tempo-template-html-table-row)
;; (defmodalalias generic-text-table-data (html-helper-mode html-mode) tempo-template-html-table-data)
;; (defmodalalias generic-text-table-heading (html-helper-mode html-mode) tempo-template-html-table-heading)
(defmodalalias generic-text-hyperlink (html-helper-mode html-mode) tempo-template-html-hyperlink)
(defmodalalias generic-text-header-1 (html-helper-mode html-mode) tempo-template-html-header-1)
(defmodalalias generic-text-header-2 (html-helper-mode html-mode) tempo-template-html-header-2)
(defmodalalias generic-text-header-3 (html-helper-mode html-mode) tempo-template-html-header-3)
(defmodalalias generic-text-header-4 (html-helper-mode html-mode) tempo-template-html-header-4)
(defmodalalias generic-text-horizontal-line (html-helper-mode html-mode) tempo-template-html-horizontal-line)
(defmodalalias generic-text-image (html-helper-mode html-mode) tempo-template-html-aligned-image-with-alt\.-text)
(defmodalalias generic-text-link-target (html-helper-mode html-mode) tempo-template-html-link-target)
(defmodalalias generic-text-italic (html-helper-mode html-mode) tempo-template-html-italic)
(defmodalalias generic-text-bold (html-helper-mode html-mode) tempo-template-html-bold)
(defmodalalias generic-text-emphasized (html-helper-mode html-mode) tempo-template-html-emphasized)
(defmodalalias generic-text-strong (html-helper-mode html-mode) tempo-template-html-strong)
(defmodalalias generic-text-variable (html-helper-mode html-mode) tempo-template-html-variable)
(defmodalalias generic-text-keyboard (html-helper-mode html-mode) tempo-template-html-keyboard-input)
(defmodalalias generic-text-definition (html-helper-mode html-mode) tempo-template-html-definition)
(defmodalalias generic-text-code (html-helper-mode html-mode) tempo-template-html-code)
(defmodalalias generic-text-preformatted (html-helper-mode html-mode) tempo-template-html-preformatted)
(defmodalalias generic-text-blockquote (html-helper-mode html-mode) tempo-template-html-blockquote)

;;; end of gentext-html.el
