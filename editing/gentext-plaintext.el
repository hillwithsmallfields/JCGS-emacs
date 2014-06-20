;;;; gentext-plaintext.el in support of generic-text.el -- generic things that get passed to TeX, LaTeX, html, troff, texinfo etc
;;; Time-stamp: <2006-01-29 23:04:57 jcgs>

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

(provide 'gentext-plaintext)
(require 'gentext-models)

;;; not yet written -- these are automatically generated stubs

(defun insert-or-surround (surround before after)
  "If SURROUND, put BEFORE before the region, and AFTER after it. Otherwise just insert BEFORE AFTER."
  (if surround
      (save-excursion
	(goto-char (region-end))
	(insert after)
	(goto-char (region-beginning))
	(insert before))
    (insert before)
    (save-excursion
      (insert after))))
 
(defmodal generic-text-title (text-mode mail-mode) (&optional surround)
  "Insert a title.
With non-nil (prefix) argument, surround the region with a title."
  (interactive "P")
  (if surround
      (let* ((start (save-excursion (goto-char (region-beginning)) (current-column)))
	     (end (save-excursion (goto-char (region-end)) (current-column))))
	(end-of-line 1)
	(insert "\n" (make-string start ? ) (make-string (- end start) ?=) "\n"))))

(defmodal new-paragraph (text-mode mail-mode) (&optional surround)
  "Insert a paragraph.
With non-nil (prefix) argument, surround the region with a paragraph."
  (interactive "P")
  (insert-or-surround surround "\n" "\n"))

(defmodal generic-text-end-sentence (text-mode mail-mode) ()
  "Insert a period here, and capitalize the first word in the sentence."
  (interactive)
  (end-sentence))

(defmodal generic-text-end-question (text-mode mail-mode) ()
  "Insert a question mark here, and capitalize the first word in the sentence."
  (interactive)
  (end-question))

(defmodal generic-text-end-exclamation (text-mode mail-mode) ()
  "Insert an exclamation mark here, and capitalize the first word in the sentence."
  (interactive)
  (end-exclamation))

(defmodal generic-text-paragraph (text-mode mail-mode) (&optional surround)
  "Insert a paragraph.
With non-nil (prefix) argument, surround the region with a paragraph."
  (interactive "P")
  (insert-or-surround surround "\n" "\n"))

(defmodal generic-text-insert-item (text-mode mail-mode) (&optional surround)
  "Insert a insert-item.
With non-nil (prefix) argument, surround the region with a insert-item."
  (interactive "P"))

(defmodal generic-text-ordered-list (text-mode mail-mode) (&optional surround)
  "Insert a ordered-list.
With non-nil (prefix) argument, surround the region with a ordered-list."
  (interactive "P"))

(defmodal generic-text-unordered-list (text-mode mail-mode) (&optional surround)
  "Insert a unordered-list.
With non-nil (prefix) argument, surround the region with a unordered-list."
  (interactive "P"))

(defmodal generic-text-definition-list (text-mode mail-mode) (&optional surround)
  "Insert a definition-list.
With non-nil (prefix) argument, surround the region with a definition-list."
  (interactive "P"))

(defmodal generic-text-table (text-mode mail-mode) (&optional surround)
  "Insert a table.
With non-nil (prefix) argument, surround the region with a table."
  (interactive "P"))

(defmodal generic-text-table-row (text-mode mail-mode) (&optional surround)
  "Insert a table-row.
With non-nil (prefix) argument, surround the region with a table-row."
  (interactive "P"))

(defmodal generic-text-table-data (text-mode mail-mode) (&optional surround)
  "Insert a table-data.
With non-nil (prefix) argument, surround the region with a table-data."
  (interactive "P"))

(defmodal generic-text-table-heading (text-mode mail-mode) (&optional surround)
  "Insert a table-heading.
With non-nil (prefix) argument, surround the region with a table-heading."
  (interactive "P"))

(defmodal generic-text-hyperlink (text-mode mail-mode) (&optional surround)
  "Insert a hyperlink.
With non-nil (prefix) argument, surround the region with a hyperlink."
  (interactive "P"))

(defmodal generic-text-header-1 (text-mode mail-mode) (&optional surround)
  "Insert a header-1.
With non-nil (prefix) argument, surround the region with a header-1."
  (interactive "P"))

(defmodal generic-text-header-2 (text-mode mail-mode) (&optional surround)
  "Insert a header-2.
With non-nil (prefix) argument, surround the region with a header-2."
  (interactive "P"))

(defmodal generic-text-header-3 (text-mode mail-mode) (&optional surround)
  "Insert a header-3.
With non-nil (prefix) argument, surround the region with a header-3."
  (interactive "P"))

(defmodal generic-text-header-4 (text-mode mail-mode) (&optional surround)
  "Insert a header-4.
With non-nil (prefix) argument, surround the region with a header-4."
  (interactive "P"))

(defmodal generic-text-horizontal-line (text-mode mail-mode) (&optional surround)
  "Insert a horizontal-line.
With non-nil (prefix) argument, surround the region with a horizontal-line."
  (interactive "P")
  (insert (make-string fill-column ?-)))

(defmodal generic-text-image (text-mode mail-mode) (&optional surround)
  "Insert a image.
With non-nil (prefix) argument, surround the region with a image."
  (interactive "P"))

(defmodal generic-text-link-target (text-mode mail-mode) (&optional surround)
  "Insert a link-target.
With non-nil (prefix) argument, surround the region with a link-target."
  (interactive "P"))

(defmodal generic-text-italic (text-mode mail-mode) (&optional surround)
  "Insert a italic.
With non-nil (prefix) argument, surround the region with a italic."
  (interactive "P")
  (insert-or-surround surround "_" "_"))

(defmodal generic-text-bold (text-mode mail-mode) (&optional surround)
  "Insert a bold.
With non-nil (prefix) argument, surround the region with a bold."
  (interactive "P")
  (insert-or-surround surround "*" "*"))

(defmodal generic-text-emphasized (text-mode mail-mode) (&optional surround)
  "Insert a emphasized.
With non-nil (prefix) argument, surround the region with a emphasized."
  (interactive "P")
  (insert-or-surround surround "*" "*"))

(defmodal generic-text-definition (text-mode mail-mode) (&optional surround)
  "Insert a definition.
With non-nil (prefix) argument, surround the region with a definition."
  (interactive "P"))

(defmodal generic-text-code (text-mode mail-mode) (&optional surround)
  "Insert a code.
With non-nil (prefix) argument, surround the region with a code."
  (interactive "P"))

(defmodal generic-text-preformatted (text-mode mail-mode) (&optional surround)
  "Insert a preformatted.
With non-nil (prefix) argument, surround the region with a preformatted."
  (interactive "P"))

(defmodal generic-text-blockquote (text-mode mail-mode) (&optional surround)
  "Insert a blockquote.
With non-nil (prefix) argument, surround the region with a blockquote."
  (interactive "P"))

;;; end of gentext-plaintext.el
