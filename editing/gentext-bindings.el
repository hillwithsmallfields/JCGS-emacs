;;;; gentext-bindings.el -- key binding manipulations for generic text commands
;;; Time-stamp: <2013-12-10 22:42:16 jcgs>

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

(provide 'gentext-bindings)
;; (require 'keymap-hacks)	; http://www.cb1.com/~john/computing/emacs/lisp/misc/keymap-hacks.el, for map-key-definitions

(defvar gentext-binding-equivalents
  '(
    (generic-text-end-sentence end-sentence)
    (generic-text-end-question end-question)
    (generic-text-end-exclamation end-exclamation)
    (generic-text-title tempo-template-html-title
			mail-subject)
    (generic-text-paragraph tempo-template-html-paragraph)
    (generic-text-insert-item html-helper-smart-insert-item
			      LaTeX-insert-item
			      texinfo-insert-@item)
    (generic-text-ordered-list tempo-template-html-ordered-list)
    (generic-text-unordered-list tempo-template-html-unordered-list)
    (generic-text-definition-list tempo-template-html-definition-list)
    (generic-text-table tempo-template-html-table,-no-border
			texinfo-insert-@table)
    (generic-text-table-row tempo-template-html-table-row)
    (generic-text-table-data tempo-template-html-table-data)
    (generic-text-table-heading tempo-template-html-table-heading)
    (generic-text-hyperlink tempo-template-html-hyperlink)
    (generic-text-header-1 tempo-template-html-header-1)
    (generic-text-header-2 tempo-template-html-header-2)
    (generic-text-header-3 tempo-template-html-header-3)
    (generic-text-header-4 tempo-template-html-header-4)
    (generic-text-horizontal-line tempo-template-html-horizontal-line)
    (generic-text-image tempo-template-html-aligned-image-with-alt\.-text)
    (generic-text-link-target tempo-template-html-link-target)
    (generic-text-italic tempo-template-html-italic)
    (generic-text-bold tempo-template-html-bold)
    (generic-text-strong tempo-template-html-strong
			 texinfo-insert-@strong)
    (generic-text-variable tempo-template-html-variable
			   texinfo-insert-@var)
    (generic-text-emphasized tempo-template-html-emphasized
			     texinfo-insert-@emph)
    (generic-text-keyboard tempo-template-html-keyboard-input
			   texinfo-insert-@kbd)
    (generic-text-definition tempo-template-html-definition
			     texinfo-insert-@dfn)
    (generic-text-code tempo-template-html-code
		       texinfo-insert-@code)
    (generic-text-preformatted tempo-template-html-preformatted)
    (generic-text-blockquote tempo-template-html-blockquote
			     texinfo-insert-@quotation)
    )
  "List of lists of equivalent bindings from different modes.
The first item of each sub-list is the generic, gentext, one.")

(defun gentext-bindings-equivalance-row (command)
  "Return the row from gentext-binding-equivalents that contains COMMAND, or nil."
  (let ((rows gentext-binding-equivalents))
    (catch 'found
      (while rows
	(if (memq command (car rows))
	    (throw 'found (car rows))
	  (setq rows (cdr rows))))
      nil)))

(defun gentext-bindings-generate-generic-bindings (from-bindings)
  "Look through keymap FROM-BINDINGS and generate a sparse keymap corresponding to it.

For each binding in FROM-BINDINGS of a function in any of the sublists
of gentext-binding-equivalents, the new keymap binds the head item of
that sublist to the same key.

Then, you can use this to make a generic-text keymap based on your
favourite text mode."
  (let ((new-bindings nil))
    (map-key-definitions (lambda (keys definition)
			   (let ((equivalance-row (gentext-bindings-equivalance-row definition)))
			     (if equivalance-row
				 (setq new-bindings
				       (cons (cons keys (car equivalance-row))
					     new-bindings)))))
		       from-bindings)
    new-bindings))

(defun gentext-bindings-show-equivalents (from-bindings)
  "Display the gentext bindings equivalent to relevant commands in FROM-BINDINGS."
  (interactive
   (list (symbol-value
	  (intern
	   (completing-read "Keymap: "
			    obarray
			    (function
			     (lambda (symbol)
			       (and (string-match ".+-map$" (symbol-name symbol))
				    (keymapp (symbol-value symbol)))))
			    t)))))
  (let ((bindings (gentext-bindings-generate-generic-bindings from-bindings)))
    (with-output-to-temp-buffer "*Generic text bindings*"
      (while bindings
	(princ (format "%s: %S\n" (key-description (caar bindings)) (cdar bindings)))
	(setq bindings (cdr bindings))))))

(defun gentext-setup-bindings (exemplar &rest others)
  "Find the gentext bindings corresponding to the bindings in EXEMPLAR, and bind them in OTHERS.
For example, if you want LaTeX-mode and texinfo-mode to use gentext
bindings similar to html-helper-mode, you can do the following:
  (gentext-setup-bindings html-helper-mode-map
                          LaTeX-mode-map
                          texinfo-mode-map)"
  (let ((bindings (gentext-bindings-generate-generic-bindings exemplar)))
    (keymap-overwrite-entries exemplar bindings)
    (while others
      (keymap-overwrite-entries (car others) bindings)
      (setq others (cdr others)))))

;;; end of gentext-bindings.el
