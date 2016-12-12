;;;; text-sentences.el -- handling of natural language structures such as sentences
;;; Time-stamp: <2013-12-10 22:41:00 jcgs>

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

(provide 'text-sentences)
(add-to-list 'load-path (expand-file-name "my-extensions-to-packages/emacspeak"
					  user-emacs-directory))
(require 'electric-space)

;;;###autoload
(defun html-backward-sentence (&optional arg)
  "Go back to the start of the sentence, HTML-sensitively."
  (interactive)
  ;; todo: make this behave when the text is indented. Currently, it treats the left margin (on an indented line) as the start of the paragraph, and hence also of the sentence, even if the sentence starts further back.
  (backward-sentence arg)
  (if (looking-at "<[^>]+>")
      (goto-char (match-end 0))))

(defun latex-backward-sentence (&optional arg)
  "Go back to the start of the sentence, LaTeX-sensitively."
  (let ((limit (save-excursion
		 (skip-chars-backward "^{")
		 (point))))
    (backward-sentence arg)
    (if (< (point) limit)
	(goto-char limit))
    (when (looking-at "\\\\[a-z]+")
      (goto-char (match-end 0))
      (skip-syntax-forward " "))))

(defun after-sentence-space-if-needed ()
  "Return a space if one is needed in this context."
  (if (looking-at "[\"\')]")
      ""
    (if sentence-end-double-space
	"  "
      " ")))

(defvar end-sentence-hook nil
  "Functions to run on declaring the end of a sentence.
Run by end-sentence-with.")

;;;###autoload
(defun end-sentence-with (&optional arg)
  "End a sentence, that is, capitalize the first word, and insert a sentence terminator if given.
The argument is the sentence terminator.
Runs functions on end-sentence-hook first."
  (interactive)
  (run-hooks 'end-sentence-hook)
  (save-excursion
    (let ((end-of-sentence (point)))
      (cond
       ;; turn this into a modal function!
       ((memq major-mode '(html-mode html-helper-mode html-journal-helper-mode))
 	(html-backward-sentence))
       ((memq major-mode '(latex-mode))
	(latex-backward-sentence))
       (t (backward-sentence)))
      (unless (let ((case-fold-search nil))
		(looking-at "\\<[A-Z]+\\>")) ; don't spoil all-caps words
	(capitalize-word 1))
      (let ((open-quotes (re-search-forward "\"\|``" end-of-sentence t)))
	(when open-quotes
	  (let ((close-quotes (re-search-forward "\"\|''" end-of-sentence t)))
	    (unless close-quotes
	      (goto-char open-quotes)
	      (capitalize-word)))))))
  (when (and arg
	     (not (looking-at (regexp-quote arg))))
    (delete-horizontal-space)
    ;; todo: make this idempotent
    (insert arg
	    (after-sentence-space-if-needed))))

;;;###autoload
(defun end-sentence ()
  "Capitalize the first word of the current sentence, and insert a full stop, with following space if needed."
  (interactive)
  (end-sentence-with "."))

;;;###autoload
(defun end-question ()
  "Capitalize the first word of the current sentence, and insert a question mark, with following space if needed."
  (interactive)
  (end-sentence-with "?"))

;;;###autoload
(defun end-exclamation ()
  "Capitalize the first word of the current sentence, and insert an exclamation mark, with following space if needed."
  (interactive)
  (end-sentence-with "!"))

;;; end of text-sentences.el
