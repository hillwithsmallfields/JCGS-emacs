;;;; delatex.el -- expand and remove LaTeXery from a file
;;; Time-stamp: <2007-03-12 11:18:53 john>

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

(defvar delatex-replacements
  '(("\\\\ " . " ")
    ("%.+$" . "")
    ("\\\\'a" . "á")
    ("\\\\'{a}" . "á")
    ("\\\\'e" . "é")
    ("\\\\'{e}" . "é")
    ("\\\\'i" . "í")
    ("\\\\'{i}" . "í")
    ("\\\\'o" . "ó")
    ("\\\\'{o}" . "ó")
    ("\\\\'u" . "ú")
    ("\\\\'{u}" . "ú")
    ("\\\\\"a" . "ä")
    ("\\\\\"{a}" . "ä")
    ("\\\\={a}" . "a")
;;     ("\'{}" . "")
;;     ("\'{}" . "")
     ("\\\\marginpar{\\(.+\\)}" . "(Marginal note: \\1)")
     ("\\\\title{\\(.+\\)}" . "Title: \\1")
     ("\\\\chapter{\\(.+\\)}" . "Chapter: \\1")
     ("\\\\texttt{\\(.+\\)}" . "\\1")
     ("\\\\textit{\\(.+\\)}" . "\\1")
     ("\\\\begin{jotting}" . "(Begin temporary note:")
     ("\\\\end{jotting}" . "; temporary note ends)")
     ("\\\\ldots" . "...")
     ("\\\\begin{quotation}" . "")
     ("\\\\end{quotation}" . "")
     ("^ +$" . "")
     ("\n\n\n+" . "\n\n")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
;;     ("" . "")
))

(defun delatex (latex-file)
  "Expand and remove LaTeXery from FILE producing a .txt version."
  (interactive "fDeLaTeX file: ")
  (let ((text-file (concat (if (string-match "\\.tex$" latex-file)
			       (substring latex-file 0 -4)
			     latex-file)
			   ".txt"))
	(resume (point-min)))
    (copy-file latex-file text-file t)
    (find-file text-file)
    (while (progn
	     (goto-char resume)
	     (re-search-forward "\\\\include{\\(.+\\)}" (point-max) t))
      (let ((sub (match-string-no-properties 1)))
	(setq resume (match-beginning 0))
	(delete-region resume (point))
	(insert-file-contents (concat sub ".tex")))))
  (goto-char (point-min))
  (let ((commands nil))
    (while (re-search-forward "\\\\newcommand{\\\\\\([^}]+\\)}{\\([^}]+\\)}" (point-max) t)
      (push (cons (match-string-no-properties 1)
		  (match-string-no-properties 2))
	    commands)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (let ((pattern (concat "\\\\" (regexp-opt (mapcar 'car commands) t))))
      (while (re-search-forward pattern (point-max) t)
	(let ((command (match-string-no-properties 1)))
	  (replace-match (cdr (assoc command commands)) t t)))))
  (apply-replace-regexp-alist delatex-replacements
			      (point-min) (point-max) t nil)
  (basic-save-buffer))

(provide 'delatex)

;;; end of delatex.el
