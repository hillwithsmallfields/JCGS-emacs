;;;; html-from-latex.el -- Assistance for creating HTML from LaTeX documents
;;; Time-stamp: <2005-02-24 11:03:40 jcgs>

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

(provide 'html-from-latex)
(require 'replace-regexp-list)

(defvar latex-2-html-patterns nil
  "Edits to make for a first bash at htmlizing latex")

(setq latex-2-html-patterns
      '(("Time stamp<\\([^>]+\\)>" . "Time stamp: \\1")
	("\\\\\\([a-z]+ ?\\)$" . "\\\\\\1 ")
	("\\\\par" . "<p>")
	("\\\\-" . "")
	("\\\\begin{itemize}" . "<ul>")
	("\\\\end{itemize}" . "</ul>")
	("\\\\begin{codexample}" . "<ul>")
	("\\\\end{codexample}" . "</ul>")
	("\\\\begin{verbatim}" . "<pre>")
	("\\\\end{verbatim}" . "</pre>")
	("\\\\begin{chendquote}" . "<hr>\n<blockquote>")
	("\\\\end{chendquote}" . "</blockquote>")
	("\\\\item" . "<li>")
	("%\\(.+\\)$" . "<! \\1>")
	("{\\\\em \\([^}]+\\)}" . "<em>\\1</em>")
	("{\\\\it \\([^}]+\\)}" . "<it>\\1</it>")
	("{\\\\tt \\([^}]+\\)}" . "<code>\\1</code>")
	("\\\\chapter ?{\\([^}]+\\)}" . "<h1>\\1</h1>")
	("\\\\section ?{\\([^}]+\\)}" . "<h2>\\1</h2>")
	("\\\\subsection ?{\\([^}]+\\)}" . "<h3>\\1</h3>")
	("\\\\label{\\([^}]+\\)}" . "<a name=\"\\1\"></a>")
	("\\\\ref{\\([^}]+\\)}" . "<a href=\"#\\1\">\\1</a>")
	("\\\\input{\\([^}]+\\)}" . "<a href=\"\\1.html\">\\1</a>")
	("\\\\cite{\\([^}]+\\)}" .
	 "<cite><a href=\"biblio.html#\\1\">\\1</a></cite>")
	("\\\\attribute{\\([^}]+\\)}" . "<br>\\1<br>")
	("\\\\bibitem\\[\\([^]]+\\)\\]{\\([^}]+\\)}" .
	 "<DT> <a name=\"\\2\">\\1</a>\n<DD> ")
	("Biblio.Html" . "biblio.html")
	("\\\\ldots" . "...")
	("\\\\^a" . "&acirc;")
	("\\\\^o" . "&ocirc;")
	("\\\\\"o" . "&ouml;")
	("\\\\subsection{\\([^}]+\\)}" . "<h3>\\1</h3>")
	("\\\\subsubsection{\\([^}]+\\)}" . "<h4>\\1</h4>")
	("\\\\begin{equation}" . "<i>")
	("\\\\end{equation}" . "</i>")	))

(defun latex-2-html-in-place ()
  (interactive)
  "Hack latex to html in place."
  (apply-replace-regexp-alist latex-2-html-patterns))

(defun latex-2-html-region (a b buffer)
  "Roughly htmlize LaTeX text between A and B, and put into BUFFER.
Insertion is at point in that buffer."
  (interactive "r
bInsert html into buffer: ")
  (let ((latex-text (buffer-substring a b)))
    (save-window-excursion
      (set-buffer (get-buffer-create " *LaTeX to HTML conversion*"))
      (erase-buffer)
      (insert latex-text)
      (latex-2-html-in-place)
      (append-to-buffer buffer (point-min) (point-max)))))

;;; end of html-from-latex.el
