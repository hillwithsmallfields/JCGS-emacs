;;;; load-latex-elisp.el -- load a file of LaTeX with elisp embedded in it
;;; Time-stamp: <2007-04-09 19:06:40 jcgs>

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

(defun load-latex-elisp-file (file)
  "Load FILE which should have a mixture of LaTeX and emacs-lisp.
The emacs-lisp sections are taken to be all \"verbatim\" environments
whose first non-whitespace character is an open parenthesis."
  (interactive "fLoad file: ")
  (let ((visiting (find-buffer-visiting file)))
    (find-file file)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (search-forward "\begin{verbatim}" nil t)
	  (let ((verbatim-begin (point)))
	    (when (search-forward "\end{verbatim}" nil t)
	      (let ((end-end (point))
		    (verbatim-end (match-beginning 0)))
		(goto-char verbatim-begin)
		(skip-syntax-forward "-" verbatim-end)
		(when (looking-at "(")
		  (save-excursion
		    (condition-case evar
			(eval-region (point) verbatim-end)
		      (error (error "Error %S in evaluating embedded Lisp at %d..%d in %s"
				    evar
				    verbatim-begin verbatim-end
				    (buffer-file-name))))))
		(goto-char end-end)))))))
    (unless visiting
      (kill-buffer nil))))

(provide 'load-latex-elisp)

;;; end of load-latex-elisp.el
