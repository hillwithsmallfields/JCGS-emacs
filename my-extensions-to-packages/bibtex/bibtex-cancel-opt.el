;;;; bibtex-cancel-opt.el -- Remove OPT strings when editing a BibTeX line
;;; Time-stamp: <2006-07-07 15:20:33 john>

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

(provide 'bibtex-cancel-opt)

(defun bibtex-cancel-opt (begin end length)
  "If the line being changed is an OPT field, and the value is non-blank, remove the OPT marker."
  (save-excursion
    (save-match-data
      (goto-char begin)
      (beginning-of-line 1)
      (message "change at %d, bol %d" begin (point))
      (when (looking-at "^\\s *\\(OPT\\)[a-z]+\\s *=\\s *{.+}")
	(delete-region (match-beginning 1) (match-end 1))))))

(defun bibtex-setup-cancel-opt ()
  "Arrange for bibtex-cancel-opt to be called as needed."
  (interactive)
  (add-hook 'after-change-function 'bibtex-cancel-opt nil t))

(add-hook 'bibtex-mode-hook 'bibtex-setup-cancel-opt)

;;; end of bibtex-cancel-opt.el
