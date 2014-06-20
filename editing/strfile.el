;;;; strfile.el -- edit fortune files
;;; Time-stamp: <2005-03-03 16:51:31 jcgs>

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

;; todo: merge with fortune.el

(provide 'strfile)

;;;###autoload
(defun strfile-mode ()
  "Major mode for editing fortune files."
  (interactive)
  (text-mode)
  (setq major-mode 'strfile-mode
	mode-name "StrFile"
	paragraph-separate "\\(%$\\)\\|\\(\n\\)"
	paragraph-start "\\(%$\\)\\|\\(\n\\)")
  ;; (add-hook 'local-write-file-hooks 'strfile-write-file)
  (add-hook 'after-save-hook 'strfile-written-file nil t)
  )

(defun strfile-written-file ()
  "Write a fortune file."
  (if (memq system-type '(unix))
      (shell-command "strfile %s" buffer-file-truename))
  t
  )

(defun sort-fortunes ()
  ""
  (interactive)
  (sort-subr nil
	     ;; nextrec
	     (lambda () (if (not (eobp)) (next-line 1)))
	     ;; endrec
	     (lambda ()
	       (if (re-search-forward "^%$" (point-max) t)
		   (goto-char (match-beginning 0))
		 (goto-char (point-max))))))
