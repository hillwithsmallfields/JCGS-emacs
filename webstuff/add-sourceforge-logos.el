;;;; add-sourceforge-logos.el -- add sourceforge logos to html files
;;; Time-stamp: <2006-03-30 14:44:30 john>

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

(provide 'add-sourceforge-logos)

(defun add-sourceforge-logo-to-file (file)
  (interactive "fAdd logo to file:")
  (if (file-directory-p file)
      (mapcar 'add-sourceforge-logo-to-file  (directory-files file t "\\.html$" t))
    (let ((visiting (find-buffer-visiting file)))
      (save-window-excursion
	(find-file file)
	(save-excursion
	  (goto-char (point-min))
	  (unless (search-forward "sflogo" nil t)
	    (goto-char (point-min))
	    (re-search-forward "<body[^>]*>")
	    (insert "<a href=\"http://sourceforge.net\"><img
  src=\"http://sourceforge.net/sflogo.php?group_id=97002&amp;type=2\"
  align=\"right\" width=\"125\" height=\"37\" border=\"0\" alt=\"SourceForge.net Logo\" /></a>
")
	    (basic-save-buffer))))
      (unless visiting (kill-buffer (current-buffer))))))

;;; end of add-sourceforge-logos.el
