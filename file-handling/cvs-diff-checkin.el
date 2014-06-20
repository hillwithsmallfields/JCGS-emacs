;;;; cvs-diff-checkin.el -- Parse a buffer of CVS diffs and run checkins
;;; Time-stamp: <2006-07-31 18:00:44 john>

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

(provide 'cvs-diff-checkin)

(defun cvs-diff-checkin ()
  "Run cvs checkins from a cvs diff."
  (interactive)
  (while (re-search-forward "^Index: \\(.+\\)$" (point-max) t)
    (let* ((filename (match-string-no-properties 1))
	   (diff (buffer-substring-no-properties
		  (save-excursion
		    (beginning-of-line 5)
		    (point))
		  (save-excursion
		    (if (re-search-forward "^Index:")
			(match-beginning 0)
		      (point-max))))))
      (recenter 0)
      (save-excursion
	(save-window-excursion
	  (find-file filename)
	  (with-output-to-temp-buffer "*Diff*"
	    (princ diff))
	  (when (yes-or-no-p (format "Checkin %s? " filename))
	    ;;(vc-toggle-read-only)
	    (vc-next-action t)
	    ))))))

;;; end of cvs-diff-checkin.el
