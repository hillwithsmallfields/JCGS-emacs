;;; Time-stamp: <2021-11-14 18:31:02 jcgs>

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

(defun log-cycle-ride (wheels etime ttime mxs avs dst &optional comment)
  "Log a cycle ride."
  (interactive "sWheels: 
sElapsed Time: 
sTravel Time: 
sMaximum speed: 
sAverage speed: 
sDistance: 
sComment: ")
  (if (string-match "^1:[0-5][0-9]$" etime)
      (setq etime (concat etime ":00")))
  (find-file (substitute-in-file-name "$SYNCED/var/cycle-log"))
  (goto-char (point-max))
  (let ((date (decode-time)))
    (insert "(\"" (format "%04d-%02d-%02d" (nth 5 date) (nth 4 date) (nth 3 date)) "\" "
	    wheels " " dst " \"" etime "\" \"" ttime "\" " avs " " mxs)
    (when (and (stringp comment) (not (string= comment "")))
      (insert " \"" comment "\""))
    (insert ")\n"))
  (basic-save-buffer)
  (bury-buffer))

(provide 'cycle-log)

;;; end of cycle-log.el
