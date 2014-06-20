;;;; other.el -- go to the other window or buffer
;;; Time-stamp: <2007-06-28 13:44:38 jcgs>

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

(defvar other-window-or-buffer-consecutive-count 0
  "How many consecutive commands have been other-window-or-buffer")

(defvar other-window-or-buffer-previous-used-buffer nil
  "The buffer from which we most recently started a series of other-window-or-buffer commands.")

(defvar other-window-or-buffer-previous-previous-used-buffer nil
  "The buffer from which we next-most recently started a series of other-window-or-buffer commands.")

(defun other-window-or-buffer ()
  (interactive)
  "Switch to the next window, or, if there is only one window, the next buffer."
  (setq other-window-or-buffer-consecutive-count
	(if (eq last-command 'other-window-or-buffer)
	    (1+ other-window-or-buffer-consecutive-count)
	  1))
  (if (= other-window-or-buffer-consecutive-count 1)
      (setq other-window-or-buffer-previous-previous-used-buffer
	    other-window-or-buffer-previous-used-buffer
	    other-window-or-buffer-previous-used-buffer (current-buffer)))
  (if (one-window-p)
      (cond
       ((<= other-window-or-buffer-consecutive-count 2)
	(switch-to-buffer other-window-or-buffer-previous-previous-used-buffer))
       ((<= other-window-or-buffer-consecutive-count 3)
	(switch-to-buffer (other-buffer)))
       (t
	(switch-to-buffer (nth other-window-or-buffer-consecutive-count
			       (buffer-list)))
	(message "Next one will be %s"
		 (buffer-name (nth (1+ other-window-or-buffer-consecutive-count)
				   (buffer-list))))))
    
    (if (<= other-window-or-buffer-consecutive-count 2)
	(other-window 1)
      (delete-other-windows))))

(defun other-window-backwards (n)
  "Select the -Nth window -- see other-window, this just negates the argument."
  (interactive "p")
  (other-window (- n)))

(provide 'other)

;;; end of other.el
