;;;; split-window-multi.el -- multi-way split of window
;;; Time-stamp: <2011-01-28 14:49:05 johnstu>

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

(provide 'split-window-multi)

;;;###autoload
(defun split-window-multi (&optional n horizontally each-window-function)
  "Split this window into N windows of equal width.
With non-nil second arg, split HORIZONTALLY.
Third arg is EACH-WINDOW-FUNCTION to call in each window.
N being negative also implies horizontal splitting.
Interactively, prefix arg gives number of windows."
  (interactive "p")
  (when (or (not (numberp n))
	    (= n 1))
    (setq n 3))
  (when (< n 0)
    (setq n (- n)
	  horizontally t))
  (let* ((edges (window-edges))
	 (total-used-width (- (nth (if horizontally 2 3) edges)
			      (nth (if horizontally 0 1) edges)))
	 (one-used-width (/ total-used-width n))
	 (i 1)
	 (window-to-split (selected-window)))
    (while (< i n)
      (setq window-to-split
	    (split-window window-to-split one-used-width horizontally))
      (select-window window-to-split)
      (switch-to-buffer (other-buffer (window-buffer window-to-split)))
      (when each-window-function (funcall each-window-function))
      (setq i (1+ i)))
    (other-window 1)))

;;;###autoload
(defun split-to-80-columns (&optional each-window-function)
  "Split the window into windows of 80 columns.
If optional EACH-WINDOW-FUNCTION is given, call it on each window."
  (interactive)
  (split-window-multi
   (let ((edges (window-edges)))
     (/ (- (nth 2 edges)
	   (nth 0 edges))
	80))
   t
   each-window-function))

;;;###autoload
(defun split-to-24-lines (&optional each-window-function)
  "Split the window into windows of 24 lines.
If optional EACH-WINDOW-FUNCTION is given, call it on each window."
  (interactive)
  (split-window-multi
   (let ((edges (window-edges)))
     (/ (- (nth 3 edges)
	   (nth 1 edges))
	24))
   nil
   each-window-function))

;;;###autoload
(defun split-window-grid (horizontally vertically)
  "Split the window into HORIZONTALLY windows across and VERTICALLY windows down.
Each window is given a different buffer (until we run out of buffers)."
  (interactive "nNumber of windows across: 
nNumber of windows down: ")
  (split-window-multi
   horizontally t
   (function
    (lambda ()
      (split-window-multi vertically nil nil)))))
      
;;;###autoload
(defun split-window-squarishly (n)
  "Split the window into at least N windows, with about the same number across and down.
Each window is given a different buffer (until we run out of buffers)."
  (interactive "NNumber of windows: ")
  (let* ((fside (sqrt n))
	 (iside (floor fside)))
    (split-window-grid
     iside
     (if (= (* iside iside) n) iside (1+ iside)))))

(defun nth-next-window (n)
  "Return the Nth next window."
  (and (or (null n) (<= n 0))
       (setq n 1))
  (let ((w (selected-window)))
    (while (< n 0)
      (setq w (next-window w)
	    n (1- n)))
    w))

;;;###autoload
(defun transpose-windows (arg)
  "Exchange the buffer in the selected window with that in another window.
ARG says how many windows further along to swap with."
  (interactive "p")
  (let* ((this-window (selected-window))
	 (this-buffer (window-buffer this-window))
	 (that-window (nth-next-window arg))
	 (that-buffer (window-buffer that-window)))
    (select-window that-window)
    (switch-to-buffer this-buffer)
    (select-window this-window)
    (switch-to-buffer that-buffer)))

;;; end of split-window-multi.el
