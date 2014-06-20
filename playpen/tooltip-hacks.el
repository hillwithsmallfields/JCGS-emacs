;;;; tooltip-hacks.el -- experimenting with tooltips
;;; Time-stamp: <2007-08-19 23:53:00 jcgs>

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

(defvar tooltip-hack-params 
  '((name . "tooltip")
    (internal-border-width . 4)
    (border-color . "red")
    ;; (foreground-color . "blue")
    (background-color . "yellow")
    (left . 0)
    (top . 0)
    (border-width . 2)))

(defun tooltip-hack (x y &optional str)
  (interactive "nX: 
nY: ")
  (rplacd (assoc 'left tooltip-hack-params) x)
  (rplacd (assoc 'top tooltip-hack-params) y)
  ;; Yes, I know it says not to call it directly.  But that's nothing
  ;; compared with what I'm thinking of doing.
  (x-show-tip (or str (concat "Hi " (propertize "there" 'face (cons 'foreground-color "red")))) nil
	      tooltip-hack-params
	      360))

(defun tooltip-circle (r)
  (interactive "nR: ")
  (let ((centre-x (/ (frame-pixel-width) 2))
	(centre-y (/ (frame-pixel-height) 2)))
    (dotimes (i 359)
      (sit-for .1) 
      (let ((theta (degrees-to-radians i)))
	(tooltip-hack (round (+ centre-x (* r (sin theta))))
		      (round (+ centre-x (* r (cos theta))))
		      (int-to-string i))))))

;;; end of tooltip-hacks.el
