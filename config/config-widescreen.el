;;; config-widescreen.el --- functions for widescreen use of emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for making better use of widescreens easier

;;; Code:

(defun widescreen-index-for-position (i n)
  "For a window index I out of N pick which window to put here.
The most recent ones go in the middle."
  (let* ((middle (/ n 2))
         (j (- i (if (evenp n) -1 0))))
    (if (<= j middle)
        (* 2 (- middle j))
      (- (* 2 (- j middle)) 1))))

(defun widescreen-split-across (n)
  "Split the screen into N windows side-by-side.

Recent buffers are placed in the windows, with the most recent
nearest the centre of the screen.

The window containing the buffer from which the command was run
is selected."
  (interactive "NNumber of windows: ")
  (delete-other-windows)
  (let ((columns-per-window (/ (frame-width) n))
        (buffers-in-order (delete-if 'minibufferp (buffer-list))))
    (dotimes (i (1- n))
      (split-window-horizontally columns-per-window)
      (display-buffer-same-window
       (nth (widescreen-index-for-position i n) buffers-in-order) nil)
      (other-window 1))
    (display-buffer-same-window
     (nth (widescreen-index-for-position n n) buffers-in-order) nil))
  (other-window (+ (/ n 2) (if (evenp n) 0 1))))

(provide 'config-widescreen)
;;; config-widescreen.el ends here
