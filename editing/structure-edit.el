;;; -*-emacs-lisp-*- /harlqn/usr/users/jcgs/emacs/handy-lisp.el
;;; Time-stamp: <2021-10-29 20:19:45 jcgs>
;;; T i m e stamp <89/06/24 13:51:19 jcgs>

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A couple of commands for structure-style editing ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'structure-edit)

(defvar wander-yank-depth 0
  "How many levels of wander-yank we are in.")

(defun pick-up-sexp-at-point (count)
  "Copy COUNT sexps from point into the kill ring, and exit recursive edit.
Use with wander-yank for copying (non-destructive-cut-and-paste)."
  (interactive "p")
  (mark-sexp count)
  (copy-region-as-kill (point) (mark))
  (exit-recursive-edit))

(defun move-sexp-from-point (count)
  "Kill COUNT sexps starting at point, and exit recursive edit.
Use with wander-yank for moves (destructive-cut-and-paste)."
  (interactive "p")
  (kill-sexp count)
  (exit-recursive-edit))

(defun wander-yank ()
  "Let the user wander in a recursive edit inside save-excursion, then yank.
Presumably the user picks something up into the kill ring during
the recursive edit."
  (interactive)
  (let ((wander-yank-depth (1+ wander-yank-depth)))
    (save-excursion
      (save-window-excursion
	(recursive-edit))))
  (yank 1))

(defun wander-yank-dwim (count)
  "If not already in a wander-yank (which see), do wander-yank, else pick-up-sexp-at-point."
  (interactive "p")
  (if (> wander-yank-depth 0)
      (pick-up-sexp-at-point count)
    (wander-yank)))

(setq enable-recursive-minibuffers t)

(defun wander ()
  "Let the user wander around in a recursive edit inside a save-excursion."
  (interactive)
  (save-excursion
    (save-window-excursion
      (recursive-edit))))

(defun sexp-preceding-next-parenthesis ()
  "Move to the start of the sexp preceding the next opening parenthesis."
  (interactive)
  (down-list 1)
  (backward-up-list 1)
  (backward-sexp 1))

(defun insert-quotes (n)
  "Insert quotes.  With argument, surround N sexps with quotes."
  (interactive "p")
  (if (= n 1)
      (insert "\"\"")
    (insert "\"")
    (forward-sexp n)
    (insert "\"")))

(defun move-in-or-out-of-string ()
  "Move to before the most recent string quote."
  (interactive)
  (search-backward "\""))

;;; end of /harlqn/usr/users/jcgs/emacs/handy-lisp.el
