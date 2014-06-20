;;; -*-emacs-lisp-*- /harlqn/usr/users/jcgs/emacs/handy-lisp.el
;;; Time-stamp: <2005-01-18 19:13:55 jcgs>
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

(defvar in-wander-yank nil
  "Whether we are currently in a wander-yank.")

;;;###autoload
(defun pick-up-sexp-at-point (count)
  "Copy COUNT sexps from point into the kill ring, and exit recursive edit.
Use with wander-yank for copying (non-destructive-cut-and-paste)."
  (interactive "p")
  (mark-sexp count)
  (copy-region-as-kill (point) (mark))
  (exit-recursive-edit))

;;;###autoload
(defun move-sexp-from-point (count)
  "Kill COUNT sexps starting at point, and exit recursive edit.
Use with wander-yank for moves (destructive-cut-and-paste)."
  (interactive "p")
  (kill-sexp count)
  (exit-recursive-edit))

;;;###autoload
(defun wander-yank ()
  "Let the user wander around in a recursive edit inside a
save-excursion, then do a yank - presumably the user picks something
up into the kill ring during the recursive edit."
  (interactive)
  (let ((in-wander-yank t))
    (save-excursion
      (save-window-excursion
	(recursive-edit))))
  (yank 1))

;;;###autoload
(defun wander-yank-dwim (count)
  "If not already in a wander-yank (which see), do wander-yank, else pick-up-sexp-at-point."
  (interactive "p")
  (if in-wander-yank
      (pick-up-sexp-at-point count)
    (wander-yank)))

(setq enable-recursive-minibuffers t)

;;; end of /harlqn/usr/users/jcgs/emacs/handy-lisp.el
