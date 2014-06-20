;;;; electric-space.el -- electric handling of inserting spaces -- particularly for use with voice input
;;; Time-stamp: <2007-10-12 23:11:12 jcgs>

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

(require 'cl)
(require 'speak-parens)

(defvar electric-space-no-space-regexp
  ;; "[({\"`] +"
  "[(`] +"
  "Regular expression indicating things after which we should not insert a space.
Useful for when Dragon puts one in when you don't really want it.")

(defvar electric-space-debug nil
  "*Whether electric-space should explain what it's doing.")

(defvar electric-space-hooks nil
  "*List of functions to call on attempting to insert a space.
One argument is passed, which is the number of spaces.")

;;;###autoload
(defun electric-space (n)
  "Electric handling of space.
Useful for when Dragon puts one in when you don't really want it."
  ;; invented around November 2003 by John Sturdy
  ;; (noted in case this provides prior art for anything later)
  (interactive "p")
  (when electric-space-debug (message "(electric-space %S)" n))
  ;; do this so we trigger abbrevs and auto-fill
  (self-insert-command n)

  ;; now the electric stuff -- but not if the user asked for a different number of spaces explicitly
  (if (= n 1)
      (cond
       ((save-excursion
	  (skip-syntax-backward " ")
	  (unless (bobp) (backward-char))
	  (looking-at electric-space-no-space-regexp))
	(when electric-space-debug (message "electric-space reckons there should be no space here"))
	(delete-horizontal-space))
       (nil
	;; I may yet think of a situation where I want to do this
	;; It might even be most of the time?
	;; We might want to do it except when the previous command was also a space?
	(when electric-space-debug (message "electric-space reckons there should be a single space here"))
	(just-one-space))
       (t
	(when electric-space-debug (message "electric-space reckons this is an ordinary space"))
	nil)))
  (run-hook-with-args 'electric-space-hooks n))

(global-set-key " " 'electric-space)

(defun electric-open-paren ()
  "Insert an open bracket. If at start of list at margin, open line to do so."
  (interactive)
  (when (and (looking-at "(")
	     (save-excursion
	       (let ((started (point)))
		 (beginning-of-line 1)
		 (skip-syntax-forward " ")
		 (= started (point)))))
    (open-line 1)
    (save-excursion
      (forward-line)
      (lisp-indent-line)))
  (insert "(")
  (current-depth (point) t))

(defun electric-close-paren ()
  "Insert an close bracket."
  (interactive)
  (insert ")")
  (blink-matching-open)
  (current-depth (point) t))

(defun electric-insert-parentheses (arg)
  "Like insert-parentheses, but if at margin, open line to do so."
  (interactive "P")
  (if arg (setq arg (prefix-numeric-value arg))
    (setq arg 0))
  (when (and (looking-at "(")
	     (save-excursion
	       (let ((started (point)))
		 (beginning-of-line 1)
		 (skip-syntax-forward " ")
		 (= started (point)))))
    (open-line 1)
    (save-excursion
      (forward-line)
      (lisp-indent-line)))
  (insert-parentheses arg))

(defun electric-insert-quotes (arg)
  (interactive "P")
  (if arg (setq arg (prefix-numeric-value arg))
    (setq arg 0))
  (if (> arg 1)
      (self-insert-command arg)
    (insert "\"\"")
    (backward-char 1)))

(mapcar (lambda (map)
	  (define-key map "(" 'electric-open-paren)
	  (define-key map ")" 'electric-close-paren)
	  (define-key map "\M-(" 'electric-insert-parentheses)
	  (define-key map "\M-\"" 'electric-insert-quotes))
	(list lisp-mode-map emacs-lisp-mode-map lisp-interaction-mode-map))

(provide 'electric-space)

;;; end of electric-space.el
