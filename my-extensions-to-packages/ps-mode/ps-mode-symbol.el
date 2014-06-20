;;;; ps-mode-symbol.el -- symbol evaluation of PostScript fragments
;;; Time-stamp: <2007-06-25 18:05:49 jcgs>

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

(defvar ps-mode-code-and-comment-regexp "^\\(.*\\)%\\(.*\\)"
  "Regexp to split a PostScript line into code and comment.")

(defun ps-mode-update-stack-comment (where)
  "Update the stack comment for the line containing WHERE (point, interactively)."
  (interactive "d")
  (save-excursion
    (end-of-line)
    (unless (search-backward "%" (line-beginning-position) t)
      (insert "% "))
    (if (looking-at ps-mode-code-and-comment-regexp)
	(let* ((ps (match-string-no-properties 1))
	       (stack (match-string-no-properties 2))
	       (previous-line-stack
		(save-match-data
		  (beginning-of-line 0)
		  (if (looking-at ps-mode-code-and-comment-regexp)
		      (match-string-no-properties 2)
		    nil))))
	  (when previous-line-stack
	    (replace-match (ps-mode-symbolic-eval ps previous-line-stack)
			   t t nil 2))))))

(defun ps-split-string (string)
  "Split STRING, as PostScript syntax."
  ;; todo: handle (bracketted strings)
  (split-string string))

(defun ps-mode-symbolic-eval (postscript stack)
  "Return the result of running POSTSCRIPT on STACK.
Each of the arguments may be either a string or a list.
The result is returned in whichever form STACK was given.
If a list, the car of the list is the top of the stack;
if a string, the last word in the string is the top."
  (let ((was-string (stringp stack)))
    (when was-string
      (setq stack (nreverse (ps-split-string stack))))
    (when (stringp postscript)
      (setq postscript (ps-split-string postscript)))
    (catch 'unknown-operator
      (dolist (operator postscript)
	(let ((op-def (intern-soft operator ps-mode-operators-obarray)))
	  (if op-def
	      (setq stack (apply (symbol-value op-def) stack))
	    (throw 'unknown-operator operator)))))
    (if was-string
	(mapconcat 'identity (nreverse stack) " ")
      stack)))

(defvar ps-mode-operators-obarray (make-vector 1511 nil)
  "An obarray defining the effects of PostScript operators on the stack.
Its symbols should have values that are Lisp functions taking the
stack contents as the args they need and a rest arg, and returning a
list of the stack contents afterwards.")

(defun ps-safe-term (term)
  "Make sure that TERM is safe to append stuff to, as algebraic notation."
  (if (string-match "[-+!|]" term)
      (format "(%s)" term)
    term))

(defun ps-mode-define-symbolic-op (opname args body)
  "Register a PostScript symbol operator definition."
  (let ((symbol (intern (symbol-name opname) ps-mode-operators-obarray)))
    (set symbol `(lambda ,args ,@body))))

(defmacro ps-def-op (name args &rest body)
  "Define the stack effects of a PostScript operator."
  `(ps-mode-define-symbolic-op ',name ',args ',body))

(ps-def-op dup (a &rest stack) (cons a (cons a stack)))
(ps-def-op pop (a &rest stack) stack)
(ps-def-op exch (a b &rest stack) (cons b (cons a stack)))
(ps-def-op def (a b &rest stack) stack)
(ps-def-op translate (a b &rest stack) stack)
(ps-def-op scale (a b &rest stack) stack)
(ps-def-op rotate (a b &rest stack) stack)
(ps-def-op moveto (a b &rest stack) stack)
(ps-def-op lineto (a b &rest stack) stack)
(ps-def-op currentpoint (&rest stack) (cons "y" (cons "x" stack)))
(ps-def-op stroke (&rest stack) stack)
(ps-def-op fill (&rest stack) stack)
(ps-def-op show (a &rest stack) stack)
(ps-def-op add (a b &rest stack) (cons (format "%s+%s" a b) stack))
(ps-def-op sub (a b &rest stack) (cons (format "%s-%s" a b) stack))
(ps-def-op neg (a &rest stack) (cons (format "-%s" a) stack))
(ps-def-op mul (a b &rest stack) (cons (format "%s*%s" (ps-safe-term a) (ps-safe-term b)) stack))
(ps-def-op div (a b &rest stack) (cons (format "%s/%s" (ps-safe-term a) (ps-safe-term b)) stack))
(ps-def-op mod (a b &rest stack) (cons (format "%s%%%s" (ps-safe-term a) (ps-safe-term b)) stack))
(ps-def-op or (a b &rest stack) (cons (format "%s|%s" a b) stack))
(ps-def-op and (a b &rest stack) (cons (format "%s&%s" (ps-safe-term a) (ps-safe-term b)) stack))
(ps-def-op length (s &rest stack) (cons (format "len(%s)" s) stack))
(ps-def-op not (a &rest stack) (cons (format "!%s" a) stack))
(ps-def-op string (&rest stack) (cons "newstring" stack))

(provide 'ps-mode-symbol)

;;; end of ps-mode-symbol.el
