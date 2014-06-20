;;; turing-machine.el --- interpreter for Turing machine code

;; Copyright (C) 2008  John Sturdy

;; Author: John Sturdy <john.sturdy@ul.ie>
;; Keywords: emulations

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This parses and executes Turing Machine instructions, in the form
;; laid out in Turing's original paper, "On Computable Numbers, with
;; an application to the Entscheidungsproblem" (Proceedings of the
;; London Mathematical Society, 1936-7

;;; Code:

(defun turing-parse-buffer (buffer)
  "Parse a BUFFER of Turing Machine code.
The buffer should be in four columns, delimited by one or more tabs.

The first column is the state label, and is optional -- if the
line begins with a tab, it uses the same state as the previous
line.

The second column is the symbol recognized, or the text \"Any\" or \"None\".

The third column is the actions, as a comma-separated list.

The fourth column is the state to move to.

The result is an alist of states to alists of symbols to lists
starting with the next-state and continuing with the actions."
  (set-buffer buffer)
  (save-excursion
    (goto-char (point-max))
    (let ((last-label nil)
	  (states nil))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^\t]+\\)?\t+\\([^\t]+\\)\t++\\([^\t]+\\)\t++\\([^\t\n]+\\)$" (point-max) t)
	(let ((label (if (match-beginning 1)
			 (intern (match-string-no-properties 1))
		       last-label))
	      (symbol (match-string-no-properties 2))
	      (action-starts (match-beginning 3))
	      (action-ends (match-end 3))
	      (actions nil)
	      (next-state (match-string-no-properties 4)))
	  (setq symbol
		(cond
		 ((string-match "^any$" symbol) '*)
		 ((string-match "^none$" symbol) '_)
		 (t symbol)))
	  (unless (and (= action-ends (1+ action-starts))
		       (= (char-after action-starts) ?-))
	    (goto-char action-starts)
	    (while (re-search-forward "\\(.+\\),? ?" action-ends t)
	      (push (match-string-no-properties 1) actions)))
	  (let ((s-a-n (cons symbol (cons next-state (nreverse actions))))
		(already (assoc label states)))
	    (if already
		(rplacd already (cons s-a-n (cdr already)))
	      (setq states (cons (cons label (list s-a-n))
				 states))))
	  (setq last-label label)
	  (end-of-line 1)))
      (nreverse states))))

(defun turing-show-program (program)
  "Display PROGRAM."
  (with-output-to-temp-buffer "*Turing machine program*"
    (dolist (state program)
      (let ((label (car state))
	    (symbols (cdr state)))
      (princ label) (princ ":")
      (dolist (symbol symbols)
	(princ "\t")
	(princ (car symbol))
	(princ "\t-->\t")
	(princ (cadr symbol))
	(princ "\t")
	(princ (cddr symbol))
	(princ "\n"))
      (princ "\n")))))

(defun turing-test-parse ()
  "Test the parser."
  (interactive)
  (let ((program (turing-parse-buffer (current-buffer))))
    (message "Program is %S" program)
    (turing-show-program program)))

(provide 'turing-machine)
;;; turing-machine.el ends here
