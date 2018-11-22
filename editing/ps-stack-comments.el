;;;; ps-stack-comments.el -- update stack comments in a PostScript file
;;; Time-stamp: <2018-11-15 19:23:48 jcgs>

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

(defvar ps-stack-ops
  '((pop . (lambda (a &rest z)
	     z))
    (dup . (lambda (a &rest z)
	     (cons a (cons a z))))
    (exch . (lambda (a b &rest z)
	      (cons b (cons a z))))
    (roll . (lambda (affected-string movement-string &rest z)
	      (let* ((affected (string-to-number affected-string))
		     (movement (string-to-number movement-string))
		     (unaffected (nthcdr affected-string z)))
		;; todo: write this
		)))
    (index . (lambda (i &rest z)
	       (if (string-match "^[0-9]+$" i)
		   (cons (nth (string-to-number i) z)
			 z)
		 (cons "?" z))))
    (copy . (lambda (amount &rest z)
	      ;; todo: write this
	      ))
    (moveto . (lambda (a b &rest z) z))
    (lineto . (lambda (a b &rest z) z))
    (curveto . (lambda (a b c d e f &rest z) z))
    (translate . (lambda (a b &rest z) z))
    (rotate . (lambda (a &rest z) z))
    (def . (lambda (a b &rest z) z))
    (scale . (lambda (a b &rest z) z))
    (show . (lambda (a &rest z) z))
    (findfont . (lambda (a &rest z) (cons a z)))
    (scalefont . (lambda (a b &rest z) (cons (format "%s*%s" a b) z)))
    (setfont . (lambda (a &rest z) z))
    (setlinewidth . (lambda (a &rest z) z))
    (setlinecap . (lambda (a &rest z) z))
    (setgray . (lambda (a &rest z) z))
    (gsave . (lambda (&rest z) z))
    (grestore . (lambda (&rest z) z))
    (newpath . (lambda (&rest z) z))
    (stroke . (lambda (&rest z) z))
    (bind . (lambda (&rest z) z))
    (showpage . (lambda (&rest z) z))
    (fill . (lambda (&rest z) z))
    (currentpoint . (lambda (&rest z) (cons "x" (cons "y" z))))
    (mul . (lambda (a b &rest z) (cons (format "%s*%s" a b) z)))
    (div . (lambda (a b &rest z) (cons (format "%s/%s" a b) z)))
    (add . (lambda (a b &rest z) (cons (format "%s+%s" a b) z)))
    (sub . (lambda (a b &rest z) (cons (format "%s-%s" a b) z)))
    )
  "The PostScript stack operators.")

(defun ps-update-stack-comments ()
  "Update the stack comments on the current line."
  (interactive)
  (let ((previous-comment
	 (save-excursion
	   (beginning-of-line 0)
	   (if (looking-at "^\\s-*\\(.*\\)\\(?:%\\s-*\\(.+\\)\\)$")
	       (match-string-no-properties 2)
	     nil))))
    (if previous-comment
	(save-excursion
	  (beginning-of-line 1)
	  (when (looking-at "^\\s-*\\(.*\\)\\(?:%\\s-*\\(.+\\)\\)$")
	    (let* ((code (match-string-no-properties 1))
		   (steps (split-string code))
		   (stack (nreverse (split-string previous-comment)))
		   (comment-start (match-beginning 2))
		   (comment-end (match-end 2))
		   (comment (match-string-no-properties 2)))
	      (while steps
		(cond
		 ((string-match "^[-0-9]+$" (car steps))
		  (setq stack (cons (car steps) stack)))
		 ((string-match "^[a-z]+$" (car steps))
		  (let ((op (cdr (assoc (intern (car steps))
					ps-stack-ops))))
		    (if op
			(setq stack (apply op stack)))))
		 (t (setq stack nil)))
		(setq steps (cdr steps)))
	      (when stack
		(when (and comment-start comment-end)
		  (delete-region comment-start comment-end))
		(if comment-start
		    (goto-char comment-start)
		  (end-of-line 1)
		  (comment-indent))
		(insert (mapconcat 'identity (nreverse stack) " ")))
	      (message "previous comment \"%s\"; code \"%s\"" previous-comment code))))
      (error "No previous comment to base this one on"))))

(provide 'ps-stack-comments)

;;; end of ps-stack-comments.el
