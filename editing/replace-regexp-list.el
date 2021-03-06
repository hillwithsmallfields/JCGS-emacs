;;; Time-stamp: <2019-04-27 14:47:23 jcgs>

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


(provide 'replace-regexp-list)

;;;###autoload
(defun replace-regexp-alist (edit-list &optional start end
				       fixedcase literal
				       verbose)
  "Apply EDIT-LIST to the current buffer, and return the count of alterations.
Each edit is a regexp and a replacement for that regexp.

The edits are applied in order, to the whole buffer; that is, the buffer is
edited throughout with the first one, then with the next one, and so on.

If a replacement is function, it is called to get the text to use.
If it is a list, it is iterated over.

Elements of the list may be strings (normal replacement rules
apply), or functions to call, or expressions to evaluate, or
numbers to use as capture group references.

If START and END are specified, replace only between them.


See documentation of `replace-match' for further optional arguments.

Optional argument FIXEDCASE is passed to `replace-match'.
Optional argument LITERAL is passed to `replace-match'.
Optional argument VERBOSE produces a report on the alterations made."
  (unless start (setq start (point-min)))
  (cond
   ((null end)
    (setq end (point-max-marker)))
   ((integerp end)
    (setq end (copy-marker end))))
  (let ((total-count 0)
	(messages nil)
	(message-format (format "%%%ds --> %%%ds: %%d"
				(apply 'max
				       (mapcar 'length
					       (mapcar 'car edit-list)))
				(apply 'max
				       (mapcar 'length
					       (mapcar 'cdr edit-list))))))
    (save-excursion
      (dolist (edit edit-list)
	(goto-char start)
	(let* ((regexp (car edit))
	       (replacement (cdr edit))
	       (this-count 0)
	       (end-marker (make-marker)))
	  (while (re-search-forward regexp end t)
	    (let ((matched (match-string-no-properties 0)))
	      (set-marker end-marker (match-end 0))
	      ;; (message "Replacing %S with %S" matched replacement)
	      (cond
	       ((stringp replacement)
		(replace-match replacement fixedcase literal))
	       ((functionp replacement)
		(replace-match (save-match-data
                                 (funcall replacement matched))
                               fixedcase literal))
	       ((consp replacement)
		(replace-match
		 (mapconcat (lambda (replace-element)
			      ;; (message "Replacement element is %S" replace-element)
			      (cond
			       ((stringp replace-element)
				replace-element)
			       ((functionp replace-element)
				(save-match-data
                                  (funcall replace-element matched)))
			       ((integerp replace-element)
				(match-string-no-properties replace-element))
			       ((consp replace-element)
				(save-match-data
                                  (eval replace-element)))))
			    replacement
			    "")
		 fixedcase literal))))
	    (goto-char end-marker)
	    (setq this-count (1+ this-count)))
	  (set-marker end-marker nil)
	  (setq total-count (+ total-count this-count))
	  (when verbose
	    (push (format message-format regexp replacement this-count) messages)))))
    (when verbose
      (with-output-to-temp-buffer "*Changes made*"
	(princ (mapconcat 'identity messages "\n"))))
    total-count))

;;;###autoload
(defun replace-regexp-alist-repeatedly (edit-list &optional fixedcase literal)
  "Like apply-replace-regexp-alist but keeps doing it until no further changes occur.
It is not guaranteed that this will ever terminate."
  (while (not (zerop (replace-regexp-alist edit-list fixedcase literal)))))


;;; I'm not sure who wrote this; I don't think I did. JCGS
;;;###autoload
(defun replace-with-eval-result (regexp form)
  "Replace occurrences of REGEXP with the result of (eval FORM).
While FORM is evaluated, the variable \\& is bound to the whole
matched text, and the variables \\1, \\2 etc bound to the
corresponding one of the strings matched by bracketed expressions in
the regexp.  The whole matched text is deleted from the buffer before
the form is evaluated.  Thus, it is quite like `replace-regexp', with
binding names being the same as the corresponding substitution markers
in the replacement string of `replace-regexp'."
  (interactive "sReplace regexp:
xReplace regexp %s with result of form: ")
  (while (re-search-forward regexp (point-max) t)
    (let ((m-d (match-data))
	  (matched-strings nil))
      (while m-d
	(setq matched-strings (cons (buffer-substring (car m-d) (cadr m-d))
				    matched-strings)
	      m-d (cddr m-d)))
      (progv
	  '(\\& \\1 \\2 \\3 \\4 \\5 \\6 \\7 \\8 \\9)
	  (nreverse matched-strings)
	(delete-region (match-beginning 0)
		       (match-end 0))
	(let ((result (save-excursion
			(save-window-excursion
			  (eval form)))))
	  (if (not (stringp result))
	      (setq result (prin1-to-string result 'no-escape)))
	  (insert result))))))

;;; end of replace-regexp-list.el
