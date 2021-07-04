(defun sort-body-lines ()
  "Sort all but the first line of the buffer."
  (sort-lines nil
	      (save-excursion
		      (goto-char (point-min))
		      (beginning-of-line 2)
		      (point))
	      (point-max))))

(define-derived-mode sorted-csv-mode csv-mode "Sorted CSV"
  "CSV mode which works well with with ISO dates in the first column.
When you save the file, all but the first line are sorted."
  (add-hook 'before-save-hook 'sort-body-lines)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))
