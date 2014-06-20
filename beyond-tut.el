;;; bits for helping me with writing beyond-tutorial
;;; Time-stamp: <2008-09-24 11:20:25 jcgs>

(defun fill-in-commands (&optional keymap)
  "Fill in the commands in the table, from the keystrokes.
Optional KEYMAP says where to look them up, instead of the globel map."
  (interactive)
  (save-excursion
    (while (re-search-forward "<td class=\"commandname\"><code>\\(</code></td>\\)" (point-max) t)
      (let ((fill-in-at (match-beginning 1)))
	(beginning-of-line 0)
	(if (looking-at "^ +<td class=\"keystrokes\"><code>\\(.+\\)</code></td>")
	    (let* ((raw (match-string-no-properties 1))
		   (km (read-kbd-macro raw))
		   (command (if keymap
				(lookup-key keymap km)
			      (global-key-binding km))))
	      (goto-char fill-in-at)
	      (insert (symbol-name command)))
	  (goto-char fill-in-at))))))

(defun fill-in-dired-commands ()
  "Fill in dired commands."
  (interactive)
  (fill-in-commands dired-mode-map))

(defun fill-in-help ()
  "Get the help strings."
  (interactive)
  (save-excursion
    (while (re-search-forward "<td class=\"explanation\"><p>\\(</p></td>\\)" (point-max) t)
      (let ((fill-in-at (match-beginning 1)))
	(beginning-of-line 0)
	(if (looking-at "^ +<td class=\"commandname\"><code>\\([-a-z]+\\)</code></td>")
	    (let* ((command (match-string-no-properties 1))
		   (docstring (documentation (intern command))))
	      (goto-char fill-in-at)
	      (insert (substring docstring 0 (string-match "\n" docstring))))
	  (goto-char fill-in-at))))))

;;; beyond-tut.el ends here
