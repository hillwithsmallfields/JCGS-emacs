;;;; inline-html.el
;;; Time-stamp: <2003-11-10 16:14:02 john>

(provide 'inline-html)

(defun inline-html (master target)
  "Copy MASTER file into TARGET, inlining hyperlinks.
Only hyperlinks in containers marked class=\"inlinable\"
are processed thus. The original links are replaced by
local anchor links, and the inlined texts are inserted
at the end of the container."
  (interactive "fMaster file for inlining: 
FTarget file for inlining result: ")
  (find-file target)
  (erase-buffer)
  (let ((target-buffer (current-buffer)))
    (find-file master)
    (goto-char (point-min))
    (let ((old (point)))
      (while (re-search-forward "<\\([a-z0-9]+\\)[^>]+class=\"inlinable\"[^>]*>" (point-max) t)
	(append-to-buffer target-buffer old (point))
	(setq old (match-end 0))
	(let* ((tag (match-string 1))
	       (tag-start (point))
	       (tag-pattern (format 
			     ;; "\\(</?\\)%s "
			     "\\(</?\\)%s[^>]*>"
			     ;; "\\(</?\\)"
			     tag))
	       (tag-depth 1))
	  (while (and (> tag-depth 0)
		      (re-search-forward tag-pattern (point-max) t))
	    (if (string= (match-string 1) "</")
		(decf tag-depth)
	      (incf tag-depth)))
	  (inline-html-1 old (point) (current-buffer) target-buffer))
	(setq old (point)))
      (append-to-buffer target-buffer old (point-max)))))

(defun inline-add-strings-at (buffer place &rest strings)
  "To BUFFER at PLACE (string to search for, or t for end) add STRINGS"
  (save-window-excursion
    (set-buffer buffer)
    (if (eq place t)
	(goto-char (point-max))
      (progn
	(goto-char (point-min))
	(search-forward place)
	(goto-char (match-beginning 0))))
    (mapcar 'insert strings)))

(defun inline-add-html-body-at (buffer place file)
  "To BUFFER at PLACE (string to search for) insert the html body of FILE."
  (save-window-excursion
    (find-file file)
    (goto-char (point-min))
    (re-search-forward "<body[^>]*>")
    (let ((start (point)))
      (search-forward 
       (if nil 
	   "</body>"
	 "<hr>"))
      (let ((contents (buffer-substring start (match-beginning 0))))
	(set-buffer buffer)
	(goto-char (point-min))
	(search-forward place)
	(goto-char (match-beginning 0))
	(insert contents)))))

(defun inline-html-1 (from to source target)
  "From FROM to TO in SOURCE buffer, handle inlining into TARGET buffer."
  (set-buffer source)
  (goto-char from)
  (let* ((old from)
	 (unique (gensym))
	 (index-marker (format "<!-- index %s -->" unique))
	 (text-marker (format "<!-- text %s -->" unique)))
    (inline-add-strings-at target t 
			   index-marker "\n"
			   text-marker "\n")
    (while (re-search-forward "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>" to t)
      (let ((tag-start (match-beginning 0))
	    (tag-end (match-end 0))
	    (link (match-string 1))
	    (text (match-string 2))
	    (tag-code (symbol-name (gensym)))
	    )
	(message "link=%s text=%s" link text)
	(inline-add-strings-at target index-marker
			       (buffer-substring old tag-start)
			       (format "<a href=\"#%s\">%s</a>" tag-code text) 
			       )
	(inline-add-strings-at target text-marker (format "<a name=\"%s\"></a>" tag-code))
	(inline-add-html-body-at target text-marker
				 link)
	(goto-char (setq old tag-end))
	))
    (inline-add-strings-at target index-marker old to)))
