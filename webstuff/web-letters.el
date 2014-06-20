;;;; web-letters.el -- use HTML for preparing printed letters, using templates
;;; Time-stamp: <2004-11-01 11:48:03 john>

(defvar web-letters-template-directory (substitute-in-file-name "$COMMON/www/letters/templates")
  "*Where to find web letter templates.")

(defvar web-letters-directory (substitute-in-file-name "$COMMON/www/letters")
  "*Where to put web letters.")

(defvar web-letters-source-buffer nil
  "The buffer from which the user has most recently selected text to go into a letter.")

(defun web-letter (letter template)
  "Start a web LETTER, using a TEMPLATE."
  (interactive
   (let* ((letter (read-from-minibuffer "Letter name: "))
	  (template (completing-read (format "Template for %s: " letter)
				     (mapcar 'list (directory-files web-letters-template-directory
								    nil
								    ".*\\.html$"
								    t)))))
     (list (expand-file-name (concat letter ".html") web-letters-directory)
	   (expand-file-name template web-letters-template-directory))))
  (when (file-exists-p letter) (error "Letter file %s already exists" letter))
  (find-file letter)
  (erase-buffer)
  (insert-file-contents template)
  (goto-char (point-min))
  (let ((remembered nil))
    (while (re-search-forward "<!-- insert \\([^>]+\\) -->" (point-max) t)
      (let* ((start (match-beginning 0)) ; so we also scan the inserted text for insertion markers
	     (description (match-string 1))
	     (prev (assoc description remembered))
	     (text (read-from-minibuffer
		    (format "Text for insertion ``%s'' (none for select): " description)
		    (if prev (cdr prev) nil)
		    ))
	     )
	(when (string= text "")
	  (save-match-data
	    (save-window-excursion
	      (when web-letters-source-buffer
		(switch-to-buffer web-letters-source-buffer))
	      (message (substitute-command-keys "Select text and use \\[exit-recursive-edit]"))
	      (recursive-edit)
	      (setq web-letters-source-buffer (current-buffer))
	      (setq text (buffer-substring (region-beginning) (region-end))))))
	(pushnew (cons description text) remembered)
	(replace-match text t t)))))
  
;;; end of web-letters.el
