;;; Time-stamp: <2006-01-23 16:38:14 john>

;;; <title>Find files named by URLs, and construct and update indices</title>

;; (require 'url)
(require 'browse-url)
(provide 'find-page-file)

(setq browse-url-regexp "[^-./_a-zA-Z0-9]\\([a-z]+:\\)?\\(//[^/]+/\\)?[-./_a-zA-Z0-9]+[^-./_a-zA-Z0-9]")

(defun browse-url-at-point ()
  "Return the URL around or before point.
Then search backwards for the start of a URL.  If no URL found, return
the empty string."
  (save-excursion
    (if (or (looking-at browse-url-regexp) ; Already at start
	    (let ((eol (point-at-eol)))
	      ;; Search forwards for the next URL or end of line in case
	      ;; we're in the middle of one.
	      (and (re-search-forward browse-url-regexp eol 'lim)
		   (goto-char (match-beginning 0)))
	      ;; Now back to where we started or earlier.
	      (re-search-backward browse-url-regexp nil t)))
	(buffer-substring (1+ (match-beginning 0)) (1- (match-end 0)))
      "")))				; No match

;;;###autoload
(defun find-page-file (url)
  "Try to get the file referenced by URL into a buffer.
It will also try its argument as a filename.
Returns whether it succeeded in getting a page file."
  (interactive (browse-url-interactive-arg "Find file for URL: "))
  (cond
   ((file-exists-p url) (find-file url))
   ((url-buffer-visiting url) (switch-to-buffer (url-buffer-visiting url)))
   (t (let ((file (webmaster:file-of-url url)))
	(if file
	    (find-file file)
	  nil)))))

(defun page-already-in-buffer-p (page)
  "Return whether PAGE (url or file) is already in a buffer."
  (cond
   ((file-exists-p page) (find-buffer-visiting page))
   ((url-buffer-visiting page) t)
   (t (let* ((file (webmaster:file-of-url page)))
	(if file
	    (find-buffer-visiting file)
	  nil)))))

(defun find-page-file-already (page)
  "Find the file containing PAGE (url or filename),
returning a cons of whether it was found and whether it was already in a buffer."
  (cond
   ((file-exists-p page)
    (let ((already (find-buffer-visiting page)))
      (find-file page)
      (cons t already)))
   ((url-buffer-visiting page)
    (switch-to-buffer (url-buffer-visiting page))
    (cons t t))
   (t (let* ((file (webmaster:file-of-url page)))
	(if file
	    (let ((already (find-buffer-visiting file)))
	      (find-file file)
	      (cons t already))
	  (cons nil nil))))))

(defmacro with-page-file (page &rest forms)
  "With PAGE in the current buffer, do FORMS."
  `(save-window-excursion
     (let* ((already-in-buffer (page-already-in-buffer-p ,page))
; 	    (font-lock-mode (if already-in-buffer
; 				font-lock-mode
; 			      nil))
; 	    (buffer-read-only (or t (if already-in-buffer
; 				  buffer-read-only
; 				t)))
	    (found (find-page-file ,page))
	    )
       (when found
	 ;; (message "Found %s, already=%s, in buffer %s" page already-in-buffer (current-buffer))
	 (let ((result
		(save-excursion
		  (save-window-excursion
		    ,@forms))))
	   ;; (message "completing already processing, in buffer %s" (current-buffer))
	   (unless already-in-buffer
	     (kill-buffer nil))
	   result)))))

;;; end of find-page-file.el
