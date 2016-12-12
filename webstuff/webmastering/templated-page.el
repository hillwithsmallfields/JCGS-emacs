;;;; templated-page.html
;;; create a page based on a template

(provide 'templated-page)

(defvar templated-link-format  "<a href=\"\\([^:\"]+\\)\">\\([^<]+\\)</a>"
  "How to recognize a link")

(defun templated-page (page title)
  "Create a templated PAGE with TITLE."
  (interactive "FPage file: 
sPage title: ")
  (setq page (expand-file-name page))
  (if (file-exists-p page)
      (message "%S already exists -- not modifying" page)
    (save-window-excursion
      (copy-file (expand-file-name "template.html"
				   (file-name-directory page))
		 page)
      (find-file page)
      (mapcar (function (lambda (construct)
			  (goto-char (point-min))
			  (if (search-forward (format "<%s></%s>" construct construct) (point-max) t)
			      (replace-match (format "<%s>%s</%s>" construct title construct) t t))))
	      '("title" "h1"))
      (basic-save-buffer))))

(defun templated-pages-from-links ()
  "Create local pages referred to from this page, using templates."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward templated-link-format
			      (point-max)
			      t)
      (templated-page (match-string 1) (match-string 2)))))

(defun templated-page-from-links ()
  "Create a local page referred to from this link, using templates."
  (interactive)
  (save-excursion
    (let ((anchor-start (save-excursion (search-backward "<a href" (point-min) t)))
	  (anchor-end  (save-excursion (search-backward "</a>" (point-min) t))))
      (if (and (integerp anchor-start)
	       (or (not (integerp anchor-end))
		   (< anchor-end anchor-start)))
	  (progn
	    (goto-char anchor-start)
	    (if (looking-at templated-link-format)
		(templated-page (match-string 1) (match-string 2))))
	(error "Not in an anchor")))))
