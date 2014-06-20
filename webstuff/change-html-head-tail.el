;;;; <title>Replace heads and tails of HTML files</title>
;;; Time-stamp: <2007-06-10 22:18:14 jcgs>

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

(provide 'change-html-head-tail)
(require 'cl)
(require 'webmaster-macros)
(require 'page-structure)
;;(require 'page-status)

(defvar html-repair-pages t)

(defvar html-re-tail-hooks nil)

;;;###autoload
(defun html-re-tail-page (page &optional tail)
  "Change the tail of PAGE to be TAIL -- a file containing the new tail.
If TAIL is not given, use .tail in the same directory."
  (interactive "fRe-tail page file: 
fGet tail for %s from: ")
  (unless (and (not (interactive-p))
	       (string= (file-name-nondirectory page) "index.html"))
    (find-file page)
    (message "Updating %s with new tail from %s" page tail)
    (html-with-undisturbed-timestamps
     (let ((half-buffer (/ (- (point-max) (point-min)) 2)))
       (save-excursion
	 ;; todo: get this functionality working again
	 ;; (html-jcgs-move-status-markers-to-safety)
	 (if (or (null tail)
		 (string= tail "")
		 (string= tail page))
	     (setq tail
		   (if (file-exists-p ".tail")
		       ".tail"
		     (if (file-exists-p "../.tail")
			 "../.tail"
		       (if (file-exists-p "../../.tail")
			   "../../.tail"
			 "../../../.tail")))))
	 (when (catch 'found
		 (dolist (start-string (list html-tail-section-start
					     "<hr>"
					     "<!--#exec cgi=\"/cgi-bin/footer\" -->"
					     "</body>"))
		   (goto-char (point-min))
		   (when (search-forward start-string (point-max) t)
		     (message "Matched on %S" start-string)
		     (throw 'found t)))
		 (message "Warning: no tail markers found in %S" (buffer-file-name))
		 nil)
	   (goto-char (match-beginning 0))
	   (let* ((start (point))
		  (end (or (progn
			     (goto-char (point-max))
			     (if (search-backward html-tail-section-end (point-max) t)
				 (match-end 0)
			       nil))
			   (point-max))))
	     (when nil
	       (write-region start end (concat page ".old-tail") t))
	     (message "old tail was %S" (buffer-substring-no-properties start end))
	     (delete-region start end)
	     (goto-char start)
	     (insert html-tail-section-start "\n")
	     (let* ((end-marker (point-marker))
		    (insertion (insert-file-contents tail nil)))
	       (goto-char (+ end-marker (cadr insertion)))
	       (insert html-tail-section-end "\n")
	       (set-marker end-marker nil))
	     ;; (message "In recursive-edit before page repair") (save-excursion (recursive-edit))
	     (when html-repair-pages
	       (goto-char (point-max))
	       (unless (search-backward "</body>" (point-min) t)
		 (delete-blank-lines)
		 (insert "\n</body>\n"))
	       (goto-char (point-max))
	       (unless (search-backward "</html>" (point-min) t)
		 (delete-blank-lines)
		 (insert "\n</html>\n")))
	     ;; (message "In recursive-edit before tail hooks") (save-excursion (recursive-edit))
	     (run-hooks 'html-re-tail-hooks)
	     ;; (message "In recursive-edit at end") (save-excursion (recursive-edit))
	     )))))))

;;;###autoload
(defun html-re-tail-directory (directory)
  "Re-tail html files in directory, using html-re-tail-page, which see."
  (interactive "DDirectory to re-tail pages in: ")
  (mapcar 'html-re-tail-page (directory-files directory t "\.s?html$" t)))

;;;###autoload
(defun html-re-head-page (page &optional head)
  "Update a marked area within the head of PAGE using file HEAD,
which, if not given, defaults to .head in the same directory."
  (interactive "fRe-head page file: 
fGet head for %s from: ")
  (find-file page)
  (if (or (null head)
	  (string= head "")
	  (string= head page))
      (setq head ".head"))
  (message "Updating %s with new head from %s" page head)
  (html-with-undisturbed-timestamps
   (save-excursion
     (goto-char (point-min))
     ;; first remove the old controlled head section if present, but leaving the marker comments around where it was
     (if (not
	  ;; this expr returns whether or not it deleted an old controlled head section
	  (if (search-forward html-head-section-start (point-max) t)
	      (let ((start (point)))
		(goto-char (point-max))
		(if (search-backward html-head-section-end start t)
		    (progn
		      (delete-region start (point))
		      t)
		  nil))
	    nil))
	 ;; if no old header was found, try a rule of thumb for where the marker comments should go, and put them there
	 (progn
	   (goto-char (point-min))
	   (if (or (search-forward "</head>" (point-max) t)
		   (progn
		     (goto-char (point-min))
		     (search-forward "<!--#exec cgi=\"/cgi-bin/header\" -->" (point-max) t)
		     ))
	       (progn
		 (goto-char (match-beginning 0))
		 (insert html-head-section-start "\n" html-head-section-end "\n")))))
     (goto-char (point-min))
     (if (search-forward html-head-section-end (point-max) t)
	 (progn
	   (goto-char (match-beginning 0))
	   (insert-file-contents
	    (if (and (not (null head))
		     (not (string= head page)))
		head
	      ".head")))))))
	      
;;;###autoload
(defun html-re-head-directory (directory)
  "Re-head html files in directory, using html-re-head-page, which see."
  (interactive "DDirectory to re-head pages in: ")
  (mapcar 'html-re-head-page (directory-files directory t "\.html$" t))) 

;;;###autoload
(defun html-restyle-body-tag-to-string (page string)
  "Set the body tag for PAGE to STRING."
  (interactive "fRe-style body tag for: 
Get body tag to : ")
  (find-file page)
  (html-with-undisturbed-timestamps
   (save-excursion
     (goto-char (point-min))
     (if (re-search-forward "<body[^>]+>" (point-max) t)
	 (replace-match string)
       (insert string)))))

;;;###autoload
(defun html-restyle-body-tag-from-file (page params-file)
  "Set the body tag for PAGE."
  (interactive "fRe-style body tag for: 
fGet body tag from file: ")
  (let ((params-data (save-window-excursion
		       (find-file params-file)
		       (buffer-string))))
    (html-restyle-body-tag-to-string page params-data)))

;;;###autoload
(defun html-restyle-body-directory (directory)
  "Restyle all body tags in DIRECTORY according to .body"
  (let ((params-data (save-window-excursion
		       (find-file (expand-file-name ".body" directory))
		       (buffer-string))))
    (mapcar '(lambda (page)
	       (html-restyle-body-tag-to-string page params-data)))))

;;; end of change-html-head-tail.el
