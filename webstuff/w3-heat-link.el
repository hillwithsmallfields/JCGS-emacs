;;; Time-stamp: <2007-03-19 09:51:46 jcgs>

;;; <title>Things for grabbing links quickly in passing, and classifying them</title>

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

(provide 'w3-heat-link)

;;;;;;;;;;;;;;;;;;
; Grab new links ;
;;;;;;;;;;;;;;;;;;

(defvar w3-oven-file
  "~/LocalWWW/hotlinks.html"
  "Where to put new hot links.")

(defvar w3-hot-spot "<li> <a name=\"insertion\">New links</a> are added here"
  "Marker for new links.")

(defvar mail-reading-modes '(vm-mode rmail-mode gnus-article-mode)
  "Known mail reading modes.")

;;;###autoload
(defun w3-heat-link (start end visible &optional link-file link-fragment)
  "Log the text from START to END as an anchor, displayed as VISIBLE
in your unsorted hot links file which is named by w3-oven-file.

If you appear to be in a mail message at the time, the subject is
taken as the default visible text.

Optional fourth argument gives the link file into which to put the link,
and fifth argument the fragment of that link file.

If START is nil, take VISIBLE as being a complete pre-formatted anchor."

  (interactive
   (list (region-beginning) (region-end)
	 (read-from-minibuffer
	  "Text for anchor: "
	  (if (memq major-mode mail-reading-modes)
	      (save-excursion
		(save-match-data
		  (goto-char (point-min))
		  (if (re-search-forward "^Subject: \\(.+\\)$"
					 (point-max) t)
		      (buffer-substring (match-beginning 1)
					(match-end 1))
		    nil)))
	    nil))))

  (let ((href (if start
		  (concat "<li> <a href=\""
			  (buffer-substring start end)
			  "\">"
			  visible
			  "</a>\n")
		visible))
	(current-hot-spot w3-hot-spot))

    (save-window-excursion
      (find-file (if link-file link-file w3-oven-file))
      (save-excursion
	(goto-char (point-min))

	;; If we are sorting into different fragments of the file,
	;; find the appropriate fragment, and use the next end of list
	;; as the insertion      point.

	(if link-fragment
	    (if (search-forward (format "<a name=\"%s\">" link-fragment)
				(point-max)
				t)
		(setq current-hot-spot "</ul>")))

	;; If there is no insertion point, make one.
	(if (not (search-forward current-hot-spot (point-max) t))
	    (progn
	      (if link-fragment
		  (progn
		    ;; insert a fragment insertion point
		    (insert
		     "<a name=\""
		     link-fragment
		     "\">\n<ul>\n</ul>\n")
		    (goto-char (point-min))
		    (search-forward (format "<a name=\"%s\">" link-fragment)
				    (point-max)
				    t))
		(progn
		  ;; insert an ordinary insertion point
		  (insert
		   "<p>\nNew links:\n<ul>\n"
		   w3-hot-spot
		   "\n</ul>\n")
		  (goto-char (point-min)))
		(search-forward current-hot-spot (point-max) t))))

	(goto-char (match-beginning 0))
	(insert href))
      (basic-save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sort lists of links into bucket files ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-file-fragments (file &optional omit)
  "Return an alist of the fragment names in FILE.
Optional second argument is regexp for ones not to include."
  (let ((frags nil))
    (save-window-excursion
      (find-file file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<a name=\"\\(.+\\)\">" (point-max) t)
	  (let ((name (buffer-substring (match-beginning 1) (match-end 1))))
	    (if (or (null omit) (not (string-match omit name)))
		(push (cons name
			    (match-beginning 0))
		      frags))))))
    (nreverse frags)))

(defun w3-list-file-fragments (file omit)
  "Display an alist of the fragment names in FILE.
Second argument is regexp for ones not to include."
  (interactive "fFile of which to list fragments:
sOmit fragments matching: ")
  (let ((frags (w3-file-fragments file omit)))
  (with-output-to-temp-buffer (format "*Fragments for %s" (file-name-nondirectory file))
    (princ (int-to-string (length frags)))
    (princ " fragments:\n\n")
    (while frags
      (princ (car (car frags)))
      (princ "\n")
      (setq frags (cdr frags))))))

(defun w3-classify-links ()
  "Interactively classify links from a list in the current buffer."
  (interactive)
  (let ((stop-here (save-excursion
		     (search-forward "</ul>" (point-max) t)
		     (point-marker)))
	(links-dir (file-name-directory w3-oven-file))
	(link-file-name nil)
	(link-fragments nil)
	(link-fragment nil)
	(case-fold-search t)
	(completion-ignore-case t))
    (while (search-forward "<li>" stop-here t)
      (let ((start (match-beginning 0)))
	(if (search-forward "<li>" stop-here t)
	    (let* ((end (match-beginning 0))
		   (ref-string (buffer-substring start end)))
	      (delete-region start end)
	      (with-output-to-temp-buffer
		  "*Link*"
		(princ "Link to classify:\n")
		(princ ref-string))
	      (recenter)
	      (setq link-file-name (read-file-name "Put link into: "
						   links-dir
						   link-file-name
						   t
						   nil)
		    link-fragments (w3-file-fragments link-file-name
						       "insertion"
						      )
		    link-fragment (if link-fragments
				      (completing-read "Section: "
						       link-fragments
						       nil
						       t
						       nil
						       nil)
				    nil))

	      (w3-heat-link
	       nil nil
	       ref-string
	       link-file-name link-fragment)
	      (goto-char (1- start)))
	  (goto-char start))))))

(defun w3-classify-links-same-file ()
  "Interactively classify links from point onwards into sections of this buffer."
  (interactive)
  (let* ((links-dir (file-name-directory w3-oven-file))
	 (link-file-name (buffer-file-name))
	 (case-fold-search t)
	 (link-fragments (w3-file-fragments link-file-name
					    "insertion"
					    ))
	 (link-fragment nil)
	 (completion-ignore-case t))
    (while (search-forward "<li>" (point-max) t)
      (let ((start (match-beginning 0)))
	(if (search-forward "<li>" (point-max) t)
	    (let* ((end (match-beginning 0))
		   (ref-string (buffer-substring start end)))
	      (delete-region start end)
	      (with-output-to-temp-buffer
		  "*Link*"
		(princ "Link to classify:\n")
		(princ ref-string))
	      (setq link-fragment (if link-fragments
				      (completing-read "Section: "
						       link-fragments
						       nil
						       t
						       nil
						       nil)
				    nil))

	      (w3-heat-link
	       nil nil
	       ref-string
	       link-file-name link-fragment)
	      (goto-char (1- start)))
	  (goto-char start))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Convert and insert NetScape bookmark file ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'replace-regexp-list)

(defun w3-insert-bookmark-from-Netscape (bookmark)
  "Convert BOOKMARK file into <ul> items and insert at point."
  (interactive "fInsert bookmarks from file: ")
  (insert (save-window-excursion
	    (set-buffer (get-buffer-create " *Bookmark conversion*"))
	    (insert-file-contents bookmark ; file name
				  nil	   ; do not visit
				  nil nil  ; beg, end -- whole file
				  t        ; replace old contents
				  )
	    (goto-char (point-min))
	    (search-forward "<DT>")
	    (delete-region (point-min) (match-beginning 0))
	    (search-forward "</DL>")
	    (delete-region (match-beginning 0) (point-max))
	    (apply-replace-regexp-alist
	     '(("" . "")
	       ("^ *<DT>" . "<li> ")
               ("<A HREF=" . "<a href=")
	       ("</A>" . "</a>")
	       (" [A-Z]+_[A-Z]+=\"[0-9]+\"" . "")))
	    (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sort a list of links around point ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-list-around-point (where)
  "Return a cons of the start and end points of the HTML list containing WHERE."
  (save-excursion
    (let (start)
      (goto-char where)
      (search-backward "<ul>")
      ;; (re-search-backward "<\\(ul\\)\\|\\(ol\\)>")
      (setq start (match-end 0))
      (search-forward "</ul>")
      ;; (re-search-forward "</\\(ul\\)\\|\\(ol\\)>")
      (cons start (match-beginning 0)))))

(defun w3-sort-link-list (where)
  (interactive "d")
  (require 'replace-regexp-list)
  ;; (with-output-to-temp-buffer "*hackery"
  (let* ((case-fold-search t)
	 (list-extents (w3-list-around-point where)))
    (save-restriction
      (narrow-to-region (car list-extents) (cdr list-extents))
      ;; (prin1 (buffer-string))
      (goto-char (point-min))
      (apply-replace-regexp-alist
       '(("<LI>" . "<li>")
	 ("<A HREF=" . "<a href=")
	 ("</A>" . "</a>")
	 ("\n[^<]". " <"))
       (point-min)
       (point-max)
       t t)
      ;; (prin1 (buffer-string))
      (shell-command-on-region (point-min)
			       (point-max)
			       "sort -u"
			       t)
      ;; (prin1 (buffer-string))
      ))
  ;; )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rework ones we have mucked up ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-tidy-link-file (file)
  "Tidy up damage I've done to FILE, which should be in my link file format."
  (interactive "fTidy link file: ")
  (find-file file)
  (goto-char (point-min))
  (if (not (re-search-forward "\\(<html>\\)\\|\\(<head>\\)\\|\\(<title>\\)" (point-max) t))
      (error "You'll have to tidy this one yourself -- it has no header so the format is probably wrong"))
  (let ((recovered-links nil)
	(header-marker (make-marker)))
    (set-marker header-marker (match-beginning 0))
    ;; (goto-char header-marker) (message "Marker is here") (sit-for 2)
    (goto-char (point-min))
    (while (re-search-forward "\\(<a href.+/a>\\)" header-marker t)
      (push (buffer-substring (match-beginning 1) (match-end 1)) recovered-links)
      (delete-region (match-beginning 1) (match-end 1)))
    (delete-region (point-min) header-marker)
    (while recovered-links
      (w3-heat-link nil nil (concat "<li> " (car recovered-links) "\n") file nil)
      (setq recovered-links (cdr recovered-links)))))

;;; end of w3-heat-link.el
