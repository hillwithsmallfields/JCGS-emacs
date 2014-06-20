;;;; misc-patchup.el -- various mending of pages
;;; Time-stamp: <2004-11-20 12:06:30 jcgs>

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

(provide 'misc-patchup)
(require 'webmaster-macros)


(defun ensure-page-has-html ()
  "Ensure the page has html markers."
  (interactive)
  (webmaster:throughout-buffer
   (unless (search-forward "<html>" (point-max) t)
     (goto-char (point-min))
     (insert "<html>\n")))
  (webmaster:throughout-buffer
   (unless (search-forward "</html>" (point-max) t)
     (goto-char (point-max))
     (insert "\n</html>\n"))))

(defun ensure-page-has-head ()
  "Ensure the page has a head."
  (interactive)
  (ensure-page-has-html)
  (webmaster:throughout-buffer
   (unless (search-forward "<head>" (point-max) 1)
     (search-backward "<html>")
     (insert "<head>\n")))
  (webmaster:throughout-buffer
   (unless (search-forward "</head>" (point-max) 1)
     (search-backward "<html>")
     (insert "</head>\n"))))

(defun html-match-heading ()
  "Set the match data to the first match for a heading.
Returns the result of the re-search-forward that succeeded."
  (let ((case-fold-search t))
    (cond
     ((save-excursion (re-search-forward "<h1.*>\\(.+\\)</h1>" (point-max) t)))
     ((save-excursion (re-search-forward "<h2.*>\\(.+\\)</h2>" (point-max) t)))
     ((save-excursion (re-search-forward "<h3.*>\\(.+\\)</h3>" (point-max) t)))
     ((save-excursion (re-search-forward "<h4.*>\\(.+\\)</h4>" (point-max) t)))
     ((save-excursion (re-search-forward "<h5.*>\\(.+\\)</h5>" (point-max) t)))
     ((save-excursion (re-search-forward "<h6.*>\\(.+\\)</h6>" (point-max) t)))
     (t nil))))

(defun guess-page-title ()
  "Guess a title for this page."
  (webmaster:throughout-buffer
   (cond
    ((html-match-heading)
     (match-string 1))
    ((save-excursion
       (re-search-forward "<!--#set var=\"title\" value=\"\\(.+\\)\" -->" (point-max) t))
     (match-string 1))
    (t (buffer-name)))))

(defun guess-page-title ()
  "Guess a title for this page."
  (webmaster:throughout-buffer
   (cond
    ((html-match-heading)
     (match-string 1))
    ((save-excursion
       (re-search-forward "<!--#set var=\"title\" value=\"\\(.+\\)\" -->" (point-max) t))
     (match-string 1))
    (t (buffer-name)))))


(defun ensure-page-has-title ()
  "Ensure this page has a title, using the heading if necessary and possible."
  (interactive)
  (ensure-page-has-head)
  (webmaster:throughout-buffer (delete-matching-lines "^<title>AceDB </title>$"))
  (webmaster:throughout-buffer
   (unless (re-search-forward "<title>" (point-max) t)
     (goto-char (point-max))
     (search-backward "</head>")
     (insert "\n<title>" (guess-page-title) "</title>\n"))))


(defun ensure-tree-has-titles (tree)
  "Ensure that all pages in TREE have titles."
  (interactive "DTree: ")
  (webmaster:apply-throughout-tree tree 'ensure-page-has-title nil nil))





(defun make-symbolic-linkd (filename linkname)
  "dummy"
  (message "Making link %s to file %s" linkname filename))

(defun symlink-files-in-directories (files directories)
  "Make a symlink to each of FILES in each of DIRECTORIES."
  (let* ((filepairs (mapcar (function
			     (lambda (file)
			       (cons (file-name-nondirectory file)
				     (expand-file-name file))))
			    files))
	 (fulldirs (mapcar 'expand-file-name directories)))
    (dolist (dir fulldirs)
      (dolist (filepair filepairs)
	(make-symbolic-link
	 (cdr filepair)
	 (expand-file-name (car filepair) dir)
	 t)))))



$COMMON

;;; end of misc-patchup.el
