;;;; make-versor-web.el -- set up the versor web pages
;;; Time-stamp: <2018-11-15 19:25:21 jcgs>

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


(add-to-list 'load-path (expand-file-name "webstuff" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "webstuff/webmastering" user-emacs-directory))
(require 'change-html-head-tail)

(setq stack-trace-on-error t)

(let ((manual-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-versor/htdocs/manual"))
      (manuals '("versor" "languide")))
  (mapcar (lambda (manual)
	    (let ((dir (expand-file-name manual manual-dir)))
	      (html-re-tail-directory dir)
	      (html-re-tail-page (expand-file-name "index.html" dir))))
	  manuals))

;;;; some post-processing stuff for the web pages

(defun insert-versor-sourceforge-logo ()
  (insert "<br clear=\"all\"><a href=\"http://sourceforge.net\"><img
  src=\"http://sourceforge.net/sflogo.php?group_id=97002&amp;type=2\"
  align=\"right\"
  width=\"125\"
  height=\"37\"
  border=\"0\"
  alt=\"SourceForge.net Logo\" /></a>"))

(defun versor-demo-postprocess-web-demo-recording ()
  (interactive)
  (let* ((demo-image-dir (substitute-in-file-name
			  "$OPEN_PROJECTS/emacs-versor/htdocs/demo"))
	 (images (directory-files demo-image-dir
				  t
				  "\\.png$"
				  t)))
    (message "%d images" (length images))
    (while images
      (let* ((image (car images))
	     (base-file (file-name-nondirectory image))
	     (matched (string-match "-\\([0-9]+\\)\\.png" image))
	     (number-string (match-string 1 image))
	     (step-number (if (stringp number-string)
			      (string-to-number number-string)
			    0)))
	(message "Processing step %d" step-number)
	(find-file (expand-file-name (format "demo-step-%d.html" step-number)
				     demo-image-dir))
	(erase-buffer)
	(insert (format "<html><head><title>Versor demo step %d</title></head>\n" step-number))
	(insert "<body>\n")
	(insert
	 (format
	  "<a href=\"demo-step-%d.html\"><img border=\"0\" src=\"%s\"></a>\n"
	  (1+ step-number) base-file))
	(insert "<table width=\"100%\" border=\"0\"><tr><td align=\"left\">")
	(insert (format "<a href=\"demo-step-%d.html\">Prev</a>" (1- step-number)))
	(insert (format "</td><td align=\"center\">Demo frame %d</td><td align=\"right\">"
			step-number))
	(insert (format "<a href=\"demo-step-%d.html\">Next</a>" (1+ step-number)))
	(insert "</td></tr></table>\n")
	(insert "<p>")
	(let ((textfile (expand-file-name (format "demo-step-%d.txt" step-number)
					  demo-image-dir)))
	  (when (file-exists-p textfile)
	    (insert-file-contents textfile)
	    (goto-char (point-max))))
	(insert "</p>\n")
	(insert-versor-sourceforge-logo)
	(insert "</body></html>\n")
	(basic-save-buffer)
	(kill-buffer nil))
      (setq images (cdr images)))))

(versor-demo-postprocess-web-demo-recording)

(save-buffers-kill-emacs t)

;;; end of make-versor-web.el
