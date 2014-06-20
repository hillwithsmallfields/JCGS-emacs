;;;; Report on connectivity within a web site mastered here
;;; Time-stamp: <2005-01-18 19:04:55 jcgs>

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

(provide 'connectivity)
(require 'cl)
(require 'webmaster-macros)

(defun connectivity:register-url (url)
  "Register URL in the connectivity data structures."
  (message "Registering %s" url)
  (let ((abs-url (webmaster:absolute-url url)))
    (when (string-match "^http:" abs-url)
      (let ((hash (string-match "#" abs-url)))
	(when hash
	  (setq abs-url (substring abs-url 0 hash))))
      (if (not (null webmaster:page-site-homepage-url))
	  (progn
	    (connectivity:add
	     (connectivity:find connectivity:from webmaster:page-site-homepage-url webmaster:page-url)
	     abs-url)
	    (connectivity:add
	     (connectivity:find connectivity:to webmaster:page-site-homepage-url abs-url)
	     webmaster:page-url))
	(message "Could not register %s" url))))
  nil)

(defun connectivity:find (direction site url)
  "Find the data structure collecting links in DIRECTION in SITE from/to URL."
  ;; (message "connectivity:find(direction=%S, site=\"%s\", url=\"%s\")" direction site url)
  (let* ((for-site (assoc site direction))
	 (for-url (assoc url (cdr for-site))))
    ;; (message "for-site=%S for-url=%S" for-site for-url)
    ;; I'm having problems with for-site being null here (error occurs at the push below)
    ;; what does this mean, and is it OK?
    ;; maybe just not initialized properly?
    (when (null for-url)
      (setq for-url (cons url nil))
      (push for-url (cdr for-site)))
    for-url))

(defun connectivity:add (collection url)
  "To COLLECTION add URL."
 (pushnew url (cdr collection) :test 'string=))

(defvar connectivity:from nil
  "Alist of site names to alists of links from pages.")

(defvar connectivity:to nil
  "Alist of site names to alists of links to pages.")

(defun connectivity:compare (a b)
  "Compare connectivity records A and B."
  (> (length (cdr a)) (length (cdr b))))
 
(defun connectivity:sort (direction site)
  "Sort the connectivity data for DIRECTION and SITE."
  (let ((pair (assoc site direction)))
    (rplacd pair (sort (cdr pair) 'connectivity:compare))))

(defun connectivity:start-site (url)
  "Reset and initialize connectivity data for the site at URL."
  ;; (message "Resetting connectivity data for \"%s\"" url)
  (let ((from-site (assoc url connectivity:from)))
    (if from-site
	(rplacd from-site nil)
      (push (cons url nil) connectivity:from)))
  (let ((to-site (assoc url connectivity:to)))
    (if to-site
	(rplacd to-site nil)
      (push (cons url nil) connectivity:to)))
  (message "connectivity:from=%S connectivity:to=%S" connectivity:from connectivity:to)
  )

(defun connectivity:scan-site (site)
  "Scan SITE, setting up data structures recording its connectivity."
  (connectivity:start-site site)
  (webmaster:apply-to-urls-throughout-tree (webmaster:file-of-url site) t (list 'connectivity:register-url)))

;;;###autoload
(defun connectivity:report-on-site (site)
  "Produce a connectivity report on SITE."
  (interactive "sURL: ")
  (connectivity:scan-site site)
  (connectivity:sort connectivity:to site)
  (with-output-to-temp-buffer (format "*Connectivity of %s*" site)
    (let ((outer-list (cdr (assoc site connectivity:to))))
      (dolist (outer outer-list)
	(princ (format "Links to %s (%d):\n" (car outer) (length (cdr outer))))
	(let ((inner-list (cdr outer)))
	  (dolist (inner inner-list)
	    (princ (format "  %s\n" inner))))
	(princ "\n")))))

;;; end of connectivity.el
