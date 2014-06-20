;;; make html page file names relative to a base

(provide 'page-attributes)
(require 'cl)

;;; <title>Normalize filename for a locally stored page</title>

;; Looks after a collection of website directories, and for each file
;; visited, sets a collection of per-buffer variables that can then be
;; used in summarizing sites, re-writing pages, etc.

(mapcar 'makunbound '(recent-changes-file recent-changes-base-directory))

(defun standardize-pathname-delimiters (filename)
  "Make FILENAME have forward slashes for delimiters."
  (let* ( ;; (newname (copy-sequence filename))
	 (newname filename)
	 (i (1- (length filename))))
    (while (>= i 0)
      (if (= (aref newname i) 92)
	  (aset newname i ?/))
      (setq i (1- i)))
    newname))

(defvar recent-changes-file
  (standardize-pathname-delimiters
   (expand-file-name "~/LocalWWW/recent.html"))
  "Where to record recent changes")

(defvar recent-changes-base-directory 
  (standardize-pathname-delimiters
   (file-name-directory recent-changes-file))
  "The directory to which recent changes are taken relatively")

(defun trimmed-name (filename)
  "Return FILENAME relative to recent-changes-file."
  (setq filename (standardize-pathname-delimiters
		  (expand-file-name filename)))
  (substring filename (length recent-changes-base-directory)))

(defvar page-url-base "http://www.mysite.org/~me/"
  ;; todo: make this relative to webmaster:web-site-directories entries
  "Base for file-page-url to prepend to (trimmed-name filename) to make a URL
as will be seen once uploaded.")

(defun file-page-url (filename)
  "Return FILENAME as a URL as it will be seen once uploaded.
This uses trimmed-name and page-url-base."
  (concat page-url-base (trimmed-name filename)))

(defun link-is-internal (url)
  "Return whether URL appears to be an internal link."
  (or (not (string-match ":" url))
      (and (> (length url)
	      (length page-url-base))
	   (string= page-url-base (substring url 0 (length page-url-base))))))

(defvar webmaster:web-site-directories nil 
 "Alist listing web sites sourced from directories on this system.
This is used to work out whether a local filename contains the source
for a URL on the web, or, given a URL, whether we have a file for it.
These correspondences are partly worked out when an HTML file is loaded,
so this variable must be set up before visiting the files concerned.

webmaster:define-site-directory is provided for adding to this list.")

(defvar webmaster:contact-addresses nil
  ;; todo: make use of this!
  "Alist of contact addresses for web sites.")

(defun webmaster:define-site-directory (site directory &optional contact-url)
  "Define SITE as being sourced from DIRECTORY."
  (message "directory %S; (substitute-in-file-name directory) %S" directory (substitute-in-file-name directory))
  (let* ((true-directory (file-truename (substitute-in-file-name directory)))
	 (sdpair (assoc site webmaster:web-site-directories))
	 (dspair (rassoc true-directory webmaster:web-site-directories)))
    (if sdpair (rplacd sdpair true-directory))
    (if dspair (rplaca dspair site))
    (if (and (null sdpair) (null dspair))
	(setq webmaster:web-site-directories
	      (cons (cons site true-directory)
		    webmaster:web-site-directories))))
  (if contact-url
      (setq webmaster:contact-addresses
	    (cons (cons site contact-url)
		  webmaster:contact-addresses))))

(defun webmaster:find-site-pair-for-file (file)
  "Find the data structure defining FILE to be part of a web site."
  ;; (message "Looking for site containing %s" file)
  (let* ((file-true-name (file-truename file))
	 (dirs webmaster:web-site-directories)
	 (pair (catch 'found
		 (dolist (dir dirs)
		   (when (and
			  (> (length file-true-name) (length (cdr dir)))
			  (string=
			   (cdr dir) (substring file-true-name 0 (length (cdr dir)))))
		     (throw 'found dir)))
		 nil)))
    ;; (message "%S contains %s" pair file)
    pair))

(defun webmaster:find-site-pair-for-url (url)
  "Find the data structure defining URL to be part of a web site stored here."
  (let ((dirs webmaster:web-site-directories))
    (catch 'found
      (dolist (dir dirs)
	(when (and
	       (>= (length url) (length (car dir)))
	       (string=
		(car dir) (substring url 0 (length (car dir)))))
	  (throw 'found dir))
	(setq dirs (cdr dirs)))
      nil)))

;;;; Webmaster page variables.
;;; Perhaps these all really ought to be in one big defstruct?

(makunbound 'webmaster:variables)

(defvar webmaster:variables '(webmaster:page-url
			      webmaster:page-site-homepage-url
			      webmaster:page-filename
			      webmaster:page-directory-name
			      webmaster:page-site-homepage-directory-name
			      webmaster:page-title
			      webmaster:page-logo
			      webmaster:page-top-banner
			      webmaster:page-stylesheet
			      webmaster:page-first-heading
			      webmaster:page-doctype
			      webmaster:page-author
			      webmaster:page-body-options
			      webmaster:page-head-apache-directives
			      webmaster:page-head-tags
			      webmaster:page-footer)
  "Per-buffer variables for web source buffers.")

(mapcar 'makunbound webmaster:variables)

(mapcar 'make-variable-buffer-local webmaster:variables)

(defvar webmaster:page-url nil 
 "The URL for the web page sourced from this file, or nil.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-filename nil 
 "The filename for this file, in the context of it being served as a web page.
This is probably superfluous, but provided for orthogonality: it is
just the filename of the file in the buffer, or is nil if the file is
not under anything in webmaster:web-site-directories.")

(defvar webmaster:page-site-homepage-url nil
 "The URL for the home page of the site containing the page sourced by this file, or nil.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-directory-name nil 
 "The directory name for this file, in the context of it being served as a web page.
This is relative to the top level directory of the web site, or is nil
if the file is not under anything in webmaster:web-site-directories.")

(defvar webmaster:page-site-homepage-directory-name nil 
 "The directory name of the home page of the site containing the page sourced by this file, or nil.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-title nil
  "The title of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-logo nil
  "The logo URL of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-top-banner nil
  "The top banner of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-stylesheet nil
  "The stylesheet of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-first-heading nil
  "The first-heading of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-doctype nil
  "The doctype of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-author nil
  "The author of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-body-options nil
  "The body-options of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-head-apache-directives nil
  "The apache-directives of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-head-tags nil
  "The head tags of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defvar webmaster:page-footer nil
  "The footer area of this page, as found when the file was visited.
This variable has a separate value in each buffer, and is non-nil for
files in directories specified in webmaster:web-site-directories.")

(defun webmaster:set-page-variables ()
  "Set the page variables of this buffer.
This is meant for calling from the find-file-hook for HTML files,
and also perhaps from the write-file-hook to pick up changes."
  (interactive)
  (dolist (var webmaster:variables)
    (set var nil))
  (let ((pair (webmaster:find-site-pair-for-file buffer-file-name)))
    (if pair
	(let* ((fulldir (file-truename default-directory)))
	  ;; (message "%s is in %s in %s which sources %s" buffer-file-name fulldir (cdr pair) (car pair))
	  (setq webmaster:page-filename (file-truename buffer-file-name)
		webmaster:page-site-homepage-directory-name (cdr pair)
		webmaster:page-site-homepage-url (car pair)
		webmaster:page-directory-name (substring fulldir
							 (length webmaster:page-site-homepage-directory-name))
		webmaster:page-url (concat webmaster:page-site-homepage-url
					   webmaster:page-directory-name
					   (file-name-nondirectory buffer-file-name))
		webmaster:page-title (get-page-title)
		webmaster:page-logo (get-page-logo)
		webmaster:page-top-banner (get-page-top-banner)
		webmaster:page-stylesheet (get-page-stylesheet)
		webmaster:page-first-heading (get-page-first-heading)
		webmaster:page-doctype (get-page-doctype)
		webmaster:page-author (get-page-author)
		webmaster:page-body-options (get-page-body-options)
		webmaster:page-head-apache-directives (get-page-head-apache-directives)
		webmaster:page-head-tags (get-page-head-tags)
		webmaster:page-footer (get-page-footer)
		)
	  (if nil
	      (message "In %s (part of %s), set webmaster:page-first-heading to \"%s\""
		       buffer-file-name
		       (car pair)
		       webmaster:page-first-heading)))
      (message "%s not in a web site" buffer-file-name)))
  )

(defun webmaster:show-webmaster-page-variables (&optional output-buffer)
  (interactive)
  "Display the variables used to keep track of web pages."
  (with-output-to-temp-buffer (if output-buffer output-buffer"*Web page location variables*")
    (let ((body-string (webmaster:page-body-content)))
      (princ (format "Web variables for buffer %s:
                  Filename: %s
      Web site (home page): %s
                  Page URL: %s
Page directory within site: %s
        Homepage directory: %s
                     title: %s
                      logo: %s
                top banner: %s
                stylesheet: %s
             first heading: %s
                   doctype: %s
                    author: %s
              body-options: %s
         apache-directives: %s
                 head tags: %s
                    footer: %s
                      body: %s...%s"
		     (buffer-name)
		     webmaster:page-filename
		     webmaster:page-site-homepage-url
		     webmaster:page-url
		     webmaster:page-directory-name
		     webmaster:page-site-homepage-directory-name
		     webmaster:page-title
		     webmaster:page-logo
		     webmaster:page-top-banner
		     webmaster:page-stylesheet
		     webmaster:page-first-heading
		     webmaster:page-doctype
		     webmaster:page-author
		     webmaster:page-body-options
		     webmaster:page-head-apache-directives
		     webmaster:page-head-tags
		     webmaster:page-footer
		     (substring body-string 0 24) (substring body-string -24)
		     )))))

;;;###autoload
(defun webmaster:show-webmaster-sites ()
  "Display the web sites handled by this installation of this package."
  (interactive)
  (with-output-to-temp-buffer "*Web sites in directories*"
    (let ((sites webmaster:web-site-directories)
	  (pair-format (format "%% %ds: %%s\n"
			       (apply 'max
				      (mapcar (function
					       (lambda (pair)
						 (length (car pair))))
					      webmaster:web-site-directories)))))
      (dolist (site sites)
	(princ (format pair-format (car site) (cdr site)))))))

;;;###autoload
(defun webmaster:find-file-hook ()
  "Set up some variables for files that appear to be the source for web pages.
This uses webmaster:web-site-directories to find the possible web trees."
  (webmaster:set-page-variables))

;;;; various transformations and conversions of file names and URLs

(defun url-buffer-visiting (url)
  "Return the buffer visiting the file for URL, or nil."
  (let ((file (webmaster:file-of-url url)))
    (if file
	(find-buffer-visiting file)
      nil)))

(defun webmaster:file-of-url (url)
  "Given URL return the file in which its source is stored."
  (let ((pair (webmaster:find-site-pair-for-url url)))
    (message "Looked up %s and got pair %S" url pair)
    (if pair
	(let* ((base-url (car pair))
	       (base-dir (substitute-in-file-name (cdr pair)))
	       (part-url (substring url (length base-url))))
	  (concat base-dir part-url))
      nil)))

(defun webmaster:url-of-file (file)
  "Given FILE return the URL by which it may be referenced."
  (setq file (file-truename file))
  (let ((pair (webmaster:find-site-pair-for-file file)))
    (if pair
	(let* ((base-url (car pair))
	       (base-dir (substitute-in-file-name (cdr pair)))
	       (part-file (substring file (length base-dir))))
	  (concat base-url part-file))
      nil)))

(defun webmaster:homepage-url-of-file (file)
  "Given FILE return the URL for the top level of the site containing it."
  (let ((pair (webmaster:find-site-pair-for-file file)))
    (if pair
	(car pair)
      nil)))

(defun webmaster:homepage-url-of-url (url)
  "Given URL return the URL for the top level of the site containing it."
  (let ((pair (webmaster:find-site-pair-for-url url)))
    (if pair
	(car pair)
      nil)))

(defun resolvedotdots (dirstring)
  "Resolve ../ constructs in a string of directories.
May return a new string."
  (while (string-match "[^/]+/\\.\\./" dirstring)
    (setq dirstring (concat (substring dirstring 0 (match-beginning 0))
			    (substring dirstring (match-end 0)))))
  (while (string-match "/\\./" dirstring)
    (setq dirstring (concat (substring dirstring 0 (match-beginning 0))
			    (substring dirstring (1- (match-end 0))))))
  dirstring)

(defun webmaster:absolute-url (relurl &optional reltodir)
  "Transform RELURL to an absolute URL.
RELURL is normally taken to be relative to the default directory of the
current buffer, but if optional RELTODIR is given, that is used instead."
  (if (string-match ":" relurl)
      relurl
    (progn
      (if (null reltodir) (setq reltodir default-directory))
      (let ((dirurl (webmaster:url-of-file reltodir)))
	(resolvedotdots (concat dirurl relurl))))))

(defun longest-initial-identical-substring (a b)
  "Return the longest identical initial substring of the two arguments."
  (let ((n (compare-strings a nil nil b nil nil nil)))
    (cond
     ((eq n t) a)
     ((< n 0) (substring a 0 (- -1 n)))
     ((> n 0) (substring a 0 (1- n))))))

(defun longest-identical-initial-directory-string (a b)
  "Return the longest sequence of initial pathname parts that are identical in A and B."
  (let ((common-string (longest-initial-identical-substring a b)))
    (if (string-match "/$" common-string)
	common-string
      (let ((match (string-match "/[^/]+$" common-string)))
	(when
	    match
	  (substring common-string 0 (1+ match)))))))

(defun go-up-directories (dirstring)
  ;; this is still not right!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  "Produce a string of ../s as needed to go back up the directories given in DIRSTRING."
  (let ((dircount (count ?/ dirstring :test '=)))
    (apply 'concat (make-list dircount "../"))))

(defun webmaster:relative-url (absurl &optional reltodir)
  "Transform ABSURL to a relative URL.
This is normally relative to the web directory equivalent to the default
directory of the current buffer, but if RELTODIR is given, that is used
instead."
  (if (string-match "^mailto:" absurl)
      absurl
    (progn
      (when (null reltodir) 
	(setq reltodir default-directory))
      (let ((urlfile
	     (if (string-match "\\(..+\\):" absurl)
		 (webmaster:file-of-url absurl)
	       absurl)))
	(if (null urlfile)
	    absurl
	  (let* ((relfile (file-truename reltodir))
		 (common-dir (length (longest-identical-initial-directory-string urlfile relfile)))
		 (justurl (substring urlfile common-dir))
		 (justrel (substring relfile common-dir))
		 (backfromrel (go-up-directories justrel))
		 (combined (concat backfromrel justurl)))
	    (message "urlfile=%s relfile=%s common-dir=%d:%s justurl=%s justrel=%s backfromrel=%s combined=%s" urlfile relfile common-dir (longest-identical-initial-directory-string urlfile relfile) justurl justrel backfromrel combined)
	    combined))))))

;;;; Store various information about the page in buffer-local variables.
;;; This should be useful for rebuilding the structural parts of the page,
;;; and perhaps for more besides.

(defun get-page-title ()
  "Get the title of the current page."
  (webmaster:throughout-buffer
   (if (re-search-forward "<title>\\(.+\\)</title>" (point-max) t)
       (match-string-no-properties 1)
     (goto-char (point-min))
     (if (re-search-forward "<!--#set var=\"title\" value=\"\\([^\"]+\\)\" -->" (point-max) t)
	 (match-string-no-properties 1)
       (goto-char (point-min))
       (if (re-search-forward "<h1>\\(.+\\)</h1>" (point-max) t)
	   (match-string-no-properties 1)
	 (let ((from-apache (get-page-head-apache-variable "title")))
	   (if from-apache
	       from-apache
	     (capitalize (file-name-nondirectory (buffer-file-name))))))))))

(defun match-in-region (pattern start end)
  "Return the string defined by the first match-save in PATTERN found between START and END."
  (save-excursion
    (goto-char start)
    (if (re-search-forward pattern end t)
	(match-string-no-properties 1)
      nil)))

(defun img-looks-like-logo-p (start end)
  "Return whether the html between START and END appears to be an img for a logo."
  (save-excursion
    (goto-char start)
    (search-forward "logo" end t)))

(defun find-img (direction limit)
  "Look in DIRECTION ('forward or 'backward) as far as LIMIT, for an img tag.
Return the start and end thereof, as a cons."
  (if (funcall (cdr (assoc direction '((forward . re-search-forward) (backward . re-search-backward))))
	       "<img.+>" limit t)
      (cons (match-beginning 0) (match-end 0))
    nil))

(defun get-page-logo ()
  "Try to find a URL for a logo for this page."
  (save-excursion
    (let ((halfway (/ (point-max) 2)))
      (goto-char (point-min))
      (let ((found (find-img 'forward halfway)))
	(if (and found (img-looks-like-logo-p (car found) (cdr found)))
	    (match-in-region "src=\"\\([^\"]+\\)\"" (car found) (cdr found))
	  (goto-char (point-max))
	  (let ((found (find-img 'backward halfway)))
	    (if (and found (img-looks-like-logo-p (car found) (cdr found)))
		(match-in-region "src=\"\\([^\"]+\\)\"" (car found) (cdr found))
	      (get-page-head-apache-variable "logo"))))))))

(defun get-page-top-banner ()
  "Get the top banner of the page."
  (webmaster:throughout-buffer
   (if (re-search-forward "<!-- begin .*banner.*-->" (point-max) t)
       (let* ((start (match-beginning 0))
	      (end (re-search-forward "<!-- end .*banner.*-->" (point-max) t)))
	 (if end
	     (buffer-substring-no-properties start end)
	   nil))
     nil)))

(defun get-page-stylesheet ()
  "Get the stylesheet of the current page, as an alist with keys 'href, 'type, and 'title."
  (webmaster:throughout-buffer
   (if (re-search-forward "<link rel=\"StyleSheet\"" (point-max) t)
       (let* ((start (point))
	      (end (save-excursion (search-forward ">" (point-max) t))))
	 (if nil
	     ;; I thought it would be good to keep all this detail...
	     (list (cons 'href (match-in-region "href=\"\\([^\"]+\\)\"" start end))
		   (cons 'type (match-in-region "type=\"\\([^\"]+\\)\"" start end))
		   (cons 'title (match-in-region "title=\"\\([^\"]+\\)\"" start end)))
	   ;; but it's so much simpler just to keep the href string!
	   (match-in-region "href=\"\\([^\"]+\\)\"" start end)))
     (get-page-head-apache-variable "stylesheet"))))

(defun get-page-first-heading ()
  "Get the first heading of the current page."
  (let ((from-apache (get-page-head-apache-variable "firstheading")))
    (if from-apache
	from-apache
      (webmaster:throughout-buffer
       (if (re-search-forward "<h1>\\(.+\\)</h1>" (point-max) t)
	   (match-string-no-properties 1)
	 (if (re-search-forward "<h[2-6]>\\(.+\\)</h[2-6]>" (point-max) t)
	     (match-string-no-properties 1)
	   (capitalize (file-name-nondirectory (buffer-file-name)))))))))


(defconst html-doctypes '(("Default" . "-//IETF//DTD HTML//EN")
			  ("HTML 3.2" . "-//W3C//DTD HTML 3.2 Final//EN")
			  ("HTML 2.0" . "-//IETF//DTD HTML 2.0//EN")
			  ("HTML 4 Transitional" . "-//W3C//DTD HTML 4.0 Transitional//EN")
			  ("HTML 4 Strict" . "-//W3C//DTD HTML 4.0//EN"))
  "Alist of HTML versions to their doctypes.")

(defun get-page-doctype ()
  "Get the doctype of the current page."
  (if (re-search-forward "<!DOCTYPE HTML PUBLIC \"\\([^\"]+\\)\">" (point-max) t)
      (match-string-no-properties 1)
    "-//IETF//DTD HTML//EN"))

(defun get-page-author ()
  "Get the author link of the current page."
  (let ((author (let ((author (match-in-region "<!--#set var=\"author\" value=\"\\([^\"]+\\)\" -->"
					       (point-min) (point-max))))
		  (if author
		      author
		    (let ((author (match-in-region "<link rev=\"made\" href=\"\\([^\"]+\\)\">"
						   (point-min) (point-max))))
		      (if author
			  author
			"webmaster"))))))
    (if (string-match "^mailto:\\(.+\\)$" author)
	(substring author (match-beginning 1) (match-end 1))
      author)))

(defun get-page-body-options ()
  "Get the body options of the current page."
  (match-in-region "<body *\\([^>]+\\)>" (point-min) (point-max)))

(defun get-page-head-apache-directives ()
  "Get the apache directives of the current page, as a list in order."
  (webmaster:throughout-buffer
   (let ((end (re-search-forward "\\(</head.*>\\)\\|\\(end apache directives\\)" (point-max) t)))
     (goto-char (point-min))
     (if end
	 (let ((directives nil))
	   (while (re-search-forward "^ *<!--#\\(.+\\)-->" end t)
	     (push (match-string 0) directives))
	   (nreverse directives))
       nil))))

(defun get-page-head-apache-directive (directive optkey optval)
  "Get the Apache directive matching DIRECTIVE OPTKEY=OPTVAL."
  (car (get-page-head-apache-directive-list directive optkey optval)))

(defun get-page-head-apache-directive-list (directive optkey optval)
  "Get the Apache directive matching DIRECTIVE OPTKEY=OPTVAL.
The result is returned as the part of the list of directives
that begins with the one desired, allowing it to be replaced
using rplaca."
  (let ((pattern (format "<!--#%s .*%s=\"%s\"" directive optkey optval)))
    (catch 'found
      (let ((directives webmaster:page-head-apache-directives))
	(while directives
	  (when (string-match pattern (car directives))
	    (throw 'found directives))
	  (setq directives (cdr directives)))
	nil))))

(defun set-page-head-apache-directive (directive optkey optval newval)
  "Set the Apache directive matching DIRECTIVE OPTKEY=OPTVAL to NEWVAL.
NEWVAL is the whole of the directive apart from the DIRECTIVE string
and the comment / directive syntax (thus allowing you to change the
optkey and optval parts of it).
The new value will be written into the file when it is rebuild by normalize-page.el."
;; (message "set-page-head-apache-directive: newval is %S" newval)
  (let ((pair (get-page-head-apache-directive-list directive optkey optval))
	(newstring (concat "<!--#" directive " " newval " -->")))
    (if pair
	(rplaca pair newstring)
      (push newstring webmaster:page-head-apache-directives))))

(defun get-page-head-apache-variable (varname)
  "Get the value of the Apache variable VARNAME."
  (let ((wholedef (get-page-head-apache-directive "set" "var" varname)))
    (if (and (stringp wholedef) (string-match "value=\"\\([^\"]+\\)\"" wholedef))
	(substring wholedef (match-beginning 1) (match-end 1))
      nil)))

(defun set-page-head-apache-variable (varname value)
  "Set the value of the Apache variable VARNAME to VALUE.
The new value will be written into the file when it is rebuilt by normalize-page.el."
  ;; (message "(set-page-head-apache-variable %S %S)" varname value)
  (set-page-head-apache-directive "set" "var" varname
   (format "var=\"%s\" value=\"%s\"" varname value)))

(defmacro apache-setq (name value)
  "Set Apache variable NAME to VALUE.
NAME is an unquoted lisp symbol like in setq."
  `(set-page-head-apache-variable (symbol-name ',name) ,value))

(defmacro apache-set (name value)
  "Set Apache variable NAME to VALUE.
NAME is a lisp symbol like in set."
  `(progn
     ;; (message "(apache-set %S %S)" ,name ,value)
     (set-page-head-apache-variable (symbol-name ,name) ,value)))

(defun get-page-head-tags ()
  "Get the head tags of the current page, as an alist of tag names
to alists of option names to values."
  (webmaster:throughout-buffer
   (let* ((start (re-search-forward "<head.*>" (point-max) t))
	  (end (re-search-forward "</head.*>" (point-max) t)))
     (if (and start end)
	 (let ((tags nil))
	   (goto-char start)
	   (while (re-search-forward "<\\(/?[a-z][a-z0-9]*\\)" end t)
	     (let* ((tagstart (point))
		    (tagname (match-string-no-properties 1))
		    (tagend (search-forward ">" end t))
		    (options nil))
	       (goto-char tagstart)
	       (while (re-search-forward "\\([a-z]+\\)=\"\\([^\"]+\\)\"" tagend t)
		 (push (cons (match-string-no-properties 1) (match-string-no-properties 2)) options))
	       (push (cons tagname (nreverse options)) tags)))
	   (nreverse tags))
       nil))))

(defun get-page-head-tag (tag optname optval)
  "Read the value of the first header tag matching TAG OPTNAME=OPTVAL."
  (catch 'found
    (dolist (wholetag webmaster:page-head-tags)
      (when (and (string= tag (car wholetag))
		 (string= optval (cdr (assoc optname (cdr wholetag)))))
	(throw 'found wholetag)))
    nil))

(defun set-page-head-tag (tag optname optval pairs)
  "Write the value of the first header tag matching TAG OPTNAME=OPTVAL to be those and PAIRs.
The new value will be written into the file when it is rebuild by normalize-page.el."
  (let ((tagdef (get-page-head-tag tag optname optval)))
    (if tagdef
	(rplacd tagdef (cons (cons optname optval) pairs))
      (push (cons tag (cons (cons optname optval) pairs))
	    webmaster:page-head-tags))
    ))

(defun get-page-footer ()
  "Get the footer text of this page."
  (webmaster:throughout-buffer
   (let ((end (or (and (re-search-forward "<!-- end.*footer -->"  (point-max) t) (match-beginning 0))
		  (and (re-search-forward "</body>" (point-max) t) (match-beginning 0))
		  (and (re-search-forward "</html>"  (point-max) t) (match-beginning 0))
		  (point-max))))
     (goto-char (point-max))
     (let ((start (or (and (re-search-backward "<!-- begin.*footer -->"  (point-min) t) (match-end 0))
		      (and (re-search-backward "<div[^>]*>" (point-min) t) (match-beginning 0))
		      (and (re-search-backward "<!-- end body content -->" (point-min) t) (match-end 0))
		      (and (re-search-backward "<hr[^>]*>" (point-min) t) (match-end 0))
		      (point-max))))
       (let* ((raw (buffer-substring-no-properties start end)))
	 (if (and nil (string-match "</div>" raw))
	     (replace-match "" t nil raw)
	   raw))))))

(defun webmaster:page-body-content ()
  "Return the body content (minus apparent initial heading and trailer) as a string."
  (webmaster:throughout-buffer
   (let ((start (or (save-excursion
		      (goto-char (point-max))
		      (and (re-search-backward "<!-- begin \\(body\\|main\\) \\(content\\|text\\) -->" (point-min) t)
			   (match-end 0)))
		    (re-search-forward "<h[1-6][^>]*>[^<]+</h[1-6]>" (point-max) t)
		    (re-search-forward "<body[^>]*>" (point-max) t)
		    (point-min))))
     (goto-char (point-max))
     (let ((end (or (save-excursion
		      (goto-char (point-min))
		      (and (re-search-forward "<!-- end \\(body\\|main\\) \\(content\\|text\\) -->" (point-max) t)
			   (match-beginning 0)))
		    (re-search-backward "<div class=\"?tail\"?>" (point-min) t)
		    (re-search-backward "</div>" (point-min) t)
		    (re-search-backward "<hr[^>]*>" (point-min) t)
		    (point-max))))
       (buffer-substring-no-properties start end)))))

(defun webmaster:re-read-all-page-variables ()
  "Re-read the page variables for all HTML buffers.
This is meant for use after re-writing some of the page variable
reading code."
  (interactive)
  (dolist (buffer (buffer-list))
    (set-buffer buffer)
    (when webmaster:page-url
      (message "Re-reading variables for %s" buffer)
      (webmaster:set-page-variables))))

(defun webmaster:show-page-variable-all-buffers ()
  "Show a variable in all buffers.
This is meant for use after re-writing some of the page variable
reading code."
  (interactive)
  (with-output-to-temp-buffer "*Page variable*"
    (let ((variable 'webmaster:page-first-heading))
      (dolist (buffer (buffer-list))
	(set-buffer buffer)
	(let ((value (symbol-value variable)))
	  (when value
	    (princ (format "%s: %s\n" (buffer-name) value))))))))

(defun webmaster:copy-page-url ()
  "Put the URL for the current page onto the kill ring."
  (interactive)
  (if (boundp 'webmaster:page-url)
      (kill-new webmaster:page-url)))

;;; end of page-attributes.el
