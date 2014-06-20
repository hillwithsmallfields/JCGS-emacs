;;;; <title>normalize-page.el -- rebuild a page usinga  given template</title>
;;; Time-stamp: <2005-01-18 12:05:56 john>

;;; Code to put a page together using a given template and the data
;;; extracted mostly using functions and variables in
;;; page-attributes.el (which understands a lot more than just the name
;;; and URL of the page file). The template is a list of things to
;;; evaluate (in the context of the buffer containing the original
;;; page and processed by webmaster:find-file-hook from
;;; page-attributes.el -- each thing should return a string (or list /
;;; tree of strings) to be inserted into the new buffer in order.

(provide 'normalize-page)
(require 'webmaster-macros)
(require 'page-attributes)

(defun insert-string-or-tree (thing)
  "Insert THING which may be a string, list of strings, list of (string or list...)..."
  (when thing
    (if (or (stringp thing) (integerp thing))
	(insert thing)
      (mapcar 'insert-string-or-tree thing))))

(defun webmaster:format-apache-directives (directives &optional just-these)
  "Format DIRECTIVES for output by insert-string-or-tree.
With optional JUST-THESE, format only the ones matching elements of JUST-THESE."
  (mapcar (function
	   (lambda (directive)
	     (if (or (null just-these)
		     (string-match-any just-these directive))
		 (list directive "\n")
	       nil)))
	  directives))

(defun metadata-match (metadata-item match-these)
  "Return whether METADATA-ITEM matches any of MATCH-THESE."
  (let ((metadata-tail (cdr metadata-item)))
    (catch 'matched
      (dolist (match-this match-these)
	(when (and (or (null match-this)
		       (string-match (car match-this) (car metadata-item)))
		   (not (catch 'failed
			  (dolist (option-pair (cdr match-this))
			    (let ((data-pair (assoc (car option-pair) metadata-tail)))
			      (if (and data-pair
				       (not (string-match (cdr option-pair) (cdr data-pair))))
				  (throw 'failed t))))
			  nil)))
	  (throw 'matched t)))
      nil)))

(defun webmaster:normalize-metadata (head-tags &optional just-these)
  "Format HEAD-TAGS for output by insert-string-or-tree.
With optional JUST-THESE, format only the ones matching elements of JUST-THESE,
which are in the form of (headmatch (fieldname . fieldmatch)+ )."
  (mapcar (function
	   (lambda (head-tag)
	     (if (or (null just-these)
		     (metadata-match head-tag just-these))
		 (append
		  (list "<" (car head-tag))
		  (mapcar (function
			   (lambda (option-pair)
			     (list " " (car option-pair) "=\"" (cdr option-pair) "\"")))
			  (cdr head-tag))
		  '(">\n"))
	       nil)))
	  head-tags))

;;;###autoload
(defun get-page-attribute (attribute)
  "Get ATTRIBUTE of this page.
The attribute may be found from various places, including meta-data and
apache directives.
These places are in general those found by webmaster:set-page-variables
in page-attributes.el."
  (interactive "sShow attribute: ")
  (let ((result (let* ((var-name-string (if (stringp attribute) attribute (symbol-name attribute)))
		       (var-name-symbol (if (symbolp attribute) attribute (intern attribute)))
		       (apache-var (get-page-head-apache-variable var-name-string)))
		  ;; (message "apache-var=%S" apache-var)
		  (if apache-var
		      apache-var
		    (let ((meta-var (get-page-head-tag "meta" "name" var-name-string)))
		      ;; (message "meta-var=%S" meta-var)
		      (if meta-var
			  (cdr (assoc "value" (cdr meta-var)))
			(let ((symbol (intern (concat "webmaster:page-" var-name-string))))
			  ;; (message "get-page-attribute made symbol %S bound=%S" symbol (boundp symbol))
			  (if (boundp symbol)
			      (symbol-value symbol)
			    nil))))))))
    (when (interactive-p) (message "Attribute %s has value %s" attribute result))
    result))

;;;###autoload
(defun set-page-attribute (attribute value)
  "Set ATTRIBUTE of this page to VALUE.
ATTRIBUTE may be a symbol or a string.
The new value will be written into the file when it is rebuilt by normalize-page-buffer.
The attribute may be stored in various places, including meta-data and
apache directives."
  (interactive "sSet attribute: 
sSet attribute %s to: ")
  (if (not (null value))    
      (let* ((var-name-string (if (stringp attribute) attribute (symbol-name attribute)))
	     (var-name-symbol (intern (substitute ?_ ?- var-name-string))))
	(set-page-head-tag "meta" "name" var-name-string (list (cons "value" value)))
	(apache-set var-name-symbol value)
	(set (intern (concat "webmaster:page-" var-name-string)) value)
	)))

(defun spread-page-attribute (attribute)
  "Write ATTRIBUTE into all the places in which it can be stored,
having found it from one of the such places."
  (set-page-attribute attribute (get-page-attribute attribute)))

(defun meld-page-info ()
  "Meld the various pieces of information held about the page."
  (interactive)
  ;; (webmaster:show-webmaster-page-variables (format "*%s before meld*" (buffer-name)))
  (mapcar
   'spread-page-attribute
   '(author
     keywords
     stylesheet
     description
     title
     logo
     site-homepage-url
     directory-name
     ))
  ;; (webmaster:show-webmaster-page-variables (format "*%s after meld*" (buffer-name)))
  )

(defvar current-html-page-template-name ""
  "The name of the template being used.
This variable may be used within templates.")

(defun normalize-page-buffer (template old-buffer new-buffer)
  "Using TEMPLATE and data from OLD-BUFFER construct a normalized equivalent page in NEW-BUFFER."
  (set-buffer old-buffer)
  (unless webmaster:page-url		; non-nil when webmaster:set-page-variables has been run in the buffer
    (webmaster:set-page-variables))
  (meld-page-info)
  (let ((items (mapcar 'eval template)))
    (set-buffer new-buffer)
    (let ((html-helper-build-new-buffer nil)
	  (html-helper-mode-hook nil))
      (html-helper-mode))
    (erase-buffer)
    (mapcar 'insert-string-or-tree items)))

;;;###autoload
(defun normalize-page-file (file template)
  "Normalize FILE using TEMPLATE.
The old file is saved, to be on the safe side."
  (interactive
   (list (read-file-name "Normalize page file: ")
	 (let ((completion-ignore-case t)) (completing-read "Template: " page-templates))))
   (let* ((full-file (expand-file-name file))
	  (shortfile (file-name-nondirectory full-file))
	  (old-name (expand-file-name (concat "old---" shortfile) (file-name-directory full-file))))
     (find-file file)
     (basic-save-buffer)
     (when (file-exists-p old-name) (delete-file old-name))
     (copy-file file old-name)
     (let* ((src (current-buffer))
	    (dest (get-buffer-create (format "*new-%s*" shortfile))))
       (set-buffer dest)
       (erase-buffer)
       (set-buffer src)
       (let ((current-html-page-template-name template))
	 (normalize-page-buffer (getpagetemplate template) src dest))
       (kill-buffer src)
       (delete-file full-file)
       (set-buffer dest)
       (write-file full-file)
       (normal-mode)
       )))

;;;###autoload
(defun normalize-this-page-file (template)
  "Do a normalize-page-file on the file in this buffer, using TEMPLATE."
  (interactive (list (let ((completion-ignore-case t)) (completing-read "Template: " page-templates))))
  (let ((filename (buffer-file-name)))
    (normalize-page-file filename template)
    (when (interactive-p)
      (find-file filename))))

;;;###autoload
(defun normalize-web-tree (tree template)
  "Normalize TREE using TEMPLATE.
Each old file is saved, to be on the safe side."
  (interactive
   (list (read-file-name "Normalize web tree: ")
	 (let ((completion-ignore-case t)) (completing-read "Template: " page-templates))))
  (webmaster:apply-throughout-tree tree
				   'normalize-this-page-file
				   (list template)))

(defun compare-buffers (a b)
  "Compare buffers A and B."
  ;; written for test-normalize-page
  (let ((case-fold-search nil))
    (set-buffer a)
    (let ((a-size (buffer-size)))
      (set-buffer b)
      (zerop (compare-buffer-substrings a 1 (1- a-size)
					b 1 (1- (buffer-size)))))))

(defun test-normalize-page (template)
  "Test a page, using TEMPLATE."
  (interactive (list (let ((completion-ignore-case t)) (completing-read "Template: " page-templates))))
  (let* ((stack-trace-on-error t)
	 (inbuffer (current-buffer))
	 (base-file-name (file-name-nondirectory (buffer-file-name)))
	 (outbuffnameformat (format "test-%s-%%d-%%d-%s" (substring template 0 (string-match "[^a-z]" template)) base-file-name)))
    (dotimes (i 3)
      (let ((outbuffname (format outbuffnameformat 0 i)))
	(when (get-buffer outbuffname) (kill-buffer outbuffname))
	(let ((outbuff (get-buffer-create outbuffname)))
	  (normalize-page-buffer (getpagetemplate template)
			  inbuffer
			  outbuff)
	  outbuff)))
    (let* ((same-0-1 (compare-buffers (format outbuffnameformat 0 0) (format outbuffnameformat 0 1)))
	   (same-1-2 (compare-buffers (format outbuffnameformat 0 1) (format outbuffnameformat 0 2))))
      (message "same-0-1=%S same-1-2=%S" same-0-1 same-1-2)
      (if (and same-0-1 same-1-2)
	  (progn
	    ;; now test for idempotence -- run one of the copies for a few generations
	    (message "finding F1 buffer")
	    (set-buffer (format outbuffnameformat 0 1))
	    (message "Got F1 buffer")
	    (dotimes (gen 3)
	      (message "Writing latest so far as gen-%d" gen)
	      (let ((gen-file-name (expand-file-name (format outbuffnameformat gen 1))))
		(write-file gen-file-name)
		(kill-buffer nil)
		;; make it set the buffer variables
		(message "Finding %s" gen-file-name)
		(find-file gen-file-name)
		(let ((inbuffname (buffer-name))
		      (outbuffname (format outbuffnameformat (1+ gen) 0)))
		  (when (get-buffer outbuffname)
		    (message "Killing pre-existing output buffer of same name (%s)" outbuffname)
		    (kill-buffer outbuffname))
		  (let ((outbuff (get-buffer-create outbuffname)))
		    (normalize-page-buffer (getpagetemplate template)
				    inbuffname
				    outbuff)
		    (let ((same (compare-buffers inbuffname outbuffname)))
		      (message "%s at generation %d" (if same "same" "different") gen)
		      )
		    (set-buffer outbuff))))
	      ))
	(error "Variation in the first generation")
	)
      )))

(defvar page-templates nil
  "The page templates declared so far, as an alist of title to varname.")

;;;###autoload
(defmacro defpagetemplate (varname title elements docstring)
  "Define a page template in VARNAME with TITLE and ELEMENTS and DOCSTRING."
  `(progn
     (defvar ,varname ,elements ,docstring)
     (setq ,varname ,elements)
     (let ((pair (assoc ,title page-templates)))
       (if pair
	   (rplacd pair ',varname)
	 (push (cons ,title ',varname) page-templates)))))

(defun getpagetemplate (title)
  "Return the page template registered with TITLE."
  (let ((name (cdr (assoc title page-templates))))
    (if name
	(symbol-value name)
      nil)))

(defpagetemplate page-template-plain-complete "Plain but complete"
  '(
    "<!DOCTYPE HTML PUBLIC \"" webmaster:page-doctype "\">\n"
    "<html>\n"
    "<head>\n"
    (webmaster:format-apache-directives webmaster:page-head-apache-directives)
    (webmaster:normalize-metadata webmaster:page-head-tags)
    "<title>" webmaster:page-title "</title>\n"
    "</head>\n"
    "<body " webmaster:page-body-options ">\n"
    "\n" webmaster:page-top-banner "\n"
    "<h1>" webmaster:page-first-heading "</h1>\n"
    (webmaster:page-body-content)
    webmaster:page-footer
    "</body>\n"
    "</html>\n"
    )
  "A page template that includes everything significant from the page,
but no boilerplate.")

(defpagetemplate page-template-with-thick-boilerplate "Ornate and complete"
  '(
    "<!DOCTYPE HTML PUBLIC \"" webmaster:page-doctype "\">\n"
    "<html>\n"
    "<head>\n"
    "<!-- begin apache directives -->\n"
    (webmaster:format-apache-directives webmaster:page-head-apache-directives)
    "<!-- end apache directives -->\n"
    "<!-- begin metadata -->\n"
    (webmaster:normalize-metadata webmaster:page-head-tags)
    "<!-- end metadata -->\n"
    "<title>" webmaster:page-title "</title>\n"
    "</head>\n"
    "<body " webmaster:page-body-options ">\n"
    "\n" webmaster:page-top-banner "\n"
    "<!-- begin main heading -->\n"
    "<h1>" webmaster:page-first-heading "</h1>\n"
    "<!-- end main heading -->\n"
    "<!-- begin main text -->\n"
    (webmaster:page-body-content)
    "<!-- end main text -->\n"
    "<!-- begin footer -->\n"
    webmaster:page-footer
    "<!-- end footer -->\n"
    "</body>\n"
    "</html>\n"
    )
  "A page template that includes everything significant from the page,
plus lots of structural information.")

(defpagetemplate page-template-all-inline
  "Inline, with comments separating mechanically produced material"
  '(
    "<!DOCTYPE HTML PUBLIC \"" webmaster:page-doctype "\">\n"
    "<html>\n"
    "<head>\n"
    "<!-- begin metadata -->\n"
    (webmaster:normalize-metadata
     webmaster:page-head-tags
     '(("meta")
       ("link")))
    "<!-- Template ``" current-html-page-template-name "'' used -->\n"
    "<!-- Template applied by " user-mail-address " at " (current-time-string) " -->\n"
    "<!-- end metadata -->\n"
    "<title>" webmaster:page-title "</title>\n"
    "</head>\n"
    "<body " webmaster:page-body-options ">\n"
    "\n" webmaster:page-top-banner "\n"
    "<!-- begin main heading -->\n"
    "<h1>" webmaster:page-first-heading "</h1>\n"
    "<!-- end main heading -->\n"
    "<!-- begin main text -->\n"
    (webmaster:page-body-content)
    "<!-- end main text -->\n"
    "<!-- begin footer -->\n"
    webmaster:page-footer
    "<!-- end footer -->\n"
    "</body>\n"
    "</html>\n"
    )
  "A page template that includes everything significant from the page,
plus lots of structural information.
Apache variables and SSIs are not used; everything is done inline.")

(defpagetemplate page-template-all-ssi
  "Using SSI a lot, with inline comments delimiting things."
  '(
    "<!-- begin apache directives -->\n"
    (webmaster:format-apache-directives webmaster:page-head-apache-directives)
    "<!-- end apache directives -->\n"
    "<!--#exec cgi=\"/perl/header\" -->\n"
    "<!-- begin main heading -->\n"
    "<h1>" webmaster:page-first-heading "</h1>\n"
    "<!-- end main heading -->\n"
    "<!-- begin main text -->\n"
    (webmaster:page-body-content)
    "<!-- end main text -->\n"
    "<!--#exec cgi=\"/perl/footer\" -->\n"
    )
  "A page template that includes everything significant from the page,
plus lots of structural information.
Just about everything is done through the apache SSI mechanism.")

(defpagetemplate page-template-all-ssi-even-first-heading
  "Using SSI a lot (even for the first heading), with inline comments delimiting things."
  '(
    "<!-- begin apache directives -->\n"
    (webmaster:format-apache-directives webmaster:page-head-apache-directives)
    "<!-- end apache directives -->\n"
    "<!--#exec cgi=\"/perl/header2\" -->\n"
    "<!-- begin main text -->\n"
    (webmaster:page-body-content)
    "<!-- end main text -->\n"
    "<!--#exec cgi=\"/perl/footer\" -->\n"
    )
  "A page template that includes everything significant from the page,
plus lots of structural information.
Just about everything is done through the apache SSI mechanism.")

;;; end of normalize-page.el
