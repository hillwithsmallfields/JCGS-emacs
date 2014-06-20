;;;; webmastering.el -- set up autoloads for webmastering software
;;; Time-stamp: <2001-01-04 09:14:47 jcgs>


;;;### (autoloads (defpagetemplate normalize-web-tree normalize-this-page-file
;;;;;;  normalize-page-file set-page-attribute get-page-attribute)
;;;;;;  "normalize-page" "normalize-page.el" (14931 26769))
;;; Generated autoloads from normalize-page.el

(autoload (quote get-page-attribute) "normalize-page" "\
Get ATTRIBUTE of this page.
The attribute may be found from various places, including meta-data and
apache directives.
These places are in general those found by webmaster:set-page-variables
in page-attributes.el." t nil)

(autoload (quote set-page-attribute) "normalize-page" "\
Set ATTRIBUTE of this page to VALUE.
ATTRIBUTE may be a symbol or a string.
The new value will be written into the file when it is rebuilt by normalize-page-buffer.
The attribute may be stored in various places, including meta-data and
apache directives." t nil)

(autoload (quote normalize-page-file) "normalize-page" "\
Normalize FILE using TEMPLATE.
The old file is saved, to be on the safe side." t nil)

(autoload (quote normalize-this-page-file) "normalize-page" "\
Do a normalize-page-file on the file in this buffer, using TEMPLATE." t nil)

(autoload (quote normalize-web-tree) "normalize-page" "\
Normalize TREE using TEMPLATE.
Each old file is saved, to be on the safe side." t nil)

(autoload (quote defpagetemplate) "normalize-page" "\
Define a page template in VARNAME with TITLE and ELEMENTS and DOCSTRING." nil (quote macro))

;;;***

;;;### (autoloads (webmaster:rewrite-regexp-reference-throughout-tree
;;;;;;  webmaster:rewrite-regexp-reference-throughout-page webmaster:rewrite-regexp-reference
;;;;;;  webmaster:rewrite-reference-throughout-tree webmaster:rewrite-reference-throughout-page
;;;;;;  webmaster:relocate-page) "relocate-page" "relocate-page.el"
;;;;;;  (14914 4599))
;;; Generated autoloads from relocate-page.el

(autoload (quote webmaster:relocate-page) "relocate-page" "\
Relocate the current page into WHITHER." t nil)

(autoload (quote webmaster:rewrite-reference-throughout-page) "relocate-page" "\
Change all references to OLD-URL in the page to NEW-URL." t nil)

(autoload (quote webmaster:rewrite-reference-throughout-tree) "relocate-page" "\
Throughout TREE rewrite all references to OLD-URL to be NEW-URL.
This is done throughout all .html and .shtml files in the tree.
If given a file for TREE, do the rewriting of that file." t nil)

(autoload (quote webmaster:rewrite-regexp-reference) "relocate-page" "\
Expand REFERENCE into an absolute URL and if it matches OLD-URL-PATTERN return a relativized url based on NEW-URL-TEMPLATE else nil.
NEW-URL-TEMPLATE is processed using replace-match (which see)." nil nil)

(autoload (quote webmaster:rewrite-regexp-reference-throughout-page) "relocate-page" "\
Change all references to OLD-URL-PATTERN in the page to NEW-URL-TEMPLATE, using replace-match (which see)." t nil)

(autoload (quote webmaster:rewrite-regexp-reference-throughout-tree) "relocate-page" "\
Throughout TREE rewrite all references to OLD-URL-PATTERN to be based on NEW-URL-PATTERN using replace-match (which see).
This is done throughout all .html and .shtml files in the tree.
If given a file for TREE, do the rewriting of that file." t nil)

;;;***
