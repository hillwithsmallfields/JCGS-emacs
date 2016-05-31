;;; c-header-to-shim.el --- make a shim from a C header

;; Copyright (C) 2016  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: convenience, c, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate a .c file from one or more .h files, that contains renamed
;; functions that call the original functions and print the arguments
;; and results.

;;; Code:

(defconst c-shim-function-pattern "\\([a-z0-9_]+\\)\\s-+\\([a-z0-9_]+\\)\\s-+\\([a-z0-9_]+\\)\\s-*(\\([a-z0-9_ ,*]*\\));"
  "Regexp for function prototypes.")

(defun c-shim-split-arg (arg)
  "Split ARG into its type and name."
  (save-match-data
    (if (string-match "^\\(.+?\\)\\([a-z0-9_]+\\)$" arg)
	(cons (match-string 1 arg) (match-string 2 arg))
      (cons "" arg))))

(defun c-shim-from-region (from to)
  "Return a string containing shims for FROM to TO."
  (save-excursion
    (goto-char from)
    (let ((results nil))
      (while (re-search-forward c-shim-function-pattern to t)
	(let ((scope (match-string-no-properties 1))
	      (type (match-string-no-properties 2))
	      (name (match-string-no-properties 3))
	      (args (mapcar 'c-shim-split-arg
			    (split-string (match-string-no-properties 4) ",\\s-+"))))
	  (push (list scope
		      type name
		      args)
		results)))
      (nreverse results))))

(defun c-shim-from-file (file)
  "Return a string containing shims for FILE."
  (save-excursion
    (find-file file)
    (save-restriction
      (widen)
      (c-shim-from-region (point-min) (point-max)))))

(defvar c-shim-prefix "trace__"
  "String to put on each generated function name.")

(defun c-shim-generate-top (function-descr)
  "Generate the top of a function from FUNCTION-DESCR."
  (let* ((scope (nth 0 function-descr))
	 (type (nth 1 function-descr))
	 (name (nth 2 function-descr))
	 (shim-name (concat c-shim-prefix name))
	 (args (nth 3 function-descr)))
    (insert scope " "
	    type " "
	    shim-name "("
	    (c-shim-decorated-args-string args)
	    ")")))

(defun c-shim-decorated-args-string (args)
  "Make a decorated args string from ARGS."
  (mapconcat (lambda (arg)
	       (concat (car arg) (cdr arg)))
	     args ", "))

(defun c-shim-bare-args-string (args)
  "Make a bare args string from ARGS."
  (mapconcat 'cdr args ", "))

(defun c-shim-format (arg)
  "Return a formatting directive to suit ARG."
  (let ((type (car arg)))
    (cond
     ((string-match "u?int[0-9]* " type) "%#x")
     ((string= "char * " type) "\\\"%s\\\"")
     ((string-match "\\*$" type) "%p")
     (t "%#x"))))

(defun c-shim-generate-function (function-descr)
  "Insert a function for FUNCTION-DESCR."
  (c-shim-generate-top function-descr)
  (let*  ((scope (nth 0 function-descr))
	  (type (nth 1 function-descr))
	  (is-void (string= type "void"))
	  (name (nth 2 function-descr))
	  (shim-name (concat c-shim-prefix name))
	  (args (nth 3 function-descr))
	  (bare-args-string (c-shim-bare-args-string args)))
    (insert "\n{\n")
    (unless is-void
      (insert "  " type " result;\n"))
    (insert "  fprintf(stderr, \"" name "(")
    (insert (mapconcat 'c-shim-format
		       args
		       ", "))
    (insert ")\\n\", " bare-args-string ");\n  ")
    (unless is-void
      (insert "result = "))
    (insert name "(" bare-args-string ");\n")
    (unless is-void
      (insert "  fprintf(stderr, \"result of " name " is %x\\n\", result);\n")
      (insert "  return result;\n"))
    (insert "}\n\n")))

(defun c-shim-generate-prototype (function-descr)
  "Insert a prototype for FUNCTION-DESCR."
  (c-shim-generate-top function-descr)
  (insert ";\n"))

(defun c-shim-includes (headers)
  "Insert the includes for HEADERS."
  (when headers
    (insert "#include <"
	    (mapconcat 'file-name-nondirectory headers ">\n#include <")
	    ">\n\n")))

(defun c-shim-include-dirs (headers)
  "Return the set of directories of HEADERS."
  (let ((dirs nil))
    (dolist (header headers)
      (pushnew (file-name-directory header)
	       dirs
	       :test 'string=))
    dirs))

(defun c-shim-from-files (shim-source-file shim-header-file &rest headers)
  "Make a SHIM-SOURCE-FILE and SHIM-HEADER-FILE from HEADERS.
Interactively, prompts for a directory to use the headers from."
  (interactive
   (let ((shim-source-file (read-file-name "Make shim source file: "))
	 (shim-header-file (read-file-name "Make shim header file: "))
	 (header-directory (read-directory-name "Make shim from headers in directory: ")))
     (cons shim-source-file shim-header-file
	   (directory-files header-directory t "\\.h$"))))
  (save-window-excursion
    (let ((functions (apply 'append
			    (mapcar 'c-shim-from-file
				    headers)))
	  (edit-function-name (format "shim-for-%s"
				      (file-name-sans-extension
				       (file-name-nondirectory
					shim-source-file)))))
      (find-file shim-source-file)
      (erase-buffer)
      (insert "/* Generated at " (current-time-string) " */\n")
      (c-shim-includes headers)
      (mapcar 'c-shim-generate-function
	      functions)
      (basic-save-buffer)
      (find-file shim-header-file)
      (erase-buffer)
      (insert "/* Generated at " (current-time-string) " */\n")
      (c-shim-includes headers)
      (mapcar 'c-shim-generate-prototype
	      functions)
      (basic-save-buffer)
      (fset (intern edit-function-name )
	    `(lambda ()
	       (interactive)
	       (dolist (name ',(mapcar (lambda (function) (nth 2 function))
				       functions))
		 (let ((pattern (format "\\<\\(%s\\)\\>\\s-*(" name))
		       (decorated (concat c-shim-prefix name)))
		   (while (re-search-forward pattern (point-max) t)
		     (replace-match decorated t t nil 1))
		   (goto-char (point-min))))))
      (let ((source (file-name-nondirectory
		     shim-source-file)))
	(message "gcc -c -o %s.o -I %s %s; defined %s"
		 (file-name-sans-extension source)
		 (mapconcat 'identity (c-shim-include-dirs headers) " -I ")
		 source
		 edit-function-name)))))

(defun c-shim-do-it ()
  "Make my usual C shims."
  (interactive)
  (let ((header-dir  "/arm/tools/prodesign/proFPGA/2015C/include/"))
    (apply 'c-shim-from-files "/work/johstu01/c-shims/shims.c"
	   "/work/johstu01/c-shims/shims.h"
	   (mapcar (lambda (header)
		     (expand-file-name header header-dir))
		   '("mmi64.h"
		     "mmi64_module_axi_master.h"
		     "mmi64_module_regif.h"
		     "profpga.h"
		     "profpga_error.h"
		     "profpga_acm.h")))))

(provide 'c-header-to-shim)
;;; c-header-to-shim.el ends here
