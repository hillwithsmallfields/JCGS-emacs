;;;; insert-construct.el -- insert programming constructs in various languages
;;; Time-stamp: <2004-12-04 13:14:40 jcgs>

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

(provide 'insert-construct)

(require 'tempo)

(defun insert-construct (category construct &optional on-region)
  "Insert a CATEGORY CONSTRUCT in the current language."
  (interactive "*SInsert construct of category: 
SInsert %s: 
P")
  (let ((inserter (intern (format "tempo-template-%s-%s-%s" major-mode category construct))))
    (if (fboundp inserter)
	(let ((tempo-interactive t))
	  (funcall inserter on-region))
      (error "%s not defined" inserter))))

(defun define-construct (definition)
 "Define a construct, using DEFINITION, which is a list of the arguments to define-construct-1."
 ;; (message "define-construct on %d:%s" (length definition) definition)
 (apply 'define-construct-1 definition))

(defun define-construct-1 (language type name template &optional tag)
  "Define a construct, in LANGUAGE, of TYPE, called NAME, to insert TEMPLATE with optional TAG.
LANGUAGE is a symbol that will have -mode appended to it to make the name of the mode in which
to use this template. Definitions made into 'lisp are repeated into 'emacs-lisp, but not vice-versa.
TYPE classifies the NAMEd templates. The NAMEs are the nearest lisp names."
  (if (eq language 'common)
      (dolist (language '(c perl))
	(define-construct-1 language type name template tag)))
  (progn
    (tempo-define-template
     (format "%s-mode-%s-%s" language type name)
     template
     tag)
    (when (eq language 'lisp)
      (define-construct-1 'emacs-lisp type name template tag))
    (when (eq language 'emacs-lisp)
      (define-construct-1 'lisp-interaction type name template tag))))

(mapcar
 'define-construct
 '(
   (lisp definition defun
	 (& n "(defun " (p "Function name to define: ") " (" (p "Argument list: ") ")" n>
	    "\"" (p "Documentation string: ") "\"" n>
	    r> ")" n))
   (emacs-lisp definition defcommand
	       (& n "(defun " (p "Function name to define: ") " (" (p "Argument list: ") ")" n>
		  "\"" (p "Documentation string: ") "\"" n>
		  "(interactive \"" (p "Interactive control string: ") "\")" n>
		  ")" n))
   (lisp definition defmacro
	 (& n "(defmacro " (p "Macro name to define: ") " (" (p "Argument list: ") ")" n>
	    "\"" (p "Documentation string: ") "\"" n>
	    r> ")" n))
   (lisp definition require (& n "(require '" (p "Feature required: ") ")" n))
   (lisp definition provide (& n "(provide '" (p "Feature provided: ") ")" n))
   (lisp definition defvar
	 (& n "(defvar " (p "Variable name to define: ") " " (p "Initial value: ") n> "\"" (p "Documentation string: ") "\")" n))
   (lisp definition defconst
	 (& n "(defconst " (p "Constant name to define: ") " " (p "Initial value: ") n> "\"" (p "Documentation string: ") "\")" n))
   (lisp definition defstruct
	 (& n "(defstruct " (p "Name of structure type: ") n> "\"" (p "Documentation string: ") "\"" n ")"))

   (lisp statement when
	 (& > "(when " p n>
	    r ")"))
   (lisp statement if
	 (& > "(if " p n>
	    r n>
	    p ")"))
   (lisp statement unless
	 (& > "(unless " p n>
	    r ")"))
   (lisp statement setq
	 (& > "(setq " (p "Variable name: ") " " r ")"))
   (emacs-lisp statement while
	       (& > "(while " p n> r ")"))
   (lisp statement cond
	 (& > "(cond" n> "(" p ")" n> "(t (" p " " p ")))"))
   (lisp statement further-cond
	 (& > "(" p " " p ")"))
   (lisp declaration let		; I'd like to have a version that takes the current expression and asks for a scope at which to name it
	 (& > "(let ((" (p "Variable name: ") p "))" n> ")"))
   (lisp statement progn
	 (& > "(progn" n> r> ")"))
   (emacs-lisp statement save-excursion
	       (& > "(save-excursion" n> r> ")"))
   (emacs-lisp statement save-window-excursion
	       (& > "(save-window-excursion" n> r> ")"))
   (lisp statement funcall
	 (> "(" p ")"))
   (lisp statement proccall
	 (& > "(" p ")"))
   (lisp statement catch
	 (& > "(catch " (p "Tag to catch: ") n> r> ")"))
   (lisp statement throw
	 (& > "(throw " (p "Tag to throw: ") n> r> ")"))
   (lisp statement dolist
	 (& > "(dolist (" (p "Control variable: ") " " p ")" n> r> ")"))
   (lisp statement dotimes
	 (& > "(dotimes (" (p "Control variable: ") " " p ")" n> r> ")"))

					;    (lisp declaration let
					; 	 (& > "(let ((" p "))" n>
					; 	    r> ")"))
   (lisp declaration let*
	 (& > "(let* ((" p ")" n> "())" n>
	    r> ")"))
   (lisp declaration further-let
	 (& > "(" (p "Variable name: ") " " p ")"))

   (lisp op-predicate zerop ("(zerop " r ")"))
   (lisp op-predicate null ("(null " r ")"))
   (lisp op-comparison = ("(= " r " " p ")"))
   (lisp op-comparison /= ("(/= " r " " p ")"))
   (lisp op-comparison < ("(< " r " " p ")"))
   (lisp op-comparison <= ("(<= " r " " p ")"))
   (lisp op-comparison >= ("(>= " r " " p ")"))
   (lisp op-comparison > ("(> " r " " p ")"))
   (lisp op-comparison eq ("(eq " p " " p ")"))
   (lisp op-comparison equal ("(equal " p " " p ")"))
   (lisp op-comparison ne ("(ne " p " " p ")"))
         
   (lisp op-arithmetic + ("(+ " r " " p ")"))
   (lisp op-arithmetic - ("(- " r " " p ")"))
   (lisp op-arithmetic * ("(* " r " " p ")"))
   (lisp op-arithmetic / ("(/ " r " " p ")"))
   (lisp op-arithmetic % ("(% " r " " p ")"))

   (lisp op-boolean or ("(or " r " " p ")"))
   (lisp op-boolean and ("(and " r " " p ")"))
   (lisp op-boolean not ("(not " r ")"))

   (lisp op-bitwise or ("(or " r " " p ")"))
   (lisp op-bitwise and ("(and " r " " p ")"))
   (lisp op-bitwise not ("(not " r ")"))
   (lisp op-bitwise left-shift ("(asl " r " " p ")"))
   (lisp op-bitwise right-shift ("(asr " r " " p ")"))

   (lisp op-functional lambda ("(function" n> "(lambda (" p ")" n>  "))"))

   (emacs-lisp library-function output ("(insert " p ")"))
   (emacs-lisp library-function error-output ("(message " p ")"))

   (perl definition defun
	 (& n "sub " (p "Function name to define: ") " {"
	    n "}" n))
   (perl definition require
	 (& n "use " (p "Package used: ") ";"))
   (perl definition provide
	 (& n "package " (p "Package defined: ") ";"))
   (perl statement when
	 (& > "if (" p ") {" n>
	    r "}"))
   (perl statement if
	 (& > "if (" p ") {" n>
	    r "} else {"n>
	    p "}"))
   (perl statement unless
	 (& > "unless (" p ") {" n>
	    r "}"))
   (perl statement setq
	 (& > (p "Variable name: ") " = " r ";"))
   (perl statement while
	       (& > "while (" p ") {" n> r "}"))

   (perl library-function output ("print \"" p "\";"))
   (perl library-function error-output ("warn \"" p "\";"))

   (postscript statement when
	 (& > "{" n> p n>
	    r "} " p "if" n>))
   (postscript statement if
	 (& > "{" r n>
	    "} {" p n>
	    "}" p n>))
   (postscript library-function output (" print"))

   (c definition defun
      (& (p "Function class (static, extern): ") " " (p "Function result type: ") "(" (p "Argument list: ") ")" n
	 "{" n
	 "}" n))
   ))

(defun make-library-function-template (name n-args language)
  "Make a tempo template for calling NAME with N-ARGS in LANGUAGE."
  (cond
   ((memq language '(lisp emacs-lisp lisp-interaction))
    (concatenate 'list (list (format "(%s" name)) (apply 'concatenate 'list (make-list n-args '(" " p))) (list ")")))
   ((memq language '(c c++ java perl))
    (concatenate 'list (list (format "%s(" name)) (apply 'concatenate 'list (make-list (1- n-args) '(p ", "))) (list 'p ")")))
   ((eq language 'postscript)
    (concatenate 'list (apply 'concatenate 'list (make-list (1- n-args) '(p " "))) (list name)))))

(defun define-library-function (fn-desc)
  "Make and use a library function definition for FN-DESC
which should be a list (function-name n-args language)."
  (let ((name (first fn-desc))
	(n-args (second fn-desc))
	(language (third fn-desc)))
    (define-construct-1 language 'library-function name
      (make-library-function-template name n-args language))))

(mapcar
 'define-library-function
 '((cons 2 lisp)
   (car 1 lisp)
   (cdr 1 lisp)
   (assoc 2 lisp)
   (push 2 lisp)
   (pop 1 lisp)
   (nreverse 1 lisp)
   (mapcar 2 lisp)
   (apply 1 lisp)
   (funcall 1 lisp)
   (point 0 emacs-lisp)
   (point-min 0 emacs-lisp)
   (point-max 0 emacs-lisp)
   (goto-char 1 emacs-lisp)
   (search-forward 3 emacs-lisp)
   (search-backward 3 emacs-lisp)
   (re-search-forward 3 emacs-lisp)
   (re-search-backward 3 emacs-lisp)
   
))

;;;; end of insert-construct.el
