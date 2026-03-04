;;;; new-files.el -- set up auto-insert to suit me
;;; Time-stamp: <2007-06-27 15:05:01 jcgs>

(require 'autoinsert)

(auto-insert-mode 1)

(defun remove-auto-insert (condition)
  "Remove the current actions for CONDITION."
  ;; unfortunately this is not provided in autoinsert.el
  ;; it seems that only the first match for a condition is meant to be used,
  ;; but it didn't look like that in practice
  (let ((rest auto-insert-alist))
    (while rest
      (let ((this (cadr rest)))
	(if (equal (car this)
		   condition)
	    (progn
	      (rplacd rest
		      (cddr rest))))
	(setq rest (cdr rest)))))
  (if (equal (caar auto-insert-alist)
	     condition)
      (rplacd auto-insert-alist (cddr auto-insert-alist))))

(defun insert-file-variables ()
  "Make a file variables entry in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n"
	    comment-start " Local" " variables: " comment-end "\n"
	    comment-start " : " comment-end "\n"
	    comment-start " End: " comment-end "\n")))

;; (setq auto-insert-directory "/nfs/vivaldi/u/ldisk5/john/insert/") last changed 95-06!
(setq auto-insert-directory "~/common/insert/")

(defvar gpl-string "
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 3 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
")

(when nil
  (remove-auto-insert '("\\.el\\'" . "Emacs Lisp header"))

  (define-auto-insert '("\\.el\\'" . "Emacs Lisp header")
    '("Emacs lisp mode"
      ";;;; " (file-name-nondirectory (buffer-file-name)) " -- " (read-string "Description: ")
      "\n;;; Time-stamp: <>\n"
      (if (yes-or-no-p "Include GPL? ") gpl-string)
      "\n"
      _
      "\n\n(provide '" (let* ((name (file-name-nondirectory (buffer-file-name)))
			      (el-pos (string-match "\\.el" name)))
			 (substring name 0 el-pos))
      ")\n"
      "\n"
      ";;; end of " (file-name-nondirectory (buffer-file-name)) "\n")))

(define-auto-insert 'latex-mode
  '( ;; should try to offer completing read for these
    "options, RET: "
    "\\documentclass[" str & ?\] | -1
    ?{ (read-string "class: ") "}\n"
    ("package, %s: "
     "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
    "\\title{" (read-string "Title: ") "}\n"
    "\\author{" (read-string "Author: " (let* ((name (user-full-name)))
					  (if (string-match "^ *\\([^ ]+\\) *$" name)
					      (match-string 1 name)
					    name)))
    "}\n"
    _ "\n\\begin{document}\n" _
    "\n\\end{document}\n"))

(define-auto-insert "\\.xmlex$"
  '("XmLex mode"
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
    "<!DOCTYPE TermBridge SYSTEM 'XmLexForBilingualDictionaries_V00.dtd'>\n"
    "<?xml:stylesheet type=\"text/xsl\" href=\"XmLex_V00Displayer.xsl\"?>\n"
    "<Dictionary>\n"
    _
    "</Dictionary>\n"))

;;; end of new-files.el
