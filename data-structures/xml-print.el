;;;; xml-print.el -- output XML structures
;;; Time-stamp: <2007-04-26 11:14:31 jcgs>

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

;; based on xml-debug-print and xml-debug-print-internal in the Emacs distribution

(require 'xml)

(defun xml-print (xml &optional stream)
  "Outputs the XML tree in the current buffer.
The first line indented with INDENT-STRING.
With optional STREAM, output to that stream instead."
  (dolist (x xml)
    (xml-print-internal x "" stream)))

(defun xml-print-insert (stream &rest args)
  "To STREAM, output the remaining args.
If STREAM is nil, insert in current buffer."
  (if stream
      (mapcar (function
	     (lambda (arg)
	       (princ arg stream)))
	    args)
    (apply 'insert args)))

(defun xml-print-internal (xml &optional indent-string stream)
  "Outputs the XML tree in the current buffer.
The first line indented with INDENT-STRING if given.
With optional STREAM, output to that stream instead."
  (let ((tree xml)
	last
	attlist)
    (unless indent-string
      (setq indent-string ""))

    (xml-print-insert stream indent-string "<" (symbol-name (xml-node-name tree)))

    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (dolist (att attlist)
      (xml-print-insert stream " " (symbol-name (car att)) "=\"" (cdr att) "\""))

    (setq tree (xml-node-children tree))

    (if (null tree)
	(xml-print-insert stream "/>")
      (xml-print-insert stream ">")
      ;;  output the children
      (dolist (branch tree)
	(cond
	 ((listp branch)
	  (xml-print-insert stream "\n")
	  (setq last 'sub-tree)
	  (xml-print-internal branch (concat indent-string "  ") stream))
	 ((stringp branch)
	  (setq last 'string)
	  (xml-print-insert stream branch))
	 (t
	  (error "Invalid XML tree"))))
      (when (eq last 'sub-tree)
	(xml-print-insert stream "\n" indent-string))
      (xml-print-insert stream "</" (symbol-name (xml-node-name xml)) ">"))))

(provide 'xml-print)

;;; end of xml-print.el
