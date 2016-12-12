;;;; xml.el -- patches to xml.el in the distribution
;;; Time-stamp: <2007-05-20 19:50:58 jcgs>

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

(provide 'xml)

;; from GNU Emacs 21.3.1 (i386-mingw-nt5.1.2600) of 2004-03-10 on NYAUMO
;; I have added some more information on errors
(defun xml-parse-tag0 (end &optional parse-dtd)
  "Parse the tag that is just in front of point.
The end tag must be found before the position END in the current buffer.
If PARSE-DTD is non-nil, the DTD of the document, if any, is parsed and
returned as the first element in the list.
Returns one of:
   - a list : the matching node
   - nil    : the point is not looking at a tag.
   - a cons cell: the first element is the DTD, the second is the node"
  (cond
   ;; Processing instructions (like the <?xml version="1.0"?> tag at the
   ;; beginning of a document)
   ((looking-at "<\\?")
    (search-forward "?>" end)
    (skip-chars-forward " \t\n")
    (xml-parse-tag end))
   ;;  Character data (CDATA) sections, in which no tag should be interpreted
   ((looking-at "<!\\[CDATA\\[")
    (let ((pos (match-end 0)))
      (unless (search-forward "]]>" end t)
	(error "CDATA section does not end anywhere in the document"))
      (buffer-substring-no-properties pos (match-beginning 0))))
   ;;  DTD for the document
   ((looking-at "<!DOCTYPE")
    (let (dtd)
      (if parse-dtd
	  (set 'dtd (xml-parse-dtd end))
	(xml-skip-dtd end))
      (skip-chars-forward " \t\n")
      (if dtd
	  (cons dtd (xml-parse-tag end))
	(xml-parse-tag end))))
   ;;  skip comments
   ((looking-at "<!--")
    (search-forward "-->" end)
    (skip-chars-forward " \t\n")
    (xml-parse-tag end))
   ;;  end tag
   ((looking-at "</")
    '())
   ;;  opening tag
   ((looking-at "<\\([^/> \t\n]+\\)")
    (let* ((node-name (match-string 1))
	   (children (list (intern node-name)))
	   (case-fold-search nil) ;; XML is case-sensitive
	   pos)
      (goto-char (match-end 1))

      ;; parses the attribute list
      (set 'children (append children (list (xml-parse-attlist end))))

      ;; is this an empty element ?
      (if (looking-at "/>")
	  (progn
	    (forward-char 2)
	    (skip-chars-forward " \t\n")
	    (append children '("")))

	;; is this a valid start tag ?
	(if (eq (char-after) ?>)
	    (progn
	      (forward-char 1)
	      (skip-chars-forward " \t\n")
	      ;;  Now check that we have the right end-tag. Note that this one might
	      ;;  contain spaces after the tag name
	      (while (not (looking-at (concat "</" node-name "[ \t\n]*>")))
		(cond
		 ((looking-at "</")
		  (error (concat
			  "XML: invalid syntax -- invalid end tag (expecting "
			  node-name
			  ") at pos " (number-to-string (point)))))
		 ((= (char-after) ?<)
		  (set 'children (append children (list (xml-parse-tag end)))))
		 (t
		  (set 'pos (point))
		  (search-forward "<" end)
		  (forward-char -1)
		  (let ((string (buffer-substring-no-properties pos (point)))
			(pos 0))
		    
		    ;; Clean up the string (no newline characters)
		    ;; Not done, since as per XML specifications, the XML processor
		    ;; should always pass the whole string to the application.
		    ;; 	    (while (string-match "\\s +" string pos)
		    ;; 	      (set 'string (replace-match " " t t string))
		    ;; 	      (set 'pos (1+ (match-beginning 0))))
		    
		    (set 'children (append children
					   (list (xml-substitute-special string))))))))
	      (goto-char (match-end 0))
	      (skip-chars-forward " \t\n")
	      (if (> (point) end)
		  (error "XML: End tag for %s not found before end of region"
			 node-name))
	      children
	      )

	  ;;  This was an invalid start tag
	  (error "XML: Invalid attribute list at %d in %s" (point) (buffer-file-name))
	  ))))
   (t ;; This is not a tag.
    (error "XML: Invalid character"))
   ))


;;; end of xml.el
