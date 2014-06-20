;;;; xml-find.el -- find parts of an XML tree
;;; Time-stamp: <2007-05-01 10:14:45 jcgs>

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

(defun assoc-and-match-attributes (outer-key outer-alist matcher &optional prefix)
  "Find an element with OUTER-KEY as its car in OUTER-ALIST,
and with a cdr in which MATCHER occurs.
If MATCHER is a dotted pair (its cdr is not a list) require it
to be a member of the cdr of the resulting pair."
  (when xml-find-debug
    (message "%sassoc-and-match-attributes key=%S list=%S matcher=%S" prefix outer-key outer-key matcher))
  (let ((result
	 (catch 'found
	   (while outer-alist
	     (let ((pair (car outer-alist)))
	       ;; (message "%s. Comparing %S for %S and %S" prefix pair outer-key matcher)
	       (when (and (consp pair)	; make remaining tests safe
			  (equal (car pair) outer-key) ; main assoc test
			  ;; (progn (message "%s. passed outer test, trying %S on %S, (cdr pair)=%S (cadr pair)=%S" prefix matcher pair (cdr pair) (cadr pair)) t)
			  (or (null matcher)
			      (and (consp (cdr pair))
				   (consp (cadr pair))
				   (member matcher (cadr pair)))))
		 (throw 'found pair)))
	     (setq outer-alist (cdr outer-alist)))
	   nil)))
    (when xml-find-debug
      (message "%sassoc-and-match-attributes returning %S" prefix result))
    result
    ))

(defun assoc-and-match-subtree (outer-key outer-alist subtree-spec &optional prefix)
  "Find an element with OUTER-KEY as its car in OUTER-ALIST,
and with a cddr in which SUBTREE-SPEC matches according to xml-find."
  (when xml-find-debug
    (message "%sassoc-and-match-subtree key=%S list=%S subtree-spec=%S" prefix outer-key outer-key subtree-spec))
  (let ((result
	 (catch 'found
	   (while outer-alist
	     (let ((pair (car outer-alist)))
	       (when (and (consp pair)	; make remaining tests safe
			  (equal (car pair) outer-key) ; main assoc test
			  (or (null subtree-spec)
			      (xml-find subtree-spec pair nil ",   ")))
		 (throw 'found pair)))
	     (setq outer-alist (cdr outer-alist)))
	   nil)))
    (when xml-find-debug
      (message "%sassoc-and-match-subtree returning %S" prefix result))
    result))

(defvar xml-find-debug nil)

(defun xml-assoc (key list prefix)
  (let ((result (assoc key list)))
    (when xml-find-debug
      (message "%s(assoc %S %S) --> %S" prefix key list result))
    result))

(defun xml-find-convert-to-subtree (elements &optional prefix)
  "Convert ELEMENTS to a subtree."
  (when xml-find-debug
    (message "%sConverting %S to subtree" prefix elements))
  (let ((result (if elements
		    (let ((element (car elements)))
		      (cond
		       ((symbolp element)
			(list (cons element
				    (cons nil
					  (xml-find-convert-to-subtree (cdr elements)
								       (concat prefix ".  "))))))
		       ((and (consp element)
			     (symbolp (cadr element)))
			(list (cons (car element)
				    (cons (list (cdr element))
					  (xml-find-convert-to-subtree (cdr elements)
								       (concat prefix ".   "))))))
		       ((and (consp element)
			     (stringp (cadr element)))
			(list (list (car element) nil (cadr element))))))
		  nil)))
    (when xml-find-debug
      (message "%sResulting subtree is %S" prefix result))
    result))

(defun xml-find (elements tree-node &optional create prefix)
  "Follow the path ELEMENTS in TREE-NODE, taking the first entry that matches.
Each of ELEMENTS is either
* a name (symbol)
* a cons of a name and a cons of a key and value,
  in which case that inner cons must match an attribute of the entry.
* a vector containing a name and a list of the same form as ELEMENTS,
  specifying that a tree matching in the list must be present as
  a child of the node with the given name
The TREE-NODE is in the format returned by xml-parse-file etc.
The result is the pair of that name and the corresponding data.
If optional third argument is non-nil, elements are added as necessary.
The optional fourth argument is a string to prefix to debugging messages."
  (when xml-find-debug
    (if (null prefix) (setq prefix ""))
    (message "%sxml-find %S %S %S" prefix elements tree-node create))
  (let* ((print-level t) (print-length t)
	 (subtrees (cddr tree-node))
	 (name (car elements))
	 (pair (cond
		((consp name)
		 (assoc-and-match-attributes (car name)
					     subtrees
					     (cdr name)
					     (concat prefix ".  ")))
		((symbolp name)
		 (xml-assoc name (cddr tree-node) (concat prefix "'  ")))
		((vectorp name)
		 (assoc-and-match-subtree (aref name 0)
					  subtrees
					  (aref name 1)
					  (concat prefix ".  ")))
		(t (error "Bad name %S for xml-find" name)))))
    (when xml-find-debug
      (message "%sxml-find pair is %S" prefix pair))
    (when (and create
	       (null pair))
      ;; (message "xml-find creating for name %S" name)
      (setq pair (cond
		  ((consp name)
		   (cons (car name) (list (cdr name))))
		  ((symbolp name)
		   (list name nil))
		  ((vectorp name)
		   (cons (aref name 0)
			 (cons nil
			       (xml-find-convert-to-subtree (aref name 1))))
		   )
		  (t (error "Bad name %S for xml-find" name)))
	    subtrees (cons pair subtrees))
      (rplacd (cdr tree-node) subtrees)
      (when xml-find-debug
	(message "%stree-node is now %S" prefix tree-node)))
    (let ((result
	   (if (and pair
		    (cdr elements))
	       (progn
		 (when xml-find-debug
		   (message "%sxml-find recursing looking for %S in %S" prefix (cdr elements) pair))
		 (xml-find (cdr elements)
			   pair
			   create
			   (concat prefix "|  ")))
	     pair)))
      (when xml-find-debug
	(message "%sxml-find result %S" prefix result))
      result)))

(defun test-xml-find-create ()
  (interactive)
  (require 'xml-print)
  (with-current-buffer "*Messages*" (erase-buffer))
  (let ((stack-trace-on-error t)
	(xml-find-debug t)
	(entry (list 'DictionaryEntry nil)))
    (with-output-to-temp-buffer "*xml-find tests*"
      (mapcar (lambda (elements)
		(princ (format "Looking for %S\n" elements))
		(princ (format "  tree now %S\n" entry))
		(princ (format "  got %S\n" (xml-find elements entry t)))
		(princ (format "  tree now %S\n" entry))
		(xml-print-internal entry "  " standard-output)
		(princ "\n\n"))
	      '((HeadwordCtn)
		(HeadwordCtn Headword)
		(HeadwordCtn VariantCtn Variant)
		([HomographGrp ((PartOfSpeech value . "noun"))] SenseBlock SenseGroup Translation)
		([HomographGrp ((PartOfSpeech value . "noun"))] SenseBlock [SenseGroup ((SenseIndicator "Sensible"))] Translation)
		([HomographGrp ((PartOfSpeech value . "noun"))] SenseBlock [SenseGroup ((SenseIndicator "Silly"))] Translation)
		([HomographGrp ((PartOfSpeech value . "adjective"))] SenseBlock SenseGroup Translation)
		([WorkingNoteCtn ((WorkingNote type . "original-text"))] WorkingNote)
		)))))

(defun test-xml-find-dictionary ()
  (interactive)
  (let ((entry '(DictionaryEntry
		 ((identifier . "abha"))
		 (HeadwordCtn
		  nil
		  (Headword nil "abha")
		  (AlternativeScripting
		   ((scriptingScheme . "notPredefined")) "aḃa")
		  (PartOfSpeech
		   ((value . "noun")) "")
		  (GrammaticalGender
		   ((value . "feminine")) "")
		  (InflectionBlock
		   nil
		   (GrammaticalNumber ((value . "singular")) "")
		   (InflectionCtn
		    nil
		    (Inflection nil "abhann")
		    (AlternativeScripting
		     ((scriptingScheme . "notPredefined"))
		     "aḃann")
		    (Case ((value . "genitive")) ""))
		   (InflectionCtn
		    nil
		    (Inflection nil "abainn")
		    (Case
		     ((value . "dative")) "")))
		  (InflectionBlock
		   nil
		   (GrammaticalNumber ((value . "plural")) "")
		   (InflectionCtn
		    nil
		    (Inflection nil "aibhne")
		    (AlternativeScripting
		     ((scriptingScheme . "notPredefined")) "Aiḃne")
		    (Case
		     ((value . "nominative")) ""))
		   (InflectionCtn
		    nil
		    (Inflection nil "abhann")
		    (AlternativeScripting
		     ((scriptingScheme . "notPredefined")) "aḃann")
		    (Case
		     ((value . "genitive")) ""))))
		 (SenseBlock
		  nil
		  (SenseGrp
		   nil
		   (Translation nil "river")
		   (Translation nil "stream")))
		 (CompositionalPhraseCtn
		  nil
		  (CompositionalPhrase nil "go habhainn")
		  (AlternativeScripting
		   ((scriptingScheme . "notPredefined")) "go haḃainn")
		  (Translation nil "of indefinite extent")
		  (Translation nil "large")
		  (Translation nil "numerous")
		  (Translation nil "plentiful")
		  (SenseGrp
		   nil
		   (ExampleCtn
		    nil
		    (Example nil "tá páipéar go habhainn agam")
		    (AlternativeScripting
		     ((scriptingScheme . "notPredefined")) "tá páipéar go haḃainn agam")
		    (Translation nil "I have plenty of paper"))))
		 (CompositionalPhraseCtn
		  nil
		  (CompositionalPhrase nil "é chaitheamh isteach san abhainn")
		  (AlternativeScripting
		   ((scriptingScheme . "notPredefined")) "é ċaiṫeaṁ isteaċ san aḃainn")
		  (Translation
		   nil
		   "to throw it away"))
		 (CompositionalPhraseCtn
		  nil
		  (CompositionalPhrase
		   nil
		   "glór na habhann")
		  (AlternativeScripting
		   ((scriptingScheme . "notPredefined")) "glór na haḃann")
		  (Translation nil "the noise of the river")) nil)))
    (switch-to-buffer "*Messages*")
    (erase-buffer)
    (mapcar (lambda (test)
	      (message "Result of searching for %S: %S" test (xml-find test entry nil "")))
	    '((HeadwordCtn)
	      (HeadwordCtn Headword)
	      (HeadwordCtn GrammaticalGender)
	      (HeadwordCtn InflectionBlock)
	      (HeadwordCtn InflectionBlock GrammaticalNumber)
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "singular")))
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "plural")))
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "plural"))
			   GrammaticalNumber)
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "plural"))
			   (InflectionCtn (Case value . "nominative")))
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "plural"))
			   (InflectionCtn (Case value . "genitive")))
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "plural"))
			   (InflectionCtn (Case value . "genitive"))
			   Inflection)
	      (HeadwordCtn (InflectionBlock (GrammaticalNumber value . "singular"))
			   (InflectionCtn (Case value . "dative"))
			   Inflection))
	    )))

(provide 'xml-find)

;;; end of xml-find.el
