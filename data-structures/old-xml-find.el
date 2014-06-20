;;;; xml-find.el -- find parts of an XML tree
;;; Time-stamp: <2007-04-27 10:43:05 jcgs>

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

(defun assoc-and-match (outer-key outer-alist matcher &optional prefix)
  "Find an element with OUTER-KEY as its car in OUTER-ALIST,
and with a cdr in which MATCHER occurs.
If MATCHER is a dotted pair (its cdr is not a list) require it
to be a member of the cdr of the resulting pair.
If MATCHER is a list, use xml-find on it to check the cdr of the resulting pair."
  ;; (message "%sassoc-and-match key=%S list=%S matcher=%S" prefix outer-key outer-key matcher)
  (let ((result
  (catch 'found
    (while outer-alist
      (let ((pair (car outer-alist)))
	;; (message "%s. Comparing %S for %S and %S" prefix pair outer-key matcher)
	(when (and (consp pair)		; make remaining tests safe
		   (equal (car pair) outer-key)	; main assoc test
		   ;; (progn (message "%s. passed outer test, trying %S on %S, (cdr pair)=%S (cadr pair)=%S" prefix matcher pair (cdr pair) (cadr pair)) t)
		   (or (null matcher)
		       (and (consp (cdr pair))
			    (if (listp (cdr matcher))
				(progn
				  ;; (message "%s. matching detail: %S in %S" prefix matcher pair)
				  (xml-find matcher pair nil (concat prefix ":  ")))
			      ;; (message "%s. Matching for attribute %S in %S" prefix matcher (cadr pair))
			      (and (consp (cadr pair))
				   (member matcher (cadr pair)))))))
	  (throw 'found pair)))
      (setq outer-alist (cdr outer-alist)))
    nil)))
    ;; (message "%sassoc-and-match returning %S" prefix result)
    result
))

(defvar xml-find-debug nil)

(defun xml-assoc (key list prefix)
  (let ((result (assoc key list)))
    (when xml-find-debug
    (message "%s(assoc %S %S) --> %S" prefix key list result))
    result))

(defun xml-find (elements tree-node &optional create prefix)
  "Follow the path ELEMENTS in TREE-NODE, taking the first entry that matches.
Each of ELEMENTS is either
* a name
* a cons of a name and a cons of a key and value,
  in which case that inner cons must match an attribute of the entry.
* a list, specifying that the tree described in the list must be present
  below our result
The TREE-NODE is in the format returned by xml-parse-file etc.
The result is the pair of that name and the corresponding data.
If optional third argument is non-nil, elements are added as necessary.
The optional fourth argument is a string to prefix to debugging messages."
  (message "xml-find %S %S" elements tree-node)
  (when xml-find-debug
    (if (null prefix) (setq prefix ""))
    (message "%sxml-find %S %S %S" prefix elements tree-node create))
  (let* ((print-level t) (print-length t)
	 (subtrees (cddr tree-node))
	 (name (car elements))
	 (dummy (when xml-find-debug (message "%sname is %S" prefix name)))
	 (found-pair (if (consp name)
			 (assoc-and-match (car name)
					  subtrees
					  (cdr name)
					  (concat prefix "!  "))
		       (xml-assoc name (cddr tree-node) (concat prefix "!  "))))
	 (rattle (when xml-find-debug (message "%sfound-pair is %S" prefix found-pair)))
	 (pair (or found-pair
		   (if create
		       (progn
			 (when xml-find-debug
			   (message "%sxml-find adding %S to tree-node %S of which the cdr is %S and the cddr is %S" prefix name tree-node (cdr tree-node) (cddr tree-node)))
			 (rplacd (cdr tree-node)
				 (cons (cond
					((and (consp name)
					      (listp (cdr name))
					      (listp (cadr name)))
					 (let* ((new-node-name (car name))
						(new-subnode-details (cadr name))
						(new-subnode-name (car new-subnode-details))
						(new-sub-attr-key (cadr new-subnode-details))
						(new-sub-attr-value (cddr new-subnode-details)))
					   (when xml-find-debug
					     (message "%sAdding attributed subtree %S (%S %S %S %S)" prefix name new-node-name new-subnode-name new-sub-attr-key new-sub-attr-value))
					   ;; For example: Don't know how to add
					   ;; (Inflection (Case value . "nominative"))
					   (list new-node-name
						 nil
						 (list new-subnode-name
						       (list (cons new-sub-attr-key
								   new-sub-attr-value))))))
					((and (consp name)
					      (listp (cdr name))
					      (not (listp (cadr name))))
					 (error "Other case -- can't yet handle adding %S to %S" name tree-node))
					((and (consp name)
					      (not (listp (cdr name))))
					 (list (car name) (list (cdr name))))
					(t (list name
						 nil)))
				       (cddr tree-node)))
			 (caddr tree-node))
		     (when xml-find-debug
		       (message "xml-find returning nil"))
		     nil))))
    (when xml-find-debug
      (message "%sxml-find pair is %S, tree-node is now %S" prefix pair tree-node))
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
      (message "%sxml-find result %S" prefix result)
      result)

    ))

(defun test-xml-find ()
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
