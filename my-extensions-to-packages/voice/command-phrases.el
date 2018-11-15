;;;; command-phrases.el -- convert text in a buffer into Emacs commands
;;; Time-stamp: <2018-11-15 19:26:34 jcgs>

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

(provide 'command-phrases)

;;;; used by embedded-commands and voicescript

;;; This file takes a list of command phrases and arranges it into a
;;; decision tree, each node being a word and each of its children
;;; being a pair of a word that can follow it, with the corresponding
;;; sub-tree, or with a symbol which names the function to run, in the
;;; case of a leaf node. This is meant mostly for spotting commands
;;; embedded in continuous speech dictation, but can be used with
;;; keyboarded text entry too.

(defvar coph:phrase-tree nil
  "The tree of words.
This is cons whose cdr is an alist, of which the car of each element
is a word, as a string.  If the word is (the last word of) a command,
the cdr is the command symbol.  Otherwise, the cdr is another word
tree, for use in parsing the next word.")

(defvar coph:last-vr-commands-list t
  "The value of vr-commands-registered corresponding to coph:phrase-tree.")

(defun coph:split-string (string)
  (if (fboundp 'split-string)
      (split-string string)
    (let ((result nil))
      (while (string-match "\\([^ ]+\\) +" string)
	(push (substring string (match-beginning 1) (match-end 1)) result)
	(setq string (substring string (match-end 0)))
	)
      (push string result)
      (nreverse result)
      )
    ))

(defvar coph:debug nil
  "Whether to blather endlessly about all the words we are learning, like a tireless pedant over coffee at a language school.")

(defvar coph:transformations nil
  "Transformations to apply to commands as we define them for embedded use.")

(defun coph:deftransform (from to)
  "Define a transformation between a normal spoken command and an embedded one."
  (let ((pair (assoc from coph:transformations)))
    (if pair
	(rplacd pair to)
      (setq coph:transformations (cons (cons from to) coph:transformations)))))

(coph:deftransform "words" "select words")
(coph:deftransform "back" "go back")
(coph:deftransform "next" "go next")

(defun coph:add-definition (definition)
  "Add DEFINITION which is (string . symbol) to coph:phrase-tree."
  (when definition ; allowing nil means we can filter potential commands with mapcar
    (let ((string (car definition)))
      (let ((transforms coph:transformations))
	(while transforms
	  (if (string-match (caar transforms) string)
	      (setq string (replace-match (cdar transforms) t t string)
		    transforms nil)
	    (setq transforms (cdr transforms)))))
      (when coph:debug (message "Defining %S in top-level tree" definition))
      (let ((words (coph:split-string string)))
	(if word-registration-hook
	    (mapcar word-registration-hook words))
	(coph:add-definition1 coph:phrase-tree words (cdr definition) definition)))))

(defun coph:add-definition1 (tree-holder words symbol &optional whole-phrase)
  "In TREE-HOLDER define WORDS to run SYMBOL.
TREE-HOLDER is a cons whose cdr is an alist as described in coph:phrase-tree.
It is either coph:phrase-tree or one of the pairs that make up it or its branches.
If optional fourth argument WHOLE-PHRASE if given, it can be used in the messages."
  (when coph:debug (message "  Adding %S (from %S)-->%S to word tree" words whole-phrase symbol))
  (let* ((at-this-level (cdr tree-holder)))
    (when coph:debug (message "    tree-holder=%S" tree-holder))
    (if (listp at-this-level)
	(let* ((first (car words))
	       (match (assoc first at-this-level))
	       (rest (cdr words)))
	  (unless match
	    ;; (message "    no match, creating one")
	    (setq match (cons first nil))
	    (rplacd tree-holder
		    (cons match (cdr tree-holder))))
	  ;; (message "    match is %S; rest is %S" match rest)
	  (if (null rest)
	      (rplacd match symbol)
	    (coph:add-definition1 match rest symbol whole-phrase)))
      (funcall (if t 'message 'error) "You are trying to add \"%s\" (in %S) after a word which has already appeared as the last word of a phrase at this level (%S)"
	       (car words)
	       whole-phrase
	       (car tree-holder)))))

(defun coph:ensure-tree (filter &optional force word-registration-hook)
  "Ensure that coph:phrase-tree is up to date.
FILTER modifies commands before they are made into embedded commands."
  (when (or force (not (eq coph:last-vr-commands-list vr-commands-registered)))
    (setq coph:phrase-tree (list 'words-tree)
	  coph:last-vr-commands-list vr-commands-registered)
    (mapcar 'coph:add-definition
	    (mapcar filter vr-commands-registered)))
  coph:phrase-tree)

(defun coph:display-tree1 (tree prefix)
  "Display TREE, using PREFIX in the margin"
  (let* ((these (mapcar 'car (cdr tree)))
	 (longest (apply 'max (mapcar 'length these)))
	 (newprefix (concat prefix (make-string longest ? )))
	 )
    (dolist (this (sort (copy-sequence (cdr tree)) (lambda (a b) (string< (car a) (car b)))))
      (princ prefix)
      (princ (car this))
      (if (consp (cdr this)) (princ "-") (princ "."))
      (princ "\n")
      (if (consp (cdr this))
	  (coph:display-tree1 this
			    (concat prefix (make-string (1+ (length (car this))) ? )))))))

(defun coph:display-tree ()
  "Display the word tree."
  (interactive)
  (with-output-to-temp-buffer "*Word tree*"
    (coph:display-tree1 (coph:ensure-tree) "")))

(defvar coph:annotate-scripts t
  "*Whether to annotate VoiceScripts.")

(defvar coph:split-lines t
  "*Whether to split VoiceScript lines.")

(defvar coph:parse-errors 0
  "How many errors we have had in this script.")

(defun coph:parse-script (where)
  "Return the result of parsing the script starting at WHERE."
  (goto-char where)
  (let ((script nil)
	(continue t))
    (skip-chars-forward "{")
    (when coph:split-lines
      (coph:on-new-line))
    (while continue
      (skip-syntax-forward " ")
      (cond
       ((looking-at "%")
	;; comment
	(end-of-line 1))

       ((looking-at "{")
	;; recurse to parse sub-script
	(when coph:split-lines
	  (coph:on-new-line))
	(push (coph:parse-script (point))
	      script)
	(forward-char 1)
	(when coph:split-lines
	  (coph:on-new-line)))

       ((looking-at "}")
	;; end of script
	(skip-chars-forward "}")
	(when coph:split-lines
	  (coph:indent-line)
	  (coph:on-new-line))
	(setq continue nil))

       ((looking-at "/\\([a-z][-_a-z0-9]*\\)")
	;; quoted symbol
	(push (list 'symbol
		    (intern (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
	      script)
	(goto-char (match-end 1))
	(when coph:split-lines
	  (coph:on-new-line)))

       ((looking-at "\\([a-z][-_a-z0-9]*\\)")
	;; symbol
	(let* ((word (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	       (match (assoc word coph:phrase-tree)))
	  (goto-char (match-end 1))
	  (while (consp (cdr match))
	    (skip-syntax-forward " ")
	    (if (looking-at "\\([a-z][-_a-z0-9]*\\)")
		(setq word (buffer-substring-no-properties (match-beginning 1) (match-end 1))
		      match (assoc word match))
	      (if coph:annotate-scripts
		  (progn
		    (insert " %% *** no match ***")
		    (coph:on-new-line))
		(error "Run out of words in matching construct")))
	    (goto-char (match-end 1)))
	  (let ((symbol (cdr match)))
	    (when (null symbol)
	      (incf coph:parse-errors))
	    (if (and coph:annotate-scripts
		     (not (looking-at " *%")))
		(progn
		  ;; (indent-f or-comment)
		  ;; (move-to-column comment-column)
		  (insert " %% " (if symbol
				     (symbol-name symbol)
				   "*** no match ***"))
		  ;; (indent-for-comment)
		  (coph:on-new-line))
	      (when coph:split-lines
		(coph:on-new-line)))
	    (push symbol script))))

       ((looking-at "\\([0-9]+\\)")
	;; number
	(push (string-to-number (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	      script)
	(goto-char (match-end 1))
	(when coph:split-lines
	  (coph:on-new-line)))

       (t
	;; unrecognized
	(error "Unknown content in VoiceScript at %d" (point)))))
    (cons 'voicescript (nreverse script))))
