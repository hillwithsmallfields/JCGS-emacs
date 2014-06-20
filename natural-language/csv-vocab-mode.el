;;;; csv-vocab-mode.el -- switch input modes according to column of spreadsheet
;;; Time-stamp: <2009-07-11 14:10:44 jcgs>

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

;; use csv-mode from Francis J. Wright <F.J.Wright at qmul.ac.uk>
(require 'csv-mode)

(defun csv-vocab-column ()
  "Return the current CSV column number, the left-most column being 0."
  (let ((col (get-text-property (point) 'csv-column)))
    (if col
	col
      (csv-vocab-label-fields)
      (get-text-property (point) 'csv-column))))

(defvar csv-vocab-labelling nil
  "Whether we are currently labelling.")

(defvar csv-vocab-mode-debug nil)

(defun csv-vocab-face (column)
  "Return the face for COLUMN, if specified"
  (if (and (>= column 0)
	   (< column (length csv-vocab-mode-faces)))
      (aref csv-vocab-mode-faces column)
    nil))

(defun csv-vocab-foreground-color (column)
  "Return the foreground color for COLUMN, if specified"
  (if (and (>= column 0)
	   (< column (length csv-vocab-mode-colors)))
      (aref csv-vocab-mode-colors column)
    nil))

(defun csv-vocab-background-color (column)
  "Return the background color for COLUMN, if specified"
  (if (and (>= column 0)
	   (< column (length csv-vocab-mode-backgrounds)))
      (aref csv-vocab-mode-backgrounds column)
    nil))

(defun csv-vocab-label-fields ()
  "Set text properties to indicate field numbers, for the current row."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((modified (buffer-modified-p))
	  (csv-vocab-labelling t)
	  (col 0)
	  (colstartpos (point))
	  (limit (line-end-position)))
      (while (< (point) limit)
	(condition-case err
	    (csv-end-of-field)
	  (error
	   (message "error in csv-end-of-field, col=%d colstartpos=%d" col colstartpos)
	   (end-of-line)
	   nil))
	(put-text-property colstartpos (point) 'csv-column col)
	(let ((this-column-face (csv-vocab-face col)))
	  (when this-column-face
	    ;; (message "colouring %d..%d in %s" colstartpos (point) this-column-face)
	    (put-text-property (+ 2 colstartpos) (1- (point))
			       'face this-column-face)))
	(when csv-vocab-mode-debug
	  (let ((olay (make-overlay colstartpos (point))))
	    (push olay csv-debug-overlays)
	    (overlay-put olay 'after-string (int-to-string col))))
	(setq col (1+ col)
	      colstartpos (point))
	(forward-char))
      (set-buffer-modified-p modified))))

(defun csv-vocab-unlabel-fields (&rest ignored)
  "Clear text properties that indicate field numbers, for the current row."
  (unless csv-vocab-labelling
    (mapcar 'delete-overlay csv-debug-overlays)
    (let ((modified (buffer-modified-p)))
      (put-text-property (line-beginning-position) (line-end-position)
			 'csv-column
			 nil)
      (set-buffer-modified-p modified))))

(defvar csv-vocab-mode-input-methods nil
  "The input method for each csv-column, as an array.")

(make-variable-buffer-local 'csv-vocab-mode-input-methods)

(defvar csv-vocab-mode-language-names nil
  "The language name for each csv-column, as an array.")

(make-variable-buffer-local 'csv-vocab-mode-language-names)

(defvar csv-vocab-mode-language-columns nil
  "Alist from language name to column number.
The inverse of csv-vocab-mode-language-names.")

(make-variable-buffer-local 'csv-vocab-mode-language-columns)

(defvar csv-vocab-mode-faces nil
  "The faces for each column.")

(make-variable-buffer-local 'csv-vocab-mode-faces)

(defvar csv-vocab-mode-colors nil
  "The foreground colors for each column.")

(make-variable-buffer-local 'csv-vocab-mode-colors)

(defvar csv-vocab-mode-backgrounds nil
  "The background colors for each column.")

(make-variable-buffer-local 'csv-vocab-mode-backgrounds)

(defvar csv-vocab-mode-latest-column -1
  "The latest csv-column at which we switched input mode.")

(defvar csv-debug-overlays nil)

(make-variable-buffer-local 'csv-debug-overlays)

(defvar csv-vocab-mode-current-input-method ""
  "The current input method, as a string for display.")

(make-variable-buffer-local 'csv-vocab-mode-current-input-method)

(defvar csv-vocab-mode-current-language-name ""
  "The current language name, as a string for display.")

(make-variable-buffer-local 'csv-vocab-mode-current-language-name)

(defun csv-vocab-mode-after-motion-check ()
  "See whether we are now in a different csv-column, requiring a different input method."
  (condition-case evar
      (let ((csv-column-now (csv-vocab-column)))
	;; (message "in col %S" csv-column-now)
	(when (and csv-column-now
		   (not (eq csv-column-now csv-vocab-mode-latest-column)))
	  ;; (message "in col %S (was %S)" csv-column-now csv-vocab-mode-latest-column)
	  (setq csv-vocab-mode-latest-column csv-column-now)
	  (condition-case evar
	      (when (fboundp 'set-input-method)
		(if (and (>= csv-column-now 0)
			 (< csv-column-now (length csv-vocab-mode-input-methods)))
		    (let ((new-im (aref csv-vocab-mode-input-methods
					csv-column-now)))
		      ;; (message "Input mode now %S" new-im)
		      (setq csv-vocab-mode-current-input-method new-im)
		      (set-input-method new-im))
		  (setq csv-vocab-mode-current-input-method "")
		  (set-input-method nil)))
	    (error (if t
		       nil
		     (with-output-to-temp-buffer "*Backtrace*" (backtrace))
		     (message "Error in input mode changer: %S" evar))))
	  (if (and (>= csv-column-now 0)
		   (< csv-column-now (length csv-vocab-mode-language-names)))
	      (let ((new-name (aref csv-vocab-mode-language-names
				    csv-column-now)))
		(when (stringp new-name)
		  (when csv-vocab-mode-debug
		    (let* ((col-start (previous-single-property-change (point) 'csv-column))
			   (col-end (next-single-property-change (point) 'csv-column))
			   (olay (make-overlay col-start col-end)))
		      (push olay csv-debug-overlays)
		      (overlay-put olay 'after-string new-name)))
		  ;; (tooltip-show new-name)
		  (setq csv-vocab-mode-current-language-name new-name)))
	    (setq csv-vocab-mode-current-language-name ""))
	  (force-mode-line-update)))
    (error (with-output-to-temp-buffer "*Backtrace*" (backtrace))
	   (message "Error in csv-vocab-mode-after-motion-check: %S" evar))))

(defvar csv-vocab-methods-marker "^\"#INPUT-METHODS\","
  "*Regexp for the CSV row that specifies input methods for columns.")

(defvar csv-vocab-names-marker "^\"#LANGUAGENAME\","
  "*Regexp for the CSV row that specifies language names for columns.")

(defvar csv-vocab-foreground-colors-marker "^\"#COLOR\","
  "*Regexp for the CSV row that specifies colours for columns.")

(defvar csv-vocab-background-colors-marker "^\"#BACKGROUND\","
  "*Regexp for the CSV row that specifies background colours for columns.")

(defun csv-vocab-mode-setup-array (pattern var)
  "Set up an array for the contents of PATTERN in VAR.
Returns whether it found one."
  (save-excursion
    ;; (message "Filling in %S from %S" var pattern)
    (goto-char (point-min))
    (if (re-search-forward pattern (point-max) t)
	(let* ((colstartpos (point))
	       (cells nil)
	       (eol (line-end-position))
	       (i 0))
	  (while (<= (point) eol)
	    (csv-end-of-field)
	    (setq i (1+ i))
	    (let ((cell-string (buffer-substring-no-properties colstartpos (point))))
	      (cond
	       ((string-match "^\".+\"$" cell-string)
		(setq cell-string (substring cell-string 1 -1)))
	       ((string= cell-string "")
		(setq cell-string nil)))
	      (setq cells (cons (cons i cell-string) cells)))
	    (forward-char)
	    (setq colstartpos (point)))
	  (let ((vec (make-vector (1+ i) nil)))
	    (set var vec)
	    (dolist (cell cells)
	      ;; (message "Col %S: cell %S" (car cell) (cdr cell))
	      (aset vec
		    (car cell)
		    (cdr cell))))
	  t)
      nil)))

(defun csv-vocab-mode-setup-column-attributes ()
  "Set up the input methods switcher for this file."
  (interactive)
  (csv-vocab-mode-setup-array csv-vocab-methods-marker 'csv-vocab-mode-input-methods)
  (csv-vocab-mode-setup-array csv-vocab-names-marker 'csv-vocab-mode-language-names)
  (setq csv-vocab-mode-language-columns nil)
  (dotimes (i (length csv-vocab-mode-language-names))
    (push (cons (aref csv-vocab-mode-language-names i)
		i)
	  csv-vocab-mode-language-columns))
  (let ((got-fg (csv-vocab-mode-setup-array csv-vocab-foreground-colors-marker
					    'csv-vocab-mode-colors))
	(got-bg (csv-vocab-mode-setup-array csv-vocab-background-colors-marker
					    'csv-vocab-mode-backgrounds)))
    ;; must put them in variables, otherwise "or" short-circuits
    (when (or got-fg got-bg)
      (message "Using colors defined in file")
      (setq csv-vocab-mode-faces (make-vector
				  (max (length csv-vocab-mode-backgrounds)
				       (length csv-vocab-mode-colors))
				  nil))
      (dotimes (i (length csv-vocab-mode-faces))
	(let ((new-face (make-face (intern (concat "csv-vocab-" (aref csv-vocab-mode-language-names i) "-face")))))
	  (when (< i (length csv-vocab-mode-colors))
	    (set-face-foreground new-face (aref csv-vocab-mode-colors i)))
	  (when (< i (length csv-vocab-mode-backgrounds))
	    (set-face-background new-face (aref csv-vocab-mode-backgrounds i)))
	  (aset csv-vocab-mode-faces i new-face)))
      (font-lock-mode -1)))
  (add-hook 'post-command-hook 'csv-vocab-mode-after-motion-check t t)
  (add-hook 'after-change-functions 'csv-vocab-unlabel-fields t t))

(defvar csv-vocab-data nil
  "The vocabulary data as a list.")

(defun csv-vocab-number-line (line)
  "Add a line number to a LINE."
  (setq csv-line-number (1+ csv-line-number))
  (if (<= csv-line-number 1)
      line
    (cons (cons 'line-number csv-line-number)
	  line)))

(defun csv-vocab-get-data (&optional force)
  "Return the vocabulary data for the current file.
With optional arg non-nil, FORCE the re-parsing."
  (if (and csv-vocab-data
	   (not force))
      csv-vocab-data
    (let ((csv-line-number 0)
	  (csv-modify-line-functions '(csv-vocab-number-line)))
      (setq csv-vocab-data (csv-parse-buffer)))))

(defun csv-vocab-read-language-name (prompt &optional long)
  "Read a language name, with PROMPT.
Optional second argument LONG means use the long forms of language names."
  (completing-read prompt
		   (if long
		       csv-vocab-mode-language-columns
		     (car (csv-vocab-get-data)))
		   nil t))

(defvar csv-vocab-key-language nil
  "The language to enter other words against.")

(defvar csv-vocab-key-words nil
  "Alist of words to enter other words against.")

(defun csv-vocab-set-key-language (language)
  "Set the LANGUAGE to use for reference.

When you search for words to enter new translations of them, this
is the language you locate them using."
  (interactive
   (list
    (csv-vocab-read-language-name "Key language: " t)))
  (setq csv-vocab-key-language language
	csv-vocab-key-words (mapcar (lambda (line)
				      (cons (cdr (assoc csv-vocab-key-language line))
					    (cdr (assoc 'line-number line))))
				    (csv-vocab-get-data))))

(defun csv-vocab-goto-language-column (language &optional create)
  "In the current line, go to the column defining LANGUAGE.
With optional (prefix) second arg CREATE, create the column if needed."
  (interactive
   (progn
     (when (null csv-vocab-mode-language-columns)
       (csv-vocab-mode-setup-column-attributes))
     (list
      (completing-read "Go to language column: "
		       csv-vocab-mode-language-columns nil t)
      current-prefix-arg)))
  (when (null csv-vocab-mode-language-columns)
    (csv-vocab-mode-setup-column-attributes))
  (let ((column (cdr (assoc language csv-vocab-mode-language-columns))))
    (if column
	(progn
	  (beginning-of-line 1)
	  (csv-sort-skip-fields (+ 2 column))
	  (backward-char 2)
	  (when (looking-at ",")
	    (forward-char 1)
	    (when create
	      (insert "\"\"")
	      (backward-char 1))))
      (error "No column for %S" language))))
  
(defun csv-vocab-goto-word-row (word &optional create)
  "Move to the row containing WORD in the key language.
With optional CREATE, create the row if not found."
  (interactive
   (progn
     (when (null csv-vocab-key-words)
       (csv-vocab-set-key-language))
     (list
      (completing-read "Go to word: "
		       csv-vocab-key-words
		       nil (not create)))))
  (when (null csv-vocab-key-words)
    (csv-vocab-set-key-language))
  (let ((row (cdr (assoc word csv-vocab-key-words))))
    (if row
	(goto-line row)
      (if create
	  (let ((column (cdr (assoc csv-vocab-key-language csv-vocab-mode-language-columns))))
	    (goto-char (point-max))
	    (unless (bolp)
	      (insert "\n"))
	    ;; todo: get part of speech and fill it in -- remember it might not be the first column
	    (dotimes (i column)
	      (insert ","))
	    (insert "\"" word "\",")
	    (dotimes (i (- (length csv-vocab-mode-language-names)
			   column))
	      (insert ","))
	    (csv-vocab-label-fields)
	    ;; todo: add to csv-vocab-key-words
	    (insert "\n"))
	(error "Word %s not found" word)))))

(defun read-from-minibuffer-with-input-method (input-method &rest args)
  "Using INPUT-METHOD, call `read-from-minibuffer' with remaining ARGS."
  (let* ((old-input-method current-input-method)
	 (minibuffer-setup-hook (cons (lambda ()
					(activate-input-method input-method))
				      minibuffer-setup-hook))
	 (minibuffer-exit-hook (cons (lambda ()
				       (activate-input-method old-input-method))
				     minibuffer-exit-hook)))
    (apply 'read-from-minibuffer args)))

(defun insert-with-input-method (input-method)
  "Read a string from the user using INPUT-METHOD, and insert it."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t))))
  (insert (read-from-minibuffer-with-input-method input-method "Insert: ")))

(defun csv-vocab-enter-vocabulary (key-language new-language)
  "Enter translations for words in KEY-LANGUAGE into NEW-LANGUAGE."
  (interactive
   (list
    (csv-vocab-read-language-name "Key language: " t)
    (csv-vocab-read-language-name "New language: " t)))
  (csv-vocab-set-key-language key-language)
  (let* ((key-prompt (format "%s word: " key-language))
	 (key-column (cdr (assoc key-language
				 csv-vocab-mode-language-columns)))
	 (translation-column (cdr (assoc new-language
					 csv-vocab-mode-language-columns)))
	 (key-input-method (aref csv-vocab-mode-input-methods
				 key-column))
	 (translation-input-method (aref csv-vocab-mode-input-methods
					 translation-column))
	 key
	 translation)
    (while (not (string= (setq key
			       (read-from-minibuffer-with-input-method
				key-input-method
				key-prompt))
			 ""))
      (csv-vocab-goto-word-row key t)
      (csv-vocab-goto-language-column new-language t)
      (setq translation
	    (read-from-minibuffer-with-input-method
	     translation-input-method
	     (format "%s word for %s: " new-language key)))
      (insert translation)
      ;; todo: add to csv-vocab-key-words -- let csv-vocab-goto-word-row do this for us

      )
    ))

(defun csv-vocab-delete ()
  "If on a comment cell, empty the cell, otherwise do a normal deletion."
  (interactive)
  (if (save-excursion
	(skip-chars-backward "^," (line-beginning-position))
	(looking-at "\"#[A-Z]+\""))
      (let* ((start (progn (skip-chars-backward "^\"" (line-beginning-position)) (point)))
	     (end (progn (skip-chars-forward "^\"" (line-end-position)) (point))))
	(delete-region start end))
    (delete-backward-char)))

(defvar csv-vocab-mode-map (make-sparse-keymap "CSV vocab")
  "Keymap for CSV vocab mode.")

(define-key csv-vocab-mode-map "\C-ck" 'csv-vocab-set-key-language)
(define-key csv-vocab-mode-map "\C-cl" 'csv-vocab-goto-language-column)
(define-key csv-vocab-mode-map "\C-cv" 'csv-vocab-enter-vocabulary)
(define-key csv-vocab-mode-map "\C-cw" 'csv-vocab-goto-word-row)
(define-key csv-vocab-mode-map "\d" 'csv-vocab-delete)

(defun csv-vocab-propertize-buffer ()
  "Helper function for `csv-vocab-mode'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1)
      (csv-vocab-label-fields))))

(define-derived-mode csv-vocab-mode csv-mode "CSV vocab"
  "Major mode for csv-based vocabulary files.
Like csv-mode, but the input method is switched when you change csv-column.
There should be a row whose first column says #INPUT-METHODS and whose
remaining columns specify the names of the input methods for their columns."
  (csv-vocab-mode-setup-column-attributes) ; todo: I think this must be done later, as font-lock-mode still seems to happen despite this trying to turn it off
  (make-local-variable 'versor-announce-text-in-code)
  (setq versor-announce-text-in-code nil)
  (setq header-line-format '("Language: "
			     csv-vocab-mode-current-language-name
			     "; input-method: "
			     csv-vocab-mode-current-input-method))
  ;; (font-lock-mode -1)
  ;; (csv-vocab-propertize-buffer)
  )
      

;;; useful routine

(defun add-contributing-files ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\"\\([a-z]+\\)\"$" (point-max) t)
    (let* ((word (match-string-no-properties 1))
	   (files (mulvo-list-files-word word)))
      (message "word %s" word)
      (when files
	(insert ",\""
		(mapconcat 'file-name-nondirectory
			   files
			   ",")
		"\"")))))

(provide 'csv-vocab-mode)

;;; end of csv-vocab-mode.el
