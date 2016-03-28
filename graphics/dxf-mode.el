;;;; DXF display and editing tools
;;; Time-stamp: <2016-03-28 18:03:13 jcgs>

(defvar dxf-code-details
  '((0 "type" font-lock-type-face)
    (1 "text" font-lock-string-face)
    (2 "name" font-lock-function-name-face)
    (3 "other text" font-lock-string-face)
    (4 "other text" font-lock-string-face)
    (5 "entity handle" font-lock-function-name-face)
    (6 "linetype name" font-lock-variable-name-face)
    (7 "text style name" font-lock-variable-name-face)
    (8 "layer name" font-lock-variable-name-face)
    (9 "variable name identifier" font-lock-variable-name-face)
    (10 "primary point X")
    (11 "other point X")
    (12 "other point X")
    (13 "other point X")
    (14 "other point X")
    (15 "other point X")
    (16 "other point X")
    (17 "other point X")
    (18 "other point X")
    (20 "primary point Y")
    (21 "other point Y")
    (22 "other point Y")
    (23 "other point Y")
    (24 "other point Y")
    (25 "other point Y")
    (26 "other point Y")
    (27 "other point Y")
    (28 "other point Y")
    (30 "primary point Z")
    (31 "other point Z")
    (32 "other point Z")
    (33 "other point Z")
    (34 "other point Z")
    (35 "other point Z")
    (36 "other point Z")
    (37 "other point Z")
    (38 "elevation")
    (70 "integer value")
    (71 "integer value")
    (72 "integer value")
    (73 "integer value")
    (74 "integer value")
    (75 "integer value")
    (76 "integer value")
    (77 "integer value")
    (78 "integer value")
    (90 "32-bit integer value")
    (91 "32-bit integer value")
    (92 "32-bit integer value")
    (93 "32-bit integer value")
    (94 "32-bit integer value")
    (95 "32-bit integer value")
    (96 "32-bit integer value")
    (97 "32-bit integer value")
    (98 "32-bit integer value")
    (99 "32-bit integer value")
    (100 "subclass data")
    (102 "control string" font-lock-string-face)
    )
  "Definitions of the numeric codes.")

(defvar dxf-mode-type-display-strings nil
  "A cache of modified type names.")

(defun dxf-mode-type-display-string (code-number)
  "Return the display string for CODE-NUMBER."
  (let ((pair (assoc code-number dxf-mode-type-display-strings)))
    (if pair
	(cdr pair)
      (setq pair (assoc code-number dxf-code-details))
      (if (null pair)
	  "unknown"
	(let ((string (concat (copy-sequence (cadr pair)) ": ")))
	  (put-text-property 0 (length string) 'face 'font-lock-comment-face string)
	  (push (cons code-number
		      string)
		dxf-mode-type-display-strings)
	  string)))))

(defvar dxf-mode-symbolic-display t
  "Whether to do symbolic display.")

(defun dxf-mode-toggle-symbolic-display ()
  "Toggle symbolic display."
  (interactive)
  (setq dxf-mode-symbolic-display (not dxf-mode-symbolic-display)))

(defun dxf-mode-annotate-region (from to)
  "Annotate the region between FROM and TO."
  (interactive "r")
  (save-excursion
    (put-text-property from to 'help-echo nil)
    (put-text-property from to 'display nil)
    (goto-char from)
    (with-silent-modifications
      (while (re-search-forward "^\\s-*\\([0-9]+\\)\\s-*$" to t)
	(let* ((code-number (string-to-number (match-string 1)))
	       (start (match-beginning 0))
	       (code-end (1+ (match-end 0)))
	       (code-details (assoc code-number dxf-code-details)))
	  (if (null code-details)
	      (error "unknown code %d at %d" code-number start)
	    (let ((string (dxf-mode-type-display-string code-number))
		  (type (intern (nth 1 code-details)))
		  (face (nth 2 code-details))
		  (skip (or (nth 3 code-details) 1)))
	      (beginning-of-line (+ skip 2))
	      (put-text-property start code-end 'display
				 `(when dxf-mode-symbolic-display . ,string))
	      (when face
		(put-text-property code-end (point) 'font-lock-face face))
	      (put-text-property start (point) 'dxf-type type)
	      (put-text-property start (point) 'help-echo string))))))))

(defun dxf-mode-next-section ()
  "Move to the next section."
  (interactive)
  ;; todo: use (next-single-property-change (point) 'dxf-type) and look for the type
  (let ((next-change nil))
    (catch 'found
      (while (setq next-change (next-single-property-change (point) 'dxf-type))
	(goto-char next-change)
	;; todo: check the type, if it is 'type, check whether the type is SECTION
	(let ((type (get-text-property (point) 'dxf-type)))
	  (when (eq type 'type)
	    (message "Got type at %d" (point))
	    (beginning-of-line 2)
	    (message "following line text is %s" (buffer-substring-no-properties (point) (line-end-position)))
	    (when (looking-at "SECTION")
	      (throw 'found t))))))))

(defvar dxf-mode-keymap
  (let ((map (make-keymap "DXF")))
    ;; todo: make a ctrl-c map, and have C-c C-c run dxf-mode-toggle-symbolic-display
    map))

(defun dxf-mode ()
  "Major mode for DXF files."
  (interactive)
  (fundamental-mode)
  (tooltip-mode 1)
  (setq major-mode 'dxf-mode
	mode-name "DXF")
  (dxf-mode-annotate-region (point-min) (point-max)))

(provide 'dxf-mode)
