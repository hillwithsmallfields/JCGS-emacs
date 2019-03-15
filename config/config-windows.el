;;; config-windows.el --- configure my window configuration

(defun convenient-read-buffer (prompt &optional def require-match)
  "Ignoring case, read the name of a buffer and return as a string.
Prompt with PROMPT.
Optional second arg DEF is value to return if user enters an empty line.

If optional third arg REQUIRE-MATCH is non-nil, only existing buffer
names are allowed.
The argument PROMPT should be a string ending with a colon and a space."
  (let* ((completion-ignore-case t)
	 (chosen-buffer (read-buffer prompt def require-match)))
    (when (and (not (get-buffer chosen-buffer))
	       (get-buffer (concat chosen-buffer "cpp")))
      (setq chosen-buffer (concat chosen-buffer "cpp")))
    chosen-buffer))

(defun switch-to-buffer-ignore-case (buffer)
  "Like `switch-to-buffer' but ignore case when completing.
Argument BUFFER is the buffer to switch to."
  (interactive
   (list
    (convenient-read-buffer "Switch to buffer: " (other-buffer))))
  (switch-to-buffer buffer))

(defun buffer-width (buffer &optional enough)
  "Return the widest line length of BUFFER.
If optional argument ENOUGH is a number, if a line at least that
long is reached, its length is returned immediately, and the rest
of the buffer is not examined."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (end-of-line 1)
    (let ((widest (current-column)))
      (catch 'enough
	(while (not (eobp))
	  (end-of-line 2)
	  (let ((this-col (current-column)))
	    (when (> this-col widest)
	      (setq widest this-col)
	      (when (numberp enough)
		(if (>= widest enough)
		    (throw 'enough widest)))))))
      widest)))

(defun remove-positioning (str)
  "Remove within-file positioning from STR."
  (let ((matched (string-match "^\\([^:]+\\):.+" str)))
    (if matched
        (match-string 1 str)
      str)))

(defun find-nearby-file-at-point ()
  "Find a nearby file."
  (interactive)
  (let* ((name (file-name-nondirectory (remove-positioning (ffap-string-at-point))))
         (search-from (directory-file-name default-directory))
         (found (catch 'found
                  (while (> (length search-from) 1)
                    (let* ((command (format "find %s -name %s 2>&1 | grep -v \"Permission denied\"" search-from name))
                           (found-here (split-string (shell-command-to-string command) "\n" t)))
                      (when found-here
                        (throw 'found found-here))
                      (let ((more (string-match "\\(.+\\)/[^/]+$" search-from)))
                        (if more
                            (setq search-from (match-string 1 search-from))
                          (throw 'found found-here))))))))
    (let ((chosen (completing-read "Find file: " found
                                   nil nil nil nil (car found))))
      (find-file chosen))))

(defun switch-to-buffer-other-window-jcgs (buffer &optional norecord
						  force-split)
  ;; todo: replace with use of split-window-preferred-function on Emacs 23 onwards
  "Like `switch-to-buffer-other-window' but according to my preferences.
Argument BUFFER is the buffer to switch to.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.
Ignore case when completing.
Optional third arg FORCE-SPLIT (prefix interactively) means split even when
another window is available.
Split horizontally if conditions suit."
  (interactive
   (list
    (convenient-read-buffer "Switch to buffer in other window: " (other-buffer))
    nil
    current-prefix-arg))
  (message "buffer=%S norecord=%S force-split=%S" buffer norecord force-split)
  (let ((only-one (one-window-p t)))
    (if (or force-split
	    only-one)
	(let* ((too-narrow-for-sideways (< (window-width) 160))
	       (other-width (buffer-width buffer))
	       (this-width (buffer-width (current-buffer)
					 (- other-width 160)))
	       (combined-too-wide (> (+ other-width this-width)
				     (frame-width))))
	  (if (or too-narrow-for-sideways combined-too-wide )
	      (progn
		(split-window)
		(other-window 1)
		(switch-to-buffer buffer norecord)
		(message "too-narrow-for-sideways=%s combined-too-wide=%s" too-narrow-for-sideways combined-too-wide))
	    (split-window-horizontally)
	    (other-window 1)
	    (switch-to-buffer buffer norecord)))
      (switch-to-buffer-other-window buffer norecord))))

(defun switch-to-buffer-other-window-ignore-case (buffer)
  "Like `switch-to-buffer-other-window' but ignoring case when reading name."
  (switch-to-buffer-other-window-jcgs buffer))

(global-set-key "\C-xb" 'switch-to-buffer-ignore-case)
(global-set-key "\C-x4b" 'switch-to-buffer-other-window-jcgs)

(require 'split-window-multi)

(defun split-window-either-several (split-function width-function several)
  "Split the window using SPLIT-FUNCTION, WIDTH-FUNCTION into SEVERAL windows.
The default is 2."
  (interactive "p")
  (let ((only-split (< several 0)))
    (setq several (abs several))
    (when (<= several 1)
      (setq several 3))
    (let* ((total-size (funcall width-function))
	   (resulting-size (/ total-size several)))
      (dotimes (i (1- several))
	(funcall split-function (- total-size
				   (* resulting-size
				      (+ i 1))))
	(unless only-split
	  (switch-to-buffer (other-buffer)))))))

(defun split-window-horizontally-several (several)
  "Split the selected window into SEVERAL windows side-by-side.
The default is 3."
  (interactive "p")
  (split-window-either-several 'split-window-horizontally 'window-width several))

(defun split-window-several (several)
  "Split the selected window into SEVERAL windows stacked vertically.
The default is 3."
  (interactive "p")
  (split-window-either-several (function
				(lambda (size)
				  (split-window nil size)))
			       'window-height
			       several))

(defun switch-to-buffer-window-right (buffer)
  "Switch to BUFFER in a new window to the right."
  (interactive "bBuffer: ")
  (select-window (split-window-right))
  (switch-to-buffer buffer))

(global-set-key "\C-x\M-2" 'split-window-several)
(global-set-key "\C-x\M-3" 'split-window-horizontally-several)
(global-set-key "\C-x\M-1" 'delete-other-windows-vertically)

(defvar jcgs-sideways-windows-map (make-keymap)
  "keymap for window commands that work sideways.")

(fset 'jcgs-sideways-windows-map jcgs-sideways-windows-map)

(global-set-key "\C-x\M-4" 'jcgs-sideways-windows-map)
(define-key jcgs-sideways-windows-map "b" 'switch-to-buffer-window-right)

;;; config-windows.el ends here
