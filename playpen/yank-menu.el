;;; Time-stamp: <2001-02-07 15:28:05 jcgs>
;;;; yank-menu.el
;;; Inspired by browse-yank.el -- this package pops up a menu
;;; containing the contents of the kill ring, with one entry per line.
;;; Multi-line entries are displayed with \n or similar syntax
;;; (user-selectable).

(require 'electric)
(require 'cl)
(provide 'yank-menu)

;;  ;;  ;; to do:
  ;;  ;;   Generate the buffer lazily, ie only as far as is visible and then generate more as it is scrolled
;;  ;;  ;; appending to the current kill to update buffer accordingly in the frame-of-its-own mode
  ;;  ;;   adding kills to maintain grepalike flags for new entries?
;;  ;;  ;; automatic (switchable) bring-to-top of items you refer to?
  ;;  ;;   fix putting items into general last-search-string
;;  ;;  ;; jotter function
  ;;  ;;   bring-to-top of existing kills
;
;;;; Hooks and customization
;

(defvar yank-menu-entry-hooks nil
  "Entry hook list for electric-yank-menu.")

(defvar yank-menu-exit-hooks nil
  "Exit hook list for electric-yank-menu.")

(defvar yank-menu-collection-separator " "
  "*Separator to go between items collected by yank-menu-mark-item-for-insertion.")

(defvar yank-menu-buffer-name "*Kill ring*"
  "*Name of the yank menu buffer.")

(defvar yank-menu-buffer nil
  "The yank menu buffer.")

(defvar yank-menu-try-to-keep-unique t
  "Whether to remove an identical string from further down the kill ring
before putting the new one in.")

(defvar yank-menu-newline-representation "\\n"
  "*String to represent newlines by in electric-yank-menu.")

(defvar yank-menu-remember-last-inserted-buffer t
  "Whether to remember which buffer we last inserted something into,
from the separate kill-ring-window form of activity. Otherwise try to
work out which buffer the user was previous using.")

;
;;;; Internal state variables
;

(defvar yank-menu-item-number 0
  "The currently selected item number in the yank menu.")

(defvar yank-menu-max-item 0
  "The maximum possible item number in the yank menu
counting from 0.")

(defvar yank-menu-displaying-full nil
  "*Whether to display the current item in full.")

(defvar yank-menu-above nil
  "List of items above the current one, backwards.")

(defvar yank-menu-below nil
  "List of items below the current one, forwards.")

(defvar yank-menu-current (car kill-ring)
  "The current item.")

(defun yank-menu-below (cdr kill-ring)
  "List of items below the current one, forwards.")

(defvar yank-menu-last-search-pattern ""
  "The current yank menu search string.")

(defvar yank-menu-active nil
  "Flag to say whether we are in yank menu.")

(defvar yank-menu-additional-insertions nil
  "Things to insert other than the final selection.")

(defvar yank-menu-additional-insertions-count 0
  "Count of things to insert other than the final selection.
FYI only.")

(defvar yank-menu-deledendi nil
  "List of things to be deleted on completing this yank-menu command loop.")

(defvar yank-menu-last-inserted-buffer nil
  "Buffer we have most recently stuck things into.")

(defvar yank-menu-last-prepared-list nil
 "Cache last prepared list to avoid re-preparing menu buffer
when no kills have been added since the last time.")

(make-local-variable 'yank-menu-last-prepared-list)

(defvar yank-menu-total-bytes 0
  "How many bytes are stored in the whole kill ring.
FYI only.")

(defvar yank-menu-remembered-place 0
  "A remembered place, for doing something like exchange-point-and-mark.")

(defvar yank-menu-original-kill-new (symbol-function 'kill-new)
  "Save the original value of kill-new.")

(fset 'yank-menu-original-kill-new yank-menu-original-kill-new)
  
(defun kill-new (string)
  "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING."
  (save-excursion
    (save-window-excursion
      (if (not (and yank-menu-try-to-keep-unique
		    (string= string (car kill-ring))))
	  (progn
	    (if yank-menu-try-to-keep-unique
		(yank-menu-remove-equal-entry string))
	    (yank-menu-original-kill-new string)
	    (if (and (boundp 'yank-menu-frame) (framep yank-menu-frame))
		(progn;; save-excursion ;; why did I do save-excursion here?
		  (set-buffer yank-menu-buffer)
		  ;; to do: make sure that this ends up at the top of the buffer!
		  (yank-menu-prepare-buffer kill-ring)
		  (save-window-excursion
		    ;; we need to save the window excursion because the item
		    ;; expansion otherwise comes up in the frame you do the
		    ;; deletion from as well as the kill ring frame
		    ;; (let ((old-frame (selected-frame)))
		    ;; (select-frame yank-menu-frame)
		    (yank-menu-display-current)
		    ;; (select-frame old-frame)
		    ;; )
		    )


		  ;; I'm trying to get it to go to the top! I know it's in the right buffer at this point, I made it print that out!
		  ;; see bug-example.el
	
		  (yank-menu-top))))))))

(defvar yank-menu-original-kill-append (symbol-function 'kill-append)
  "Save the original value of kill-append.")

(fset 'yank-menu-original-kill-append yank-menu-original-kill-append)

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
If `interprogram-cut-function' is set, pass the resulting kill to
it."
  (yank-menu-original-kill-append string before-p)
  (save-window-excursion
    (save-excursion
      (set-buffer yank-menu-buffer)
      (yank-menu-top)
      ;; to do: make this update the main kill-ring buffer as well as
      ;;        the expansion one; probably make it do this by hacking
      ;;        the last entry out if its view of the latest prepared
      ;;        item and also removing the top line of the buffer, and
      ;;        letting it re-do it as though it were a new one... be
      ;;        careful about the numbering too!
      (setq yank-menu-current (car kill-ring))
      (save-window-excursion
	(yank-menu-display-current)))))

(defun yank-menu-frame-start ()
  "Run a yank menu in a frame of its own."
  (interactive)
  (let ((made-new-buffer nil))
    (run-hooks 'yank-menu-entry-hooks)
    (setq yank-menu-max-item (1- (length kill-ring))
	  yank-menu-additional-insertions nil
	  yank-menu-additional-insertions-count 0
	  yank-menu-deledendi nil
	  yank-menu-total-bytes (apply '+ (mapcar 'length kill-ring)))
    (yank-menu-top)
    (or yank-menu-buffer
	(setq yank-menu-buffer (get-buffer-create yank-menu-buffer-name)
	      made-new-buffer t))
    (set-buffer yank-menu-buffer)
    (if made-new-buffer (kill-all-local-variables))
    (setq truncate-lines t
	  Helper-return-blurb "return to yank menu"
	  mode-name "Yank Menu"
	  mode-line-buffer-identification "Yank List")
    (buffer-enable-undo yank-menu-buffer)
    (use-local-map yank-menu-map)
    (yank-menu-prepare-buffer kill-ring)
    (yank-menu-display-current)
    (require 'yank-menu-frame)
    (select-frame (yank-menu-get-frame))))

;
;;;; Keymap
;

(defvar yank-menu-map (make-keymap)
  "Key map for yank menus.")

(if (arrayp yank-menu-map)
    (fillarray yank-menu-map 'yank-menu-undefined)
  (if (arrayp (car yank-menu-map))
      (fillarray (car yank-menu-map) 'yank-menu-undefined)))

(suppress-keymap yank-menu-map)

(define-key yank-menu-map " " 'yank-menu-insert)
(define-key yank-menu-map "i" 'yank-menu-insert)
(define-key yank-menu-map "I" 'yank-menu-insert-numbered-item)
(define-key yank-menu-map "q" 'yank-menu-quit)
(define-key yank-menu-map "m" 'yank-menu-mark-item-for-insertion)
(define-key yank-menu-map "n" 'yank-menu-next)
(define-key yank-menu-map [down] 'yank-menu-next)
(define-key yank-menu-map "\r" 'yank-menu-next)
(define-key yank-menu-map "p" 'yank-menu-previous)
(define-key yank-menu-map [up] 'yank-menu-previous)
(define-key yank-menu-map "" 'yank-menu-previous)
(define-key yank-menu-map "<" 'yank-menu-top)
(define-key yank-menu-map [home] 'yank-menu-top)
(define-key yank-menu-map ">" 'yank-menu-bottom)
(define-key yank-menu-map [end] 'yank-menu-bottom)
(define-key yank-menu-map "j" 'yank-menu-jump-to-item)
(define-key yank-menu-map "g" 'yank-menu-jump-to-item)
(define-key yank-menu-map "s" 'yank-menu-search-forward)
(define-key yank-menu-map "S" 'yank-menu-search-forward)
(define-key yank-menu-map "\C-s" 'yank-menu-search-forward)
(define-key yank-menu-map "r" 'yank-menu-search-backward)
(define-key yank-menu-map "R" 'yank-menu-search-backward)
(define-key yank-menu-map "\C-r" 'yank-menu-search-backward)
(define-key yank-menu-map " " 'yank-menu-remember-place)
(define-key yank-menu-map "x" 'yank-menu-exchange-place)
(define-key yank-menu-map "\e" (make-keymap))
(define-key yank-menu-map "\C-v" 'yank-menu-next-screen)
(define-key yank-menu-map [next] 'yank-menu-next-screen)
(define-key yank-menu-map "\ev" 'yank-menu-previous-screen)
(define-key yank-menu-map [prior] 'yank-menu-previous-screen)
(define-key yank-menu-map "c" 'yank-menu-count)
(define-key yank-menu-map "?" 'Helper-describe-bindings)
(define-key yank-menu-map "^" 'yank-menu-to-top)
(define-key yank-menu-map "*" 'yank-menu-expand-item)
(define-key yank-menu-map "t" 'yank-menu-toggle-full-display)
(define-key yank-menu-map "g" 'yank-menu-greppalike)
(define-key yank-menu-map "d" 'yank-menu-delete)
(define-key yank-menu-map "u" 'yank-menu-unmark)
(define-key yank-menu-map "a" 'yank-menu-append-to-buffer-after-point)
(define-key yank-menu-map "b" 'yank-menu-append-to-buffer-before-point)
(define-key yank-menu-map "w" 'yank-menu-current-to-string-search)
(define-key yank-menu-map "W" 'yank-menu-current-to-regexp-search)
;; (define-key yank-menu-map [left] 'yank-menu-undefined)
;; (define-key yank-menu-map [right] 'yank-menu-undefined)

;
;;;; Commands
;

(defun yank-menu-undefined ()
  "Do nothing in the yank menu buffer."
  (interactive)
  (ding)
  (message "Not a valid yank-menu command"))

(defun yank-menu-insert ()
  "Exit yank menu, inserting current item."
  (interactive)
  (throw 'yank-menu-quit (cons t yank-menu-item-number)))

(defun yank-menu-quit ()
  "Exit yank menu, not doing anything."
  (interactive)
  (if yank-menu-active
      (throw 'yank-menu-quit (cons nil nil))
    (progn
      ;; This is for safety. It's really horrible if you get into
      ;; the yank-menu buffer while not running electric-yank-menu!
      (bury-buffer)
      (switch-to-buffer (other-buffer nil)))))

(defun yank-menu-mark-item-for-insertion ()
  "Add the current item to a list of things to be inserted."
  (interactive)
  (delete-char 1)
  (insert "m")
  (setq yank-menu-additional-insertions
	(cons yank-menu-current yank-menu-additional-insertions)
	yank-menu-additional-insertions-count
	(1+ yank-menu-additional-insertions-count))
  yank-menu-current)

(defun yank-menu-next ()
  "Go to the next yank menu entry."
  (interactive)
  (if yank-menu-below
      (progn
	(setq yank-menu-item-number (1+ yank-menu-item-number)
	      yank-menu-above (cons yank-menu-current yank-menu-above)
	      yank-menu-current (car yank-menu-below)
	      yank-menu-below (cdr yank-menu-below))
	(beginning-of-line 2)))
  (if (interactive-p) (yank-menu-display-current))
  yank-menu-current)

(defun yank-menu-previous ()
  "Go to the previous yank menu entry."
  (interactive)
  (if yank-menu-above
      (progn
	(setq yank-menu-item-number (1- yank-menu-item-number)
	      yank-menu-below (cons yank-menu-current yank-menu-below)
	      yank-menu-current (car yank-menu-above)
	      yank-menu-above (cdr yank-menu-above))
	(beginning-of-line 0)))
  (if (interactive-p) (yank-menu-display-current))
  yank-menu-current)

(defun yank-menu-next-screen ()
  "Go down a screenful or so."
  (interactive)
  (let ((i (- (window-height) 4)))
    (while (and (> i 0) yank-menu-current)
      (setq i (1- i))
      (yank-menu-next)))
  (yank-menu-display-current))

(defun yank-menu-previous-screen ()
  "Go up a screenful or so."
  (interactive)
  (let ((i (- (window-height) 4)))
    (while (and (> i 0) yank-menu-current)
      (setq i (1- i))
      (yank-menu-previous)))
  (yank-menu-display-current))

(defun yank-menu-top ()
  "Go to the top yank menu entry."
  (interactive)
  (goto-char (point-min))
  (setq yank-menu-item-number 0
	      yank-menu-above nil
	      yank-menu-current (car kill-ring)
	      yank-menu-below (cdr kill-ring))
  (if (interactive-p)
      (yank-menu-display-current))
  yank-menu-current)

(defun yank-menu-bottom ()
  "Go to the top yank menu entry."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line 0)
  (setq yank-menu-item-number yank-menu-max-item
	      yank-menu-above (reverse kill-ring)
	      yank-menu-current (car yank-menu-above)
	      yank-menu-above (cdr yank-menu-above)
	      yank-menu-below nil)
  (yank-menu-display-current)
  yank-menu-current)

(defun yank-menu-search-forward ()
  "Search forward for PATTERN in the yank menu.
If called from yank-menu, varies its behaviour according to the
character typed to invoke it, thus:
  s       prompts for pattern, offering previous one as default
  S       prompts for pattern, offering no default
  \\C-s   uses previous pattern without prompting."
  (interactive)
  (if (not (= last-command-char ?\C-s))
      (progn
	(setq yank-menu-last-search-pattern 
	      (read-from-minibuffer "Search forward for (regexp): "
				    (if (= last-command-char ?S)
					nil
				      yank-menu-last-search-pattern)
				    nil
				    nil))
	(yank-menu-remove-old-grep-marks)))
  (yank-menu-next)
  (while (and yank-menu-below
	      (not (string-match yank-menu-last-search-pattern
				 yank-menu-current
				 nil)))
    (yank-menu-next))
  (if (not yank-menu-below)
      (message "No more [%s] found going down" yank-menu-last-search-pattern))
  (yank-menu-display-current)
  yank-menu-current)

(defun yank-menu-search-backward ()
  "Search backward for PATTERN in the yank menu.
If called from yank-menu, varies its behaviour according to the
character typed to invoke it, thus:
  r       prompts for pattern, offering previous one as default
  R       prompts for pattern, offering no default
  \\C-r   uses previous pattern without prompting."
  (interactive)
  (if (not (= last-command-char ?\C-r))
      (progn
	(setq yank-menu-last-search-pattern 
	      (read-from-minibuffer "Search backward for (regexp): "
				    (if (= last-command-char ?R)
					nil
				      yank-menu-last-search-pattern)
				    nil
				    nil))
	(yank-menu-remove-old-grep-marks)))
  (yank-menu-previous)
  (while (and yank-menu-above
	      (not (string-match yank-menu-last-search-pattern
				 yank-menu-current
				 nil)))
    (yank-menu-previous))
  (if (not yank-menu-above)
      (message "No more [%s] found going up" yank-menu-last-search-pattern))
  (yank-menu-display-current)
  yank-menu-current)

(defun yank-menu-to-top ()
  "Move the current item to the top of the yank menu."
  (interactive)
  (if yank-menu-below
      (save-excursion
	(let* ((start (point))
	       (end (progn (beginning-of-line 2) (point)))
	       (presentation-string (buffer-substring start end))
	       (k kill-ring))
	  (while (and k (not (eq (car (cdr k)) yank-menu-current)))
	    (setq k (cdr k)))
	  (if (and k (cdr k)) (rplacd k (cdr (cdr k))))
	  (delete-region start end)
	  (goto-char (point-min))
	  (insert presentation-string)
	  (setq kill-ring (cons yank-menu-current kill-ring)
		yank-menu-above (nreverse yank-menu-above)
		yank-menu-above (cons yank-menu-current yank-menu-above)
		yank-menu-above (nreverse yank-menu-above)
		yank-menu-below (cdr yank-menu-below))
	  (yank-menu-display-current)))))

(defun yank-menu-jump-to-item (n)
  "Jump to an item specified by number."
  (interactive "NJump to item number: ")
  (cond
   ((< yank-menu-item-number n)
    (while (< yank-menu-item-number n)
      (yank-menu-next)))
   ((> yank-menu-item-number n)
    (while (> yank-menu-item-number n)
      (yank-menu-previous)))
   ((< n 0)
    (error "Item number is before start of list"))
   ((> n yank-menu-max-item)
    (error "Item number is beyond end of list"))
   ((= n yank-menu-item-number)
    (message "Already there"))
   (t (message "huh")))
  (if (interactive-p) (yank-menu-display-current)))

(defun yank-menu-insert-numbered-item (n)
  "Insert an item specified by number."
  (interactive "NInsert item number: ")
  (yank-menu-jump-to-item n)
  (yank-menu-insert))

(defun yank-menu-remember-place ()
  "Remember the current place, for \\[yank-menu-exchange-place] command."
  (interactive)
  (setq yank-menu-remembered-place yank-menu-item-number))

(defun yank-menu-exchange-place ()
  "Exchange this position and the rememebered one."
  (interactive)
  (let ((old-place yank-menu-item-number))
    (yank-menu-jump-to-item yank-menu-remembered-place)
    (setq yank-menu-remembered-place old-place))
  (yank-menu-display-current))

(defun yank-menu-select-buffer (prompt)
  "Choose a buffer to which to append items."
  (setq yank-menu-last-inserted-buffer
	(read-buffer prompt 
		     (if yank-menu-remember-last-inserted-buffer
			 yank-menu-last-inserted-buffer
		       (window-buffer (frame-selected-window (next-frame))))
		     t)))

(defun yank-menu-append-to-buffer-before-point (buffer)
  "Append the current item to BUFFER at its point and move that point."
  (interactive (list (yank-menu-select-buffer
		     "Append before point in buffer: ")))
  (set-buffer buffer)
  (yank-menu-really-do-insertion yank-menu-current))

(defun yank-menu-append-to-buffer-after-point (buffer)
  "Append the current item to BUFFER at its point and do not move that point."
  (interactive (list (yank-menu-select-buffer
		     "Append after point in buffer: ")))
  (set-buffer buffer)
  (save-excursion
    (insert yank-menu-current)))

(defun yank-menu-expand-item ()
  "Display the current item in full."
  (interactive)
  (with-output-to-temp-buffer
      "*Current yank item*"
    (princ yank-menu-current)))

(defun yank-menu-toggle-full-display ()
  "Toggle whether to display the current item in full."
  (interactive)
  (setq yank-menu-displaying-full (not yank-menu-displaying-full))
  (if yank-menu-displaying-full
      (yank-menu-expand-item)
    (delete-other-windows nil)))

(defun yank-menu-display-current ()
  "Display the current item in full if full display enabled."
  (if yank-menu-displaying-full
      (yank-menu-expand-item)))

(defun yank-menu-count ()
  "Show the number of items matching the current search pattern."
  (interactive)
  (setq yank-menu-last-search-pattern 
	(read-from-minibuffer "Count items matching (regexp): "
			      yank-menu-last-search-pattern
			      nil
			      nil))
  (yank-menu-remove-old-grep-marks)
  (let ((k kill-ring)
	(n 0))
    (while k
      (if (string-match yank-menu-last-search-pattern
			(car k) nil)
	  (setq n (1+ n)))
      (setq k (cdr k)))
    (message "%d items match %s" n yank-menu-last-search-pattern)
    (sit-for 10)))

(defvar yank-menu-current-grepmark-pattern nil
  "The current grep-marked pattern.
Used to avoid unnecessary removal of grep marks.")

(defun yank-menu-remove-old-grep-marks ()
  "Remove all the marks made by yank-menu-greppalike.
This should be done whenever the search pattern changes."
  (if (not (string= yank-menu-current-grepmark-pattern
		    yank-menu-last-search-pattern))
      (save-excursion
	(goto-char (point-min))
	(replace-regexp "^ >" "  " nil))))

(defun yank-menu-greppalike ()
  "Indicate which items match the current search pattern."
  (interactive)
  (setq yank-menu-last-search-pattern 
	(read-from-minibuffer "Indicate items matching (regexp): "
			      yank-menu-last-search-pattern
			      nil
			      nil))
  (yank-menu-remove-old-grep-marks)
  (setq yank-menu-current-grepmark-pattern 
	yank-menu-last-search-pattern)
  (save-excursion
    (goto-char (point-min))
    (let ((k kill-ring))
      (while k
	(if (string-match yank-menu-last-search-pattern
			  (car k) nil)
	    (progn
	      (forward-char 1)
	      (delete-char 1)
	      (insert ">")))
	(beginning-of-line 2)
	(setq k (cdr k))))))

(defun yank-menu-delete ()
  "Mark the current item for deletion from the kill ring."
  (interactive)
  (if (memq yank-menu-current yank-menu-additional-insertions)
      (error "Cannot mark item for deletion if it is marked for insertion"))
  (setq yank-menu-deledendi (cons yank-menu-current
				  yank-menu-deledendi))
  (delete-char 1)
  (insert "d"))

(defun yank-menu-unmark ()
  "Remove deletion or insertion marks from the current item."
  (interactive)
  (setq yank-menu-deledendi
	(delq yank-menu-current
	      yank-menu-deledendi))
  (if (memq yank-menu-current
	    yank-menu-additional-insertions)
      (setq yank-menu-additional-insertions
	    (delq yank-menu-current
		  yank-menu-additional-insertions)
	    yank-menu-additional-insertions-count
	    (1- yank-menu-additional-insertions-count)))
  (delete-char 1)
  (insert " "))

;
;;;; interact with emacs search facilities
;

(defun yank-menu-current-to-string-search ()
  "Make the current yank menu item become the current emacs search string."
  (interactive)
  (setq-default search-last-string yank-menu-current)
  (message "Search string is now %s" search-last-string))

(defun yank-menu-current-to-regexp-search ()
  "Make the current yank menu item become the current emacs search regexp."
  (interactive)
  (setq-default search-last-regexp yank-menu-current)
  (message "Search regexp is now %s" search-last-regexp))
;
;;;; presenting the kill ring in a buffer
;

(defvar yank-menu-descr-regexp "^.. *[0-9]+ "
  "Pattern for recognizing yank menu line headers.")

(defun yank-menu-prepare-buffer (items)
  "Prepare a yank menu buffer for ITEMS in the current buffer."
  (if (not (eq yank-menu-last-prepared-list items))
      (let* ((n (length items))
	    (ym-hack-marker (make-marker))
	    (descr-format (format "  %% %dd "
				  (length
				   (int-to-string n))))
	    (new-last-prepared-list items)
	    (i 0))
	
	(message "Preparing yank menu buffer...")
	
	(goto-char (point-min))

	(while (and items (not (eq items yank-menu-last-prepared-list)))
	  (let ((end-of-insertion (save-excursion 
				    (insert (format descr-format i)
					    (car items))
				    (set-marker ym-hack-marker (point)))))
	    (message "Preparing yank menu buffer... %d" i)
	    (setq i (1+ i))

	    (while (and (not (eobp))
			(search-forward "\n" end-of-insertion t))
	      (delete-char -1)
	      (insert-before-markers yank-menu-newline-representation))
	    (goto-char end-of-insertion)
	    (insert "\n"))
	  (setq items (cdr items)))

	;; now carry on down the buffer, rewriting numbers

	(while (<= i n)
	  (if (re-search-forward yank-menu-descr-regexp (point-max) t)
	      (progn
		(delete-region (match-beginning 0) (match-end 0))
		(insert (format descr-format i))
		(end-of-line 1))
	    (setq i (1+ n)))
	  ;; (message "Renumbering remaining lines... %d" i)
	  (setq i (1+ i)))

	;; and finally trim off old stuff from the bottom

	(delete-region (point) (point-max))

	(message "Preparing yank menu buffer... done")
	(set-marker ym-hack-marker nil)

	(setq yank-menu-last-prepared-list new-last-prepared-list))
    (if yank-menu-last-prepared-list
	(progn
	  (yank-menu-remove-old-grep-marks)
	  (goto-char (point-min))
	  (replace-regexp "^m" " " nil))))
  (yank-menu-top))

(defun yank-menu-remove-equal-entry (string)
  "Remove an entry that is equal to STRING."
    (if yank-menu-last-prepared-list
	(save-window-excursion
	  (set-buffer yank-menu-buffer)
	  (save-excursion
	    (goto-char (point-min))
	    (beginning-of-line 2)
	    (let* ((this yank-menu-last-prepared-list)
		   (next (cdr this)))
	      (while this
		(if (string= string (cadr this))
		    (progn
		      (setq next (cdr next))
		      (setcdr this next)
		      (let* ((line-start (point))
			     (line-end (progn (beginning-of-line 2)
					      (point))))
			
			(delete-region line-start line-end)))
		  (progn
		    (let ((old-line (point)))
		      (beginning-of-line 2))))
		(setq this next
		      next (cdr next)))))))
    (setq yank-menu-above (delete-if-string= string yank-menu-above)
	  yank-menu-below (delete-if-string= string yank-menu-below)
	  kill-ring (delete-if-string= string kill-ring)))

;
;;;; misc bits
;

(defun member-string= (item list)
  "Look for ITEM in LIST; return first link in LIST whose car is `string=' to ITEM."
  (catch 'found
    (while list
      (if (string= item (car list))
	  (throw 'found list))
      (setq list (cdr list)))
    nil))

(defun cadr-member-string= (item list)
  "Look for ITEM in LIST; return first link in LIST whose cadr is `string=' to ITEM."
  (let ((prev list))
    (setq list (cdr prev))
    (catch 'found
      (while list
	(if (string= item (car list))
	    (throw 'found prev))
	(setq prev list
	      list (cdr list)))
      nil)))

(defun string-list-uniquify (orig)
  "Return a copy of ORIG with duplicates removed.
Each element appears only where it first appeared in ORIG.
Comparison is done with string=."
  (let ((result nil))
    (while orig
      (let ((this (car orig)))
	(if (not (member-string= this result))
	    (setq result (cons this result))))
      (setq orig (cdr orig)))
    (nreverse result)))

(defun delete-if-memq (a b)
  "Remove any memqers of A from B, returning the result."
  (while (and b (memq (car b) a))
    (setq b (cdr b)))
  (let* ((here b)			; initial "here" guaranteed not in a
	 there)
    (while here
      (setq there (cdr here))
      (while (and there (memq (car there) a))
	(setq there (cdr there)))
      (rplacd here there)
      (setq here (cdr here))))
  b)

(defun delete-if-string= (string list)
  "Remove string= occurrences of STRING from LIST"
  (delete-if (function (lambda (a) (string= a string)))
	     list))

(defun kill-ring-defontify ()
  "Strip the font stuff out of the kill-ring."
  (let ((kr kill-ring))
    (while kr
      (let ((entry (car kr)))
	(set-text-properties 0 (length entry)
			     nil entry)
	(rplaca kr (identity entry)))
      (setq kr (cdr kr)))))

;
;;;; Command loop
;

(defun yank-menu-make-prompt ()
  "Make the current prompt string."
  (let ((contains (and yank-menu-current
		       (string-match yank-menu-last-search-pattern 
				yank-menu-current
				nil))))
    (format "search %c%s%c; %sitem %d(%d bytes)/%d(%d bytes): %s"
	    (if contains ?{ ?[)
			     yank-menu-last-search-pattern
			     (if contains ?} ?])
	    (if yank-menu-additional-insertions (format "(collecting %d) " yank-menu-additional-insertions-count) "")
	    yank-menu-item-number
	    (length yank-menu-current)
	    yank-menu-max-item
	    yank-menu-total-bytes
	    yank-menu-current)))

(defvar yank-menu-edit-before-insertion t
  "*Whether to do a recursive-edit before inserting each thing.")

(defun yank-menu-really-do-insertion (insertendum)
  "Insert INSERTENDUM, possibly allowing the user to modify it first,
according to yank-menu-edit-before-insertion."
  (if yank-menu-edit-before-insertion
      (save-window-excursion
	(let ((insertion-edit-buffer (get-buffer-create "*edit insertion*")))
	  (switch-to-buffer-other-window insertion-edit-buffer)
	  (set-buffer insertion-edit-buffer)
	  (erase-buffer)
	  (insert insertendum)
	  (message (substitute-command-keys "Edit insertion; \\[exit-recursive-edit]  to insert"))
	  (save-window-excursion
	    (recursive-edit))
	  (set-buffer insertion-edit-buffer)
	  (setq insertendum (buffer-string))
	  ;; (message "Got %s" insertendum) (sit-for 2)
	  (bury-buffer))))
  (insert-before-markers insertendum))

(defun electric-yank-menu (&optional uniquify)
  "Select an item from the kill ring using a menu buffer.
Items containing more than one line are converted to a single line
by replacing each newline with the string in
yank-menu-newline-representation (qv).
With prefix arg UNIQUIFY, discard duplicate entries.
Commands available are
\\{yank-menu-map}
"
  (interactive "P")
  (if (and (boundp 'yank-menu-frame) (framep yank-menu-frame))
      (error "Yank menu is already running in its own frame -- use that instead")
    (progn
      ;; (kill-ring-defontify)
      (if uniquify
	  (let ((old-length (length kill-ring)))
	    (message "Discarding duplicates...")
	    (setq kill-ring (string-list-uniquify kill-ring))
	    (message "Discarded %d duplicates" (- old-length (length kill-ring)))))
      (let ((todo nil))
	(save-excursion
	  (save-window-excursion
	    (run-hooks 'yank-menu-entry-hooks)
	    (setq yank-menu-max-item (1- (length kill-ring))
		  yank-menu-additional-insertions nil
		  yank-menu-additional-insertions-count 0
		  yank-menu-deledendi nil
		  yank-menu-total-bytes (apply '+ (mapcar 'length kill-ring)))
	    (yank-menu-top)
	    (unwind-protect
		(progn
		  (setq yank-menu-active t)
		  (setq todo
			(catch 'yank-menu-quit
			  (save-window-excursion
			    (setq yank-menu-buffer (get-buffer-create yank-menu-buffer-name))
			    (set-buffer yank-menu-buffer)
			    (kill-all-local-variables)
			    (setq truncate-lines t
				  Helper-return-blurb "return to yank menu"
				  mode-name "Yank Menu"
				  mode-line-buffer-identification "Yank List")
			    (buffer-enable-undo yank-menu-buffer)
			    (use-local-map yank-menu-map)
			    (yank-menu-prepare-buffer kill-ring)
			    (yank-menu-display-current)
			    (Electric-pop-up-window yank-menu-buffer-name)
			    (let ((Helper-return-blurb "return to yank menu"))
			      (Electric-command-loop 'yank-menu-quit
						     'yank-menu-make-prompt
						     t))))))
	      (setq yank-menu-active nil)
	      (bury-buffer yank-menu-buffer))
	    (run-hooks 'yank-menu-exit-hooks)))
	(message "")
	(if (and (consp todo) (car todo))
	    (progn
	      (if yank-menu-additional-insertions
		  (yank-menu-really-do-insertion
		   (mapconcat 'identity 
			      (nreverse yank-menu-additional-insertions)
			      (setq yank-menu-collection-separator
				    (read-from-minibuffer "Separator for collected items: "
							  yank-menu-collection-separator
							  nil nil)))))
	      (yank-menu-really-do-insertion yank-menu-current)
	      (if yank-menu-deledendi
		  (setq kill-ring (delete-if-memq yank-menu-deledendi kill-ring)
			yank-menu-last-prepared-list 'deletions))))))))

;;; end of yank-menu.el
