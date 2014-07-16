;;;; jcgs-bindings.el -- set up JCGS' key bindings
;;; Time-stamp: <2014-07-04 16:19:40 johstu01>

(add-to-list 'load-path (expand-file-name "convenience" user-emacs-directory))

(autoload 'smart-repeat-complex-command "smart-repeat" 
  "Like repeat-complex-command, but may skip the first one if it would do nothing."
  t)

(autoload 'other-window-or-buffer "other" nil t)
(autoload 'other-window-backwards "other" nil t)

(add-to-list 'load-path (expand-file-name "file-handling" user-emacs-directory))

(autoload 'other-window-file-name "file-name-insertions"
    "Insert at point the name of the file in the next window.
With optional (prefix) argument, insert only the non-directory part of the name.
Particularly useful in a shell window."
    t)

(autoload 'other-window-directory-name "file-name-insertions"
  "Insert at point the name of the directory of the file in the next window.
Particularly useful in a shell window."
  t)

(defun save-all-buffers-no-ask ()
  "Save all buffers, without prompting for each one."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers"))

(defun keypad-separate ()
  "Remove mappings of keypad keys."
  (interactive)
  (let* ((holder (cons nil (cdr function-key-map)))
	 (pairs holder)
	 (nextpairs (cdr pairs)))
    (while pairs
      (let* ((pair (car nextpairs))
	     (name (car pair)))
	(if (and (symbolp name)
		 (string-match "kp-" (symbol-name name)))
	    (progn
	      (message "Unmapping %S from function-key-map" name)
	      (rplacd pairs (cdr nextpairs))
	      (setq nextpairs (cdr pairs)))
	  (progn
	    (setq pairs (cdr pairs)
		  nextpairs (cdr pairs))))))
    (rplacd function-key-map (cdr holder))))

(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\C-y" 'browse-yank)

(defun grep-this (command-args)
  "Run grep on the current symbol.
Argument COMMAND-ARGS are the args."
  (interactive
   (progn
     (grep-compute-defaults)
     (list (grep-default-command))))
  (grep command-args))

(defvar jcgs-map (make-keymap)
  "Keymap for my first rank of custom commands.")

(fset 'jcgs-map jcgs-map)

(defvar jcgs-map-1 (make-keymap)
  "Keymap for my second rank of custom commands.")

(fset 'jcgs-map-1 jcgs-map-1)

(defun jcgs-map-help ()
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (prin1 (substitute-command-keys
	    "Bindings for jcgs-map are \\{jcgs-map}"))))

(defun jcgs-map-1-help ()
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (prin1 (substitute-command-keys
	    "Bindings for jcgs-map-1 are \\{jcgs-map-1}"))))

(defvar jcgs-magit-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'magit-status)
    map)
  "Keymap for magit entry points.")

(fset 'jcgs-magit-commands-map jcgs-magit-commands-map)

(defun jcgs-keys:setup-jcgs-map ()
  "Set up my personal keymaps."
  (interactive)
  (define-key jcgs-map "?" 'jcgs-map-help)
  (define-key jcgs-map "." 'choose-tags-file)
  (define-key jcgs-map "1" 'in-one-window)
  (define-key jcgs-map ":" 'repeat-matching-complex-command)
  (define-key jcgs-map ";" 'kill-comment)
  (define-key jcgs-map "B" 'bury-matching-buffers)
  (define-key jcgs-map "C" 'compare-windows-interactively)
  (define-key jcgs-map "[" 'wander-yank)
  (define-key jcgs-map "]" 'pick-up-sexp-at-point)
  (define-key jcgs-map "b" 'bury-buffer)
  (define-key jcgs-map "c" 'wander-safe)
  (define-key jcgs-map "d" 'other-window-directory-name)
  (define-key jcgs-map "f" 'other-window-file-name)
  (define-key jcgs-map "g" 'jcgs-magit-commands-map)
  (define-key jcgs-map "h" 'electric-command-history)
  (define-key jcgs-map "i" 'eval-insert)
  (define-key jcgs-map "j" 'imenu)
  (define-key jcgs-map "k" 'kill-matching-buffers-no-ask)
  (define-key jcgs-map "m" 'bring-up-buffers-matching-file)
  (define-key jcgs-map "n" 'recursive-narrow)
  (define-key jcgs-map "r" 'replace-regexp)
  (define-key jcgs-map "s" 'replace-string)
  (define-key jcgs-map "t" 'insert-tag-name)
  (define-key jcgs-map "w" 'write-region-and-find-it)
  (define-key jcgs-map "x" 'swap-windows)
  (define-key jcgs-map "y" 'browse-yank)
  (define-key jcgs-map "z" 'jcgs-map-1)

  (define-key jcgs-map-1 "?" 'jcgs-map-1-help)
  (define-key jcgs-map-1 ";" 'box-comment)
  (define-key jcgs-map-1 "c" 'compare-windows)
  (define-key jcgs-map-1 "d" 'other-window-directory-name)
  (define-key jcgs-map-1 "e" 'comment-straighten-right-edge)
  (define-key jcgs-map-1 "k" 'insert-key-command)
  (define-key jcgs-map-1 "l" 'load-other-window-file-name)
  (define-key jcgs-map-1 "q" 'revert-quickly)
  (define-key jcgs-map-1 "r" 'remember)
  )

(add-to-list 'load-path (expand-file-name "startup" user-emacs-directory))

(autoload 'bring-up-buffers-matching-file "buffers"
  "Bring up buffers whose names match REGEXP.
Returns how many buffers it brought up." t)

(defun jcgs-keys:jcgs-function-keys ()
  "Set up John's function keys."
  (interactive)

  (global-set-key "\C-x\M-2" '2C-mode-map)

  (jcgs-keys:setup-jcgs-map)

  (global-set-key [   C-f1 ] 'delete-other-windows)
  (global-set-key [ C-M-f1 ] 'delete-window)

  (global-set-key [     f2 ] 'other-window)
  (global-set-key [   C-f2 ] 'split-window-horizontally)

  (global-set-key [     f3 ] 'kill-ring-save)
  (global-set-key [   C-f3 ] 'kill-region)
  (global-set-key [   M-f3 ] 'yank)
  (global-set-key [ C-M-f3 ] 'delete-region)

  (global-set-key [     f4 ] 'other-frame)

  (global-set-key [     f5 ] 'find-file-at-point)
  (global-set-key [   C-f5 ] 'find-file-at-point)
  (global-set-key [ M-C-f5 ] 'find-file-at-point-other-window)
  (global-set-key [   M-f5 ] 'find-file-other-window)

  (global-set-key [     f6 ] 'switch-to-buffer)
  (global-set-key [   C-f6 ] 'electric-buffer-list)
  (global-set-key [   M-f6 ] 'switch-to-buffer-other-window-beside)

  (global-set-key [     f7 ] 'jcgs-map)
  (global-set-key [   C-f7 ] 'bury-buffer)
  (global-set-key [   M-f7 ] 'jcgs-map-1)

  (global-set-key [     f8 ] 'other-window-or-buffer)
  (global-set-key [   C-f8 ] 'other-window-backwards)

  (global-set-key [     f9 ] ctl-x-map)
  (global-set-key [   C-f9 ] ctl-x-4-map)

  (global-set-key [     f10 ] 'execute-extended-command)
  (global-set-key [   M-f10 ] 'eval-defun)

  (global-set-key [     f11 ] 'save-all-buffers-no-ask)
  (global-set-key [   M-f11 ] 'type-break)

  (global-set-key [     f12 ] 'smart-repeat-complex-command)
  (define-key minibuffer-local-map [ f12 ] 'previous-history-element)
  (define-key minibuffer-local-map [ C-f12 ] 'next-history-element)
  (global-set-key [   C-f12 ] 'repeat-matching-complex-command))

(jcgs-keys:jcgs-function-keys)

;;; end of jcgs-bindings.el