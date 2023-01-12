;;;; jcgs-bindings.el -- set up JCGS' key bindings
;;; Time-stamp: <2022-06-14 20:24:42 jcgs>

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

(autoload 'find-place "find-line-near-point"
  "Find a buffer visiting the file before WHERE, at line and column.
Looks in the buffers you are visiting, as well trying the
filename as such, as the filename may be relative to the wrong
directory."
  t)

(defun save-all-buffers-no-ask ()
  "Save all buffers, without prompting for each one."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers"))

(defun keypad-separate ()
  "Remove mappings of keypad keys."
  (interactive)
  (keypad-setup 'none)
  ;; (let* ((holder (cons nil (cdr function-key-map)))
  ;; 	 (pairs holder)
  ;; 	 (nextpairs (cdr pairs)))
  ;;   (while pairs
  ;;     (let* ((pair (car nextpairs))
  ;; 	     (name (car pair)))
  ;; 	(if (and (symbolp name)
  ;; 		 (string-match "kp-" (symbol-name name)))
  ;; 	    (progn
  ;; 	      (message "Unmapping %S from function-key-map" name)
  ;; 	      (rplacd pairs (cdr nextpairs))
  ;; 	      (setq nextpairs (cdr pairs)))
  ;; 	  (progn
  ;; 	    (setq pairs (cdr pairs)
  ;; 		  nextpairs (cdr pairs))))))
  ;;   (rplacd function-key-map (cdr holder)))
  )

(defun universal-argument-n (arg n)
  "Set or multiply the prefix ARG by a fixed factor of N."
  (prefix-command-preserve-state)
  (setq prefix-arg (cond ((integerp arg)
                          (* arg n))
                         (t n))))

(defun universal-prefix-two (arg)
  "Set or multiply the prefix ARG by two."
  (interactive "P")
  (universal-argument-n arg 2))

(defun universal-prefix-three (arg)
  "Set or multiply the prefix ARG by three."
  (interactive "P")
  (universal-argument-n arg 3))

(defun universal-prefix-five (arg)
  "Set or multiply the prefix ARG by five."
  (interactive "P")
  (universal-argument-n arg 5))

(defun toggle-narrow-to-defun (&optional include-comments)
  "Toggle whether we are narrowed to a defun."
  (interactive (list narrow-to-defun-include-comments))
  (let ((begin (point-min))
        (end (point-max)))
    (if (save-restriction
          (widen)
          (and (= (point-min) begin)
               (= (point-max) end)))
        (narrow-to-defun include-comments)
      (widen))))

(global-set-key "\M-\"" 'insert-quotes)
(global-set-key "\M-[" 'insert-square-brackets)

(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\C-y" 'browse-yank)
(global-set-key "\C-cd" 'ediff-buffers)
(global-set-key "\C-cf" 'find-file-at-point)
(global-set-key "\C-cl" 'find-place)
(global-set-key "\C-cL" 'find-place-forward)
(global-set-key "\C-cx" 'finances-enter) ; mnemonic eXpenditure
(global-set-key "\C-x\M-o" 'other-frame)

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

(defvar jcgs-task-tracking-map
  (make-sparse-keymap "Task tracking")
  "Keymap for my task tracking.")

(fset 'jcgs-task-tracking-map jcgs-task-tracking-map)

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
  (define-key jcgs-map "c" 'wander)
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
  (define-key jcgs-map "\C-t" 'toggle-truncate-lines)
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

  ;; (eval-after-load "org-mode"
  ;;   (define-key org-mode-map "\C-cb" 'jcgs/org-buy-for-project-and-block))
  
  (define-key jcgs-task-tracking-map "a" 'jcgs/org-start-answering)
  (define-key jcgs-task-tracking-map "b" 'jcgs/org-start-break-or-browsing)
  (define-key jcgs-task-tracking-map "c" 'org-capture)
  (define-key jcgs-task-tracking-map "e" 'jcgs/org-start-emacs)
  (define-key jcgs-task-tracking-map "h" 'jcgs/org-help-task-keys)
  (define-key jcgs-task-tracking-map "i" 'org-clock-in)
  (define-key jcgs-task-tracking-map "j" 'jcgs/org-jcgs-task)
  (define-key jcgs-task-tracking-map "l" 'org-clock-in-last)
  (define-key jcgs-task-tracking-map "p" 'jcgs/org-start-paperwork)
  (define-key jcgs-task-tracking-map "q" 'jcgs/org-start-asking)
  (define-key jcgs-task-tracking-map "r" 'jcgs/org-resume-creative)
  (define-key jcgs-task-tracking-map "u" 'jcgs/org-start-background-reading)
  (define-key jcgs-task-tracking-map "v" 'jcgs/org-start-reviewing)
  (define-key jcgs-task-tracking-map "z" 'org-clock-out)
  (define-key jcgs-task-tracking-map "?" 'jcgs/org-show-last-creative-task)
  )

(add-to-list 'load-path (expand-file-name "startup" user-emacs-directory))

(autoload 'bring-up-buffers-matching-file "buffers"
  "Bring up buffers whose names match REGEXP.
Returns how many buffers it brought up." t)

(defun jcgs-keys:jcgs-function-keys ()
  "Set up John's function keys."
  (interactive)

  (global-set-key "\C-x\M-2" '2C-mode-map)

  (global-set-key [   C-f1 ] 'delete-other-windows)
  (global-set-key [ C-M-f1 ] 'delete-window)

  ;; (global-set-key [     f2 ] 'other-window)
  ;; (global-set-key [   C-f2 ] 'split-window-horizontally)

  (global-set-key [     f3 ] 'other-frame)

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

  (global-set-key [     f8 ] 'jcgs-task-tracking-map)
  (eval-after-load "org-mode"
    '(define-key org-mode-map [ C-f8 ] 'jcgs-task-tracking-map))

  (global-set-key [     f9 ] ctl-x-map)
  (global-set-key [   C-f9 ] ctl-x-4-map)

  (global-set-key [     f10 ] 'execute-extended-command)
  (global-set-key [   M-f10 ] 'eval-defun)

  (global-set-key [     f11 ] 'save-all-buffers-no-ask)
  (global-set-key [   M-f11 ] 'type-break)

  (global-set-key [     f12 ] 'smart-repeat-complex-command)
  (define-key minibuffer-local-map [ f12 ] 'previous-history-element)
  (define-key minibuffer-local-map [ C-f12 ] 'next-history-element)
  (global-set-key [   C-f12 ] 'repeat-matching-complex-command)
  (global-set-key "\C-cr" 'run-and-display))

(defun jcgs-keys:setup-default-keyboardio ()
  "Add some keys for my keyboardio.
This copies some awkward M- bindings to C-."
  (interactive)
  (global-set-key [ C-< ] 'beginning-of-buffer)
  (global-set-key [ C-> ] 'end-of-buffer))

(defun jcgs-keys:setup-super-map ()
  "Bind some keys on the super modifier."
  (interactive)
  (global-set-key (kbd "s-a") 'beginning-of-defun)
  (global-set-key (kbd "s-e") 'end-of-defun)
  (global-set-key (kbd "s-b") 'backward-sexp)
  (global-set-key (kbd "s-f") 'forward-sexp)
  (global-set-key (kbd "s-u") 'backward-up-list)
  (global-set-key (kbd "s-d") 'down-list)
  (global-set-key (kbd "s-s") 'isearch-forward-regexp)
  (global-set-key (kbd "s-r") 'isearch-backward-regexp)
  (global-set-key (kbd "s-q") 'insert-quotes)
  (global-set-key (kbd "s-z") 'raise-sexp) ; z for zoom
  (global-set-key (kbd "s-(") 'insert-parentheses)
  (global-set-key (kbd "s-[") 'wander-yank)
  (global-set-key (kbd "s-]") 'pick-up-sexp-at-point)
  (global-set-key (kbd "s-{") 'wander)
  (global-set-key (kbd "s-}") 'exit-recursive-edit)
  (global-set-key (kbd "s-n") 'sexp-preceding-next-parenthesis)
  (global-set-key (kbd "s-o") 'other-window))

(defvar jcgs-grid-upper-map (make-keymap)
  "Keymap for the upper half of my grid keyboard.")

(fset 'jcgs-grid-upper-map jcgs-grid-upper-map)

(defvar jcgs-grid-lower-map (make-keymap)
  "Keymap for the lower half of my grid keyboard.")

(fset 'jcgs-grid-lower-map jcgs-grid-lower-map)

(defvar jcgs-org-grid-upper-map (make-keymap)
  "Keymap for the upper half of my grid keyboard in org-mode.")

(fset 'jcgs-org-grid-upper-map jcgs-org-grid-upper-map)

(defvar jcgs-org-grid-lower-map (make-keymap)
  "Keymap for the lower half of my grid keyboard in org-mode.")

(fset 'jcgs-org-grid-lower-map jcgs-org-grid-lower-map)

(defun jcgs-keys:setup-grid-keyboard-map ()
  "Bind the keys for the grid command keyboard."
  (interactive)
  (global-set-key [ S-f2 ] 'jcgs-grid-upper-map)

  (define-key jcgs-grid-upper-map "A" 'transpose-sexp)
  (define-key jcgs-grid-upper-map "B" 'copy-sexp)
  (define-key jcgs-grid-upper-map "C" 'kill-sexp)
  (define-key jcgs-grid-upper-map "D" 'mark-sexp)

  (define-key jcgs-grid-upper-map "I" 'backward-up-list)
  (define-key jcgs-grid-upper-map "J" 'backward-sexp)
  (define-key jcgs-grid-upper-map "K" 'forward-sexp)
  (define-key jcgs-grid-upper-map "L" 'down-list)

  (define-key jcgs-grid-upper-map "Q" 'toggle-narrow-to-defun)
  (define-key jcgs-grid-upper-map "R" 'beginning-of-defun)
  (define-key jcgs-grid-upper-map "S" 'end-of-defun)
  (define-key jcgs-grid-upper-map "T" 'mark-defun)

  (define-key jcgs-grid-upper-map "E" 'delete-backward-char)
  (define-key jcgs-grid-upper-map "F" 'delete-blank-lines)
  (define-key jcgs-grid-upper-map "G" 'just-one-space)
  (define-key jcgs-grid-upper-map "H" 'delete-horizontal-space)

  (define-key jcgs-grid-upper-map "M" 'negative-argument)
  (define-key jcgs-grid-upper-map "N" 'universal-prefix-two)
  (define-key jcgs-grid-upper-map "O" 'universal-prefix-three)
  (define-key jcgs-grid-upper-map "P" 'universal-prefix-five)

  (define-key jcgs-grid-upper-map "U" 'undo)
  (define-key jcgs-grid-upper-map "V" 'smart-repeat-complex-command)
  (define-key minibuffer-local-map [ S-f2 V ] 'previous-history-element)
  (define-key jcgs-grid-upper-map "W" 'eval-defun)
  (define-key jcgs-grid-upper-map "X" 'newline)

  (global-set-key [ f2 ] 'jcgs-grid-lower-map)
  
  (define-key jcgs-grid-lower-map "a" 'next-parentheses-type)
  (define-key jcgs-grid-lower-map "b" 'raise-sexp)
  (define-key jcgs-grid-lower-map "c" 'insert-parentheses)
  (define-key jcgs-grid-lower-map "d" 'insert-quotes)

  (define-key jcgs-grid-lower-map "i" 'wander-yank)
  (define-key jcgs-grid-lower-map "j" 'wander)
  (define-key jcgs-grid-lower-map "k" 'exit-recursive-edit)
  (define-key jcgs-grid-lower-map "l" 'pick-up-sexp-at-point)

  (define-key jcgs-grid-lower-map "q" 'kill-region)
  (define-key jcgs-grid-lower-map "r" 'kill-ring-save)
  (define-key jcgs-grid-lower-map "s" 'yank)
  (define-key jcgs-grid-lower-map "t" 'yank-pop)

  (define-key jcgs-grid-lower-map "e" 'backward-sentence)
  (define-key jcgs-grid-lower-map "f" 'backward-word)
  (define-key jcgs-grid-lower-map "g" 'forward-word)
  (define-key jcgs-grid-lower-map "h" 'forward-sentence)

  (define-key jcgs-grid-lower-map "m" 'electric-buffer-list)
  (define-key jcgs-grid-lower-map "n" 'next-buffer)
  (define-key jcgs-grid-lower-map "o" 'other-window)
  (define-key jcgs-grid-lower-map "p" 'other-frame)

  (define-key jcgs-grid-lower-map "u" 'beginning-of-buffer)
  (define-key jcgs-grid-lower-map "v" 'move-in-or-out-of-string)
  (define-key jcgs-grid-lower-map "w" 'sexp-preceding-next-parenthesis)
  (define-key jcgs-grid-lower-map "x" 'end-of-buffer)

  (require 'org)
  (define-key org-mode-map [ S-f2 ] 'jcgs-org-grid-upper-map)
  (define-key org-mode-map [ f2 ] 'jcgs-org-grid-lower-map)

  (define-key jcgs-org-grid-upper-map "A" 'org-ctrl-c-ctrl-c)
  (define-key jcgs-org-grid-upper-map "B" 'org-metaup)
  (define-key jcgs-org-grid-upper-map "C" 'org-metadown)
  (define-key jcgs-org-grid-upper-map "D" 'org-todo)

  (define-key jcgs-org-grid-upper-map "I" 'outline-up-heading)
  (define-key jcgs-org-grid-upper-map "J" 'org-backward-heading-same-level)
  (define-key jcgs-org-grid-upper-map "K" 'org-forward-heading-same-level)
  (define-key jcgs-org-grid-upper-map "L" 'next-line)

  (define-key jcgs-org-grid-upper-map "Q" 'org-shiftup)
  (define-key jcgs-org-grid-upper-map "R" 'org-shiftleft)
  (define-key jcgs-org-grid-upper-map "S" 'org-shiftright)
  (define-key jcgs-org-grid-upper-map "T" 'org-shiftdown)

  (define-key jcgs-org-grid-lower-map "a" 'org-demote-subtree)
  (define-key jcgs-org-grid-lower-map "b" 'org-demote)
  (define-key jcgs-org-grid-lower-map "c" 'org-promote)
  (define-key jcgs-org-grid-lower-map "d" 'org-promote-subtree)

  (define-key jcgs-org-grid-lower-map "g" 'org-clock-in)
  (define-key jcgs-org-grid-lower-map "h" 'org-clock-out)

  (define-key jcgs-org-grid-lower-map "m" 'org-set-property)

  (define-key jcgs-org-grid-lower-map "w" 'org-insert-heading)
  (define-key jcgs-org-grid-lower-map "x" 'org-insert-todo-heading)
  )

(require 'structure-edit)
(jcgs-keys:jcgs-function-keys)
(jcgs-keys:setup-jcgs-map)
(jcgs-keys:setup-super-map)

;;; end of jcgs-bindings.el
