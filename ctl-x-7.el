;;; Old stamp <90/09/11 22:47:37 john>
;;; Time-stamp: <2007-04-22 15:52:16 jcgs>

(require 'buffers) ; for M-x bury-this-buffer
(provide 'ctl-x-7)

(defvar ctl-x-7-map (make-keymap)
  "Keymap for C-x 7 (my first rank of custom commands).")

(fset 'ctl-x-7-map ctl-x-7-map)

(define-key ctl-x-map "7" 'ctl-x-7-map)

(defvar ctl-x-7-4-map (make-keymap)
  "Keymap for C-x 3-4 (a sub-rank of my first rank of custom commands).")

(fset 'ctl-x-7-4-map ctl-x-7-4-map)

(define-key ctl-x-7-map "4" 'ctl-x-7-4-map)

(defvar ctl-x-7-3-map (make-keymap)
  "Keymap for C-x 3-3 (a sub-rank of my first rank of custom commands).")

(fset 'ctl-x-7-3-map ctl-x-7-3-map)

(define-key ctl-x-7-map "3" 'ctl-x-7-3-map)

(defun ctl-x-help ()
  "Display a map of the \\C-x range of commands."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (prin1 (substitute-command-keys
	    "Bindings for \\=\\C-x are \\{ctl-x-map}"))))

(defun ctl-x-7-help ()
  "Display a map of the \\C-x 7 range of commands."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (prin1 (substitute-command-keys
	    "Bindings for \\=\\C-x 7 are \\{ctl-x-7-map}"))))

(defun ctl-x-7-3-help ()
  "Display a map of the \\C-x 7 3 range of commands."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (prin1 (substitute-command-keys
	    "Bindings for \\=\\C-x 7 3 are \\{ctl-x-7-3-map}"))))

(defun ctl-x-7-4-help ()
  "Display a map of the \\C-x 7 4 range of commands."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (prin1 (substitute-command-keys
	    "Bindings for \=C-x 7 4 are \\{ctl-x-7-4-map}"))))

;; (global-set-key "\C-x?" 'ctl-x-help)
(global-set-key "\C-x7?" 'ctl-x-7-help)
(global-set-key "\C-x73?" 'ctl-x-7-3-help)
(global-set-key "\C-x74?" 'ctl-x-7-4-help)
(global-set-key "\C-x74b" 'bring-up-buffers-matching-file)
(global-set-key "\C-x71" 'in-one-window)
(global-set-key "\C-x7;" 'kill-comment)
(global-set-key "\C-x7." 'choose-tags-file)
(global-set-key "\C-x7[" 'wander-yank)
(global-set-key "\C-x7]" 'pick-up-sexp-at-point)
(global-set-key "\C-x7\C-b" 'bury-this-buffer)
(global-set-key "\C-x7f" 'frame-jump)
(global-set-key "\C-x73;" 'box-comment)
(global-set-key "\C-x74;" 'comment-straighten-right-edge)
(global-set-key "\C-x73b" 'vb-do-buffer-realization)
(global-set-key "\C-x73\C-b" 'vb-list-vbuffers)
(global-set-key "\C-x73c" 'compare-windows)
(global-set-key "\C-x74c" 'compare-windows-interactively)
(global-set-key "\C-x73d" 'other-window-directory-name)
(global-set-key "\C-x7f" 'other-window-file-name)
(global-set-key "\C-x7d" 'other-window-directory-name)
(global-set-key "\C-x7j" 'imenu)
(global-set-key "\C-x73k" 'insert-key-command)
(global-set-key "\C-x73l" 'load-other-window-file-name)
(global-set-key "\C-x7m" 'insert-mail-alias)
(global-set-key "\C-x73p" 'plan)
(global-set-key "\C-x73q" 'revert-quickly)
(global-set-key "\C-x7\C-k" 'bury-matching-buffers)
(global-set-key "\C-x7\C-x" 'swap-windows)
(global-set-key "\C-x7g" 'goto-line)
(global-set-key "\C-x7i" 'eval-insert)
(global-set-key "\C-x7k" 'kill-matching-buffers)
(global-set-key "\C-x7l" 'lookup-tag)
(global-set-key "\C-x7n" 'recursive-narrow)
(global-set-key "\C-x7r" 'replace-regexp)
(global-set-key "\C-x73r" 'remember)
(global-set-key "\C-x7s" 'replace-string)
(global-set-key "\C-x7:" 'repeat-matching-complex-command)
(global-set-key "\C-x7\C-s" 'save-all-buffer-entries)
(global-set-key "\C-x7\C-y" 'browse-yank)
(global-set-key "\C-x7c" 'wander-safe)
(global-set-key "\C-x7h" 'electric-command-history)
(global-set-key "\C-x7t" 'insert-tag-name)
(global-set-key "\C-x73t" 'planner-create-task-from-buffer)
(global-set-key "\C-x73T" 'planner-create-task)
(global-set-key "\C-x7w" 'write-region-and-find-it)

;; (defvar ctl-x-8-map (make-keymap)
;;   "Keymap for C-x 8 (my second rank of custom commands).")

;; (fset 'ctl-x-8-map ctl-x-8-map)

;; (define-key ctl-x-map "8" 'ctl-x-8-map)

;; (global-set-key "\C-x8b" 'switch-to-buffer-other-window-beside)
;; (global-set-key "\C-x8\C-f" 'find-file-other-window-beside)
;; (global-set-key "\C-x8." 'find-tag-other-window-beside)

;;; end of ctl-x-7.el
