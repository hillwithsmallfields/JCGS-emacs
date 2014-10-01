;;;; Emacs setup for scraping transactions from my online banking page
;;; Time-stamp: <2014-10-01 08:51:20 jcgs>

(load-file "$HOME/Dropbox/emacs/basics/jcgs-common-setup.el")

(load-file (substitute-in-file-name "$GATHERED/emacs/csv/csv-mode.el"))

(defun finances-co-op-import ()
  "Import from Co-op web page section copy."
  (interactive)
  (goto-char (point-max))
  (unless (eolp)
    (insert "\n"))
  (when (and (bolp) (eolp))
    (insert ",\n"))
  (let ((start (point)))
    (yank)
    (insert "\n")
    (let ((end (point-marker)))
      (reverse-region start end)
      (finances-columns-tidy))))

(defun finances-insert-balance-line ()
  "Insert a line for the balance."
  (interactive)
  (goto-char (point-max))
  (unless (eolp)
    (insert "\n"))
  (when (and (bolp) (eolp))
    (insert ",\n"))
  (insert "balance, ")
  (yank)
  (insert "\n"))

(defvar finances-spreadsheet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'finances-columns-tidy)
    (define-key map "\C-c\C-o" 'finances-co-op-import)
    (define-key map "\C-c\C-s" 'finances-split-by-days)
    (define-key map "\C-c\C-r" 'reverse-region)
    (define-key map "\C-c\C-b" 'finances-insert-balance-line)
    map)
  "Keymap for my finances transfer spreadsheet.")

(define-derived-mode finances-spreadsheet-mode csv-mode "Finances spreadsheet"
  "Major mode for my finances transfer spreadsheet.")

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("update\\.[Cc][Ss][Vv]\\'" . finances-spreadsheet-mode))

(load-file (expand-file-name "information-management/finances-columns.el" user-emacs-directory))

(find-file (substitute-in-file-name "$HOME/finances/update.csv"))
(goto-char (point-max))

(find-file-other-window (substitute-in-file-name "$DROPBOX/transactions/Transactions.csv"))
(goto-char (point-max))

;;; financial-emacs.el ends here
