;;;; Emacs setup for scraping transactions from my online banking page
;;; Time-stamp: <2014-10-01 21:24:38 jcgs>

(load-file "$HOME/Dropbox/emacs/basics/jcgs-common-setup.el")

(load-file (substitute-in-file-name "$GATHERED/emacs/csv/csv-mode.el"))
(load-file (substitute-in-file-name "$GATHERED/emacs/csv/csv.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Co-op bank scrape to spreadsheet ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spending Tracker exported transactions file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst finances-main-spreadsheet-columns-order
  '(
    ("Date" "Date")
    ("Item" "Note")
    ("Type" "Category")
    ("Provider" nil)
    ("Location" nil)
    ("Review" nil)
    ("Type code" nil)
    ("" nil)
    ("Total payment" nil)
    ("Cash paid" Cash)
    ("VISA" Handelsbanken\ via\ VISA)
    ("Paypal" Handelsbanken\ via \PayPal)
    ("Direct" Handelsbanken)
    ("Withdrawal" nil)
    ("VISA" Co-op\ via\ VISA)
    ("Paypal" Co-op\ via\ PayPal)
    ("Direct" Co-op)
    ("Withdrawal" nil)
    ("From paypal account" nil)
    ("Direct into account" nil)
    ("Direct into account" nil)
    )
  )

(defun finances-convert-transactions (raw-file cumulative-file)
  "Convert RAW-FILE to CUMULATIVE-FILE"
  ;; 
  (find-file raw-file)
  (let* ((csv-unquoted-entry-regexp "\\(^\\|,\\)\\s-*\\([^,\n]*\\)\\s-*\\(,\\|,?$\\)")
	 (transactions (csv-parse-buffer)))
    (save-excursion
      (find-file cumulative-file)
      (erase-buffer)
      (dolist (transaction transactions)
	(let ((account (intern (cdr (assoc "Account" transaction))))
	      (amount (cdr (assoc "Amount" transaction))))
	  (setq amount (if (string-match "\\`-" amount)
			   (substring amount 1)
			 (concat "-" amount)))
	  (dolist (cell finances-main-spreadsheet-columns-order)
	    (let* ((origin (cadr cell))
		   (output (cond
			    ((stringp origin)
			     (cdr (assoc origin transaction)))
			    ((symbolp origin)
			     (if (eq origin account)
				 amount
			       nil))
			    (t nil))))
	      (when output
		(insert output))
	      (insert ",")))
	  (insert "\n")))
      (basic-save-buffer))))

(let ((raw-file (substitute-in-file-name "$DROPBOX/transactions/Transactions.csv"))
      (cumulative-file (expand-file-name (format "~/finances/%4d/cumulative-transactions.csv"
						  (nth 5 (decode-time))))))
  (finances-convert-transactions raw-file cumulative-file)
  (find-file-other-window cumulative-file)
  (goto-char (point-max)))

;;; financial-emacs.el ends here
