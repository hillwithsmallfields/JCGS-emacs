;;; Time-stamp: <2006-10-31 15:21:59 john>

;; (add-hook 'diary-mode-hook 'mulvo-decorate-words-buffer)

(setq view-diary-entries-initially t
      number-of-diary-entries [ 3 3 3 3 3 5 4 ]
      diary-display-hook 'fancy-diary-display
      list-diary-entries-hook '(include-other-diary-files sort-diary-entries)
      all-christian-calendar-holidays t
      diary-file (substitute-in-file-name "$COMMON/var/diary")
)

;; (european-calendar)

(provide 'setup-diary)

;;; end of diary-setup.el
