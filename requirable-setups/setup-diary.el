;;; Time-stamp: <2021-11-14 18:31:27 jcgs>

;; (add-hook 'diary-mode-hook 'mulvo-decorate-words-buffer)

(setq view-diary-entries-initially t
      number-of-diary-entries [ 3 3 3 3 3 5 4 ]
      diary-display-hook 'fancy-diary-display
      list-diary-entries-hook '(include-other-diary-files sort-diary-entries)
      all-christian-calendar-holidays t
      diary-file (substitute-in-file-name "$SYNCED/var/diary")
)

;; (european-calendar)

(provide 'setup-diary)

;;; end of diary-setup.el
