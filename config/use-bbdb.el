;;; use-bbdb.el --- setup for bbdb
;;; Time-stamp: <2020-11-11 20:51:08 jcgs>

(defun string-to-int (str)
  "A deprecated function still used by this package.e
Argument STR is the string to parse as a number."
  (floor (string-to-number str)))

(jcgs/use-package bbdb
	     "$GATHERED/emacs/bbdb"
	     nil
	     ((bbdb-name "bbdb-com"
			 "Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names\)." t)
	      ;; (expand-file-name "my-extensions-to-packages/bbdb" user-emacs-directory)
	      (bbdb-insinuate-vm "bbdb-vm"
				 "Hook BBDB into VM"))
	     (setq bbdb-north-american-phone-numbers-p nil
		   bbdb-file (substitute-in-file-name "$ORG/bbdb")
		   bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
		   bbdb-ignore-most-messages-alist
		   '(("Subject" . "\\[OLRG]")
		     ("From" . "citrix")
		     )
)
	     (bbdb-initialize))

;; (add-to-list 'load-path (substitute-in-file-name "$EMACS/my-extensions-to-packages/bbdb/"))

;;; use-bbdb.el ends here
