;;; use-bbdb.el --- setup for bbdb
;;; Time-stamp: <2014-01-03 08:56:30 jcgs>

(use-package bbdb
	     "$GATHERED/emacs/bbdb"
	     nil
	     ((bbdb-name "bbdb-com"
			 "Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names\)." t)
	      (expand-file-name "my-extensions-to-packages/bbdb" user-emacs-directory)
	      (bbdb-insinuate-vm "bbdb-vm"
				 "Hook BBDB into VM"))
	     (setq bbdb-north-american-phone-numbers-p nil
		   bbdb-file (substitute-in-file-name "$DROPBOX/org/bbdb")
		   bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
		   bbdb-ignore-most-messages-alist
		   '(("Subject" . "\\[OLRG]")
		     ("From" . "citrix")
		     )
)
	     (bbdb-initialize))

;;; use-bbdb.el ends here
