;;; use-bbdb.el --- setup for bbdb
;;; Time-stamp: <2016-12-12 22:52:09 jcgs>

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

(add-to-list 'load-path (substitute-in-file-name "$EMACS/my-extensions-to-packages/bbdb/"))

;;; use-bbdb.el ends here
