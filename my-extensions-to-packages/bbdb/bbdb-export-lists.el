;;;; bbdb-export-lists.el -- output various lists from my bbdb
;;; started by John Sturdy, 1999-01-07
;;; Time-stamp: <2021-11-14 18:24:30 jcgs>

(provide 'bbdb-export-lists)
(require 'bbdb-iterators)
(require 'bbdb-human-names)
(require 'bbdb-com)

(defun bbdb:insert-phone-numbers (phones &optional column)
  "Insert PHONES, sensibly formatted."
  (if phones
      (progn
	(when (> (length phones) 1)
	  (setq phones (sort phones
			     (function
			      (lambda (a b)
				(or (string= (aref a 0) "home")
				    (string< (aref a 0) (aref b 0))))))))
	(let* ((first-phone (car phones)))
	  (insert (if (or (string= (aref first-phone 0) "home")
			  (null (cdr phones)))
		      (format "%s\n" (aref first-phone 1))
		    (format "%s (%s)\n" (aref first-phone 1) (aref first-phone 0))))
	  (when (cdr phones)
	    (dolist (further-phone (cdr phones))
	      (when column
		(insert (make-string column ? )))
	      (insert
	       (format "%s (%s)\n"
		       (aref further-phone 1) (aref further-phone 0)))))))
    
    (insert "\n")))

(defun max-slot-length (all slot)
  "Return the maximum length, in any of ALL, at the array reference SLOT."
  (apply 'max
	 (mapcar (function
		  (lambda (person)
		    (length (aref person slot))))
		 all)))

(defun bbdb:make-phone-list ()
  "Make my phone list."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Phone list*"))
  (erase-buffer)
  (let ((phone-lists nil))
    (bbdb:apply-to-records-defining
     (function
      (lambda (record)
	(let ((phone (copy-sequence (bbdb-record-phones record)))
	      (phone-list (bbdb-record-getprop record 'phone-list)))
	  (when (and phone phone-list)
	    (let ((which-list (assoc phone-list phone-lists)))
	      (unless which-list
		(setq which-list (cons phone-list nil))
		(push which-list phone-lists))
	      (push (vector (bbdb-record-title-and-name record) phone)
		    (cdr which-list)))))
	nil))
     'phone-list)
    (setq phone-lists (sort phone-lists 
			    (function (lambda (a b)
					(string< (car a) (car b))))))
    (dolist (phone-list phone-lists)
      (rplacd phone-list (sort (merged-people-list (cdr phone-list) 0 1)
			       (function (lambda (a b)
					   (surname< (aref a 0) (aref b 0)))))))
    (dolist (phone-list phone-lists)
      (insert (capitalize (car phone-list)) ":\n")
      (let* ((maxnamelen (max-slot-length (cdr phone-list) 0))
	     (indent (+ 3 maxnamelen))
	     (nameformat (format " %%%ds: " maxnamelen)))
	(dolist (person (cdr phone-list))
	  (insert (format nameformat (aref person 0)))
	  (bbdb:insert-phone-numbers (aref person 1) indent)))
      (insert  "\n")))
  (goto-char (point-min)))

(defun car-string< (a b) (string< (car a) (car b)))

(defun insert-distlist (distlist)
  "Insert a distribution list"
  (let* ((listname (car distlist))
	 (members (sort (cdr distlist) 'string<))
	 (short-length (+ (length listname)
			  7
			  (apply '+ (mapcar 'length members))
			  (* (length members) 2)))
	 (infill (if (< short-length 77)
		     ", "
		   (concat ", \\\n" (make-string (+ 7 (length listname)) ? )))))
    (let* ((whole-thing (concat
			 (format "alias %s " listname)
			 (mapconcat 'identity members infill)
			 "\n\n"
			 ))
	   (whole-length (length whole-thing))
	   )
      (if (<= whole-length 1000)
	  (insert whole-thing)
	(let* ((n-groups (/ whole-length 512))
	       (per-group (/ (length members) n-groups))
	       (group nil)
	       (count 0)
	       )
	  (rplacd distlist nil)
	  (while members
	    (if (>= (length members) per-group)
		(setq group (subseq members 0 per-group)
		      members (subseq members per-group))
	      (setq group members
		    members nil))
	    (let* ((name (format "%s-part%d" listname count))
		   )
	      (incf count)
	      (push name (cdr distlist))
	      (insert-distlist (cons name group))
	      ))
	  (insert-distlist distlist)
	  )))))

(defun bbdb:make-mailrc ()
  "Make my mail aliases list."
  (interactive)
  (find-file "~/.mailrc-new")
  (erase-buffer)
  (let ((phone-lists nil)
	(distribution-lists nil))
    (bbdb:apply-to-records-defining
     (function
      (lambda (record)
	(let* ((net (copy-sequence (bbdb-record-net record)))
	       (name (subst-char-in-string
		      ?' ?-
		      (subst-char-in-string ?  ?. (bbdb-record-name record))
		      t))
	       (home-net (bbdb-record-getprop record 'home-net))
	       (net-invalid (bbdb-record-getprop record 'net-invalid))
	       (list-members (bbdb-record-getprop record 'list-members))
	       (raw-distlists (bbdb-record-getprop record 'distlists))
	       (distlists (if raw-distlists (bbdb-split raw-distlists ",") nil))
	       (phone-list (bbdb-record-getprop record 'phone-list)))
	  (when net-invalid (setq net nil))
	  (when (and (or net list-members) phone-list)
	    (let ((which-list (assoc phone-list phone-lists)))
	      (unless which-list
		(setq which-list (cons phone-list nil))
		(push which-list phone-lists))
	      (when net
		(push (vector name net)
		      (cdr which-list)))
	      (when home-net
		(push (vector (concat name ".home") (list home-net))
		      (cdr which-list)))
	      (when list-members
		(push (vector (bbdb-record-name record) (list list-members))
		      (cdr which-list)))))
	  (dolist (distlist distlists)
	    (let ((which-distlist (assoc distlist distribution-lists)))
	      (unless which-distlist
		(setq which-distlist (cons distlist nil))
		(push which-distlist distribution-lists))
	      (when net
		(push name (cdr which-distlist))))))
	nil))
     'phone-list)
    (setq phone-lists (sort phone-lists 
			    'car-string<))
    (dolist (phone-list phone-lists)
      (rplacd phone-list (sort (merged-people-list (cdr phone-list) 0 1)
			       (function (lambda (a b)
					   (surname< (aref a 0) (aref b 0)))))))
    (dolist (phone-list phone-lists)
      (insert "# section: " (capitalize (car phone-list)) "\n")
      (let* ((maxnamelen (max-slot-length (cdr phone-list) 0))
	     (indent (+ 3 maxnamelen))
	     (nameformat (format "alias %%%ds %%s\n" maxnamelen)))
	(dolist (person (cdr phone-list))
	  (insert (format nameformat
			  (aref person 0)
			  (car (aref person 1))))))
      (insert  "\n"))

    (when distribution-lists
      (insert "\n\n# distribution lists:\n\n")
      (setq distribution-lists
	    (sort distribution-lists 'car-string<))
      (dolist (distlist distribution-lists)
	(insert-distlist distlist))))
  (insert "\n# end of .mailrc\n")
  (goto-char (point-min))
  (basic-save-buffer)
  (build-mail-aliases)
  (let* ((old-aliases mail-aliases))
    ;; (message "Old aliases are %S" mail-aliases)
    (setq mail-aliases t)
    (build-mail-aliases "~/.mailrc-new")
    ;; (message "Read new aliases, got %S" mail-aliases)
    (let* ((new-aliases mail-aliases)
	   (old-only nil)
	   (both nil)
	   (new-only nil)
	   )
      ;; (message "Looking for each old alias in new aliases")
      (dolist (old old-aliases)
	(if (assoc (car old) new-aliases)
	    (push old both)
	  (push old old-only)))
      ;; (message "Looking for each new alias in old aliases")
      (dolist (new new-aliases)
	(unless (assoc (car new) old-aliases)
	  (push new new-only)))
      (with-output-to-temp-buffer "*Mailrc comparison*"
	(dolist (result-list (list (cons "Old only" (sort old-only 'car-string<))
				   (cons "Both" (sort both 'car-string<))
				   (cons "New only" (sort new-only 'car-string<))))
	  (princ (format "%s:\n" (car result-list)))
	  (dolist (address (cdr result-list))
	    (princ (format "%s <%s>\n" (car address) (cdr address))))
	  (princ "\n"))))
    (setq mail-aliases old-aliases)))

(defun bbdb:get-address (addresses location)
  "From ADDRESSES return the one matching LOCATION, or return nil."
  (catch 'found
    (dolist (address addresses)
      (when (string= (aref address 0) location)
	(throw 'found address)))
    nil))

(defun bbdb:pick-one-address (addresses)
  "Pick the most useful of ADDRESSES for writing to someone."
  (or (bbdb:get-address addresses "tower")
      (bbdb:get-address addresses "Home")
      (bbdb:get-address addresses "home")
      (bbdb:get-address addresses "parents")
      (bbdb:get-address addresses "college")
      (first addresses)))

(defun bbdb:extract-info (defining-key pattern record-info)
  "Collect records DEFINING-KEY matching PATTERN if given, applying RECORD-INFO.
The result is a list of the RECORD-INFO results -- this function takes a record
and returns whatever.
If a call to RECORD-INFO returns nil, it is not included in the result."
  (let ((result nil))
    (bbdb:apply-to-records-defining
     (function
      (lambda (record)
	(when (or (null pattern)
		  (string-match pattern (bbdb-record-getprop record defining-key)))
	  (let ((this-one (funcall record-info record)))
	    (when this-one
	      (push this-one result))))
	nil))
     defining-key)
    (nreverse result)))

(defun bbdb:insert-address (address &optional html)
  "Insert ADDRESS in a sensible form."
  (dotimes (i (- (length address) 1))
    (let ((line (aref address (+ i 1))))
      (cond
       ((null line) nil)
       ((and (stringp line) (not (string= line "")))
	(insert "   " line (if html "<br>\n" "\n")))
       ((listp line)
	(insert "   ")
	(dolist (elt line)
	  (insert (if (numberp elt) (int-to-string elt) elt) " ")))))))
  

(defun bbdb:make-Christmas-card-list (year)
  "Make my Christmas card list for YEAR."
  (interactive (list (string-to-number (read-from-minibuffer "Year: "))))
  (switch-to-buffer (get-buffer-create "*Christmas card list*"))
  (erase-buffer)
  (let ((card-list
	 (bbdb:extract-info
	  'christmas-card nil
	  (function
	   (lambda (record)
	     (let* ((recyear (bbdb-record-getprop record 'christmas-card))
		    (nyear (string-to-number recyear)))
	       (if (or (not (numberp nyear)) (< nyear year))
		   (vector (bbdb-record-title-and-name record)
			   (bbdb:pick-one-address (bbdb-record-addresses record))
			   recyear
			   (car (bbdb-record-net record)))
		 nil)))))))
    (setq card-list (merged-people-list card-list 0 1 3))
    (setq card-list (sort card-list
			  (function (lambda (a b)
				      (surname< (aref a 0) (aref b 0))))))
    (dolist (person card-list)
      (let ((name (aref person 0))
	    (address (aref person 1))
	    (years (aref person 2))
	    (email (aref person 3)))
	(insert name)
	(when t (insert " (" years ")"))
	(when t (if email
		    (insert (format " %s" email))
		  (insert "not on email")))
	(insert "\n")
	(bbdb:insert-address address)
	(insert "\n\n")))
    (insert (format "\n%d cards in list" (length card-list)))))

(defun bbdb:mark-sent-Christmas-cards (year)
  "Prompt for names and mark that they have been sent a Christmas card this YEAR."
  (interactive (list (string-to-number (read-from-minibuffer "Year: "))))
  (let ((records nil)
	(year-string (int-to-string year)))
    (while
	(setq records (bbdb-completing-read-record "Sent to: "))
      (dolist (record records)
	(bbdb-record-putprop record
			     'christmas-card
			     year-string)))))

(defun bbdb:make-Guild-list ()
  "Make the Guild list."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Guild list*"))
  (erase-buffer)
  (let* ((guild-list (bbdb:extract-info
		      'societies "CUGCR"
		      (function
		       (lambda (record)
			 (let ((college (bbdb-record-getprop record 'college))
			       (phone (copy-sequence (bbdb-record-phones record)))
			       (net (bbdb-record-net record)))
			   (vector (bbdb-record-title-and-name record)
				   (if college college "?")
				   (if net (car net) nil)
				   (if phone phone nil)
				   ))))))
	 (max-name-length (max-slot-length guild-list 0))
	 (max-college-length (max-slot-length guild-list 1))
	 (max-net-length (max-slot-length guild-list 2))
	 (guildie-format (format "%%%ds %%%ds %%%ds "
				 max-name-length
				 max-college-length
				 max-net-length))
	 )
    (dolist (person guild-list)
      (insert (format guildie-format
		      (aref person 0)
		      (aref person 1)
		      (aref person 2)))
      (bbdb:insert-phone-numbers (aref person 3)
				      (+ max-name-length
					 max-college-length
					 max-net-length
					 3)
				      ))))

(defun bbdb:make-Youths-list ()
  "Make the Youths list."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Youths list*"))
  (erase-buffer)
  (let* ((youths-list (bbdb:extract-info
		      'societies "SCY"
		      (function
		       (lambda (record)
			 (let ((phone (copy-sequence (bbdb-record-phones record)))
			       (net (bbdb-record-net record)))
			   (vector (bbdb-record-title-and-name record)
				   (if net (car net) nil)
				   (if phone phone nil)
				   ))))))
	 (max-name-length (max-slot-length youths-list 0))
	 (max-net-length (max-slot-length youths-list 1))
	 (youths-format (format "%%%ds %%%ds "
				 max-name-length
				 max-net-length))
	 )
    (dolist (person youths-list)
      (insert (format youths-format
		      (aref person 0)
		      (aref person 1)))
      (bbdb:insert-phone-numbers (aref person 2)
				      (+ max-name-length
					 max-net-length
					 2)
				      ))))

(defun mark-if-address-missing (person)
  (unless (bbdb-record-addresses person)
    (push person missing-addresses))
  nil)

(defun bbdb:show-missing-card-addresses ()
  "Present BBDB entries who are marked to receive a Christmas card, but have no postal address."
  (interactive)
  (setq missing-addresses nil)
  (bbdb:apply-to-records-defining 'mark-if-address-missing
				       'christmas-card)
  (bbdb-display-records (nreverse missing-addresses)))

(defun mark-if-phone-missing (person)
  (unless (bbdb-record-phones person)
    (push person missing-phones))
  nil)

(defun bbdb:show-missing-phones ()
  "Present BBDB entries who are marked to be on my phone list, but have no phone numbers."
  (interactive)
  (setq missing-phones nil)
  (bbdb:apply-to-records-defining 'mark-if-phone-missing
				       'phone-list)
  (bbdb-display-records (nreverse missing-phones)))

(defun bbdb:record-as-family (record)
  "Record RECORD as being one of my family"
  (vector (bbdb-record-title-and-name record)
	  (copy-sequence (bbdb-record-phones record))
	  (copy-sequence (bbdb-record-net record))
	  (bbdb:get-address (bbdb-record-addresses record) "home")
	  (bbdb-record-getprop record 'birth-date)))

(defun home-sort-compare (a b)
  (> (length (cdr a)) (length (cdr b))))

(defun same-home-phone (people)
  "Return whether all of PEOPLE have the same phone number,
according to the second array slot of each of them."
  (let* ((phones (mapcar
		  (function (lambda (person)
			    (bbdb:get-address (aref person 1) "home")))
		  people))
	 (first (car phones)))
    (catch 'same
      (dolist (other (cdr phones))
	(unless (equal other first)
	  (throw 'same nil)))
      t)))

(defun bbdb:make-family-contact-page ()
  "Make a web page containing family contact details."
  (interactive)
  (let ((file (substitute-in-file-name "$SYNCED/www/famcon.html")))
    (find-file file)
    (erase-buffer)
    (let ((family (bbdb:extract-info 'phone-list "family" 'bbdb:record-as-family))
	  (homes nil)
	  )
      (dolist (person family)
	(let* ((home (aref person 3))
	       (home-pair (assoc home homes)))
	  (if home-pair
	      (rplacd home-pair (cons person (cdr home-pair)))
	    (push (list home person)
		  homes))))
      (setq homes (sort homes 'home-sort-compare))
      (insert "<html><head><title>Family contact list</title></head><body>\n")
      (insert "<table border>\n")
      (insert "  <tr>\n")
      (dolist (heading '("Name" "birthday" "email" "home" "work" "mobile" "address"))
	(insert "    <td>" heading "</td>\n"))
      (insert "  </tr>\n")
      (dolist (home homes)
	(let* ((people-here (cdr home))
	       (same-home-phone (same-home-phone people-here))
	       (first t))
	  (dolist (person-here people-here)
	    (let ((phones (aref person-here 1)))
	      (insert "  <tr>\n")
	      (insert (format "    <th>%s</th>\n" (aref person-here 0)))
	      (let ((birth-date (aref person-here 4)))
		(if birth-date
		    (insert (format "    <td align=\"right\">%s</td>\n" birth-date))
		  (insert "    <td><!-- unknown birthday --></td>\n"))	      )
	      (let ((net (aref person-here 2)))
		(if net
		    (insert (format "    <td>%s</td>\n" (car net)))
		  (insert "    <td><!-- no email address --></td>\n")))
	      (dolist (phoneloc '("home" "work" "mobile"))
		(if (and same-home-phone
			 (string= phoneloc "home"))
		    (if first
			(let ((number  (bbdb:get-address phones phoneloc)))
			  (if number
			      (insert (format "    <td rowspan=\"%d\" valign=\"center\">%s</td>\n"
					      (length people-here)
					      (aref
					       number
					       1)))
			    (insert "    <td><!-- no number for these --></td>\n")))
		      (insert "    <!-- skipped, as same as above-->\n"))
		  (let ((phoneno (bbdb:get-address phones phoneloc)))
		    (if phoneno
			(insert (format "    <td>%s</td>\n" (aref phoneno 1)))
		      (insert (format "    <td><!-- no number for this --></td>\n"))))))
	      (when first
		(insert (format "    <td rowspan=\"%d\" valign=\"center\">" (length people-here)))
		(bbdb:insert-address (car home) t)
		(insert "    </td>\n")
		(setq first nil))
	      (insert "</tr>\n")))))
      (insert "</table>\n")
      (insert "</body></html>\n"))
    (basic-save-buffer)
    (when (fboundp 'w3-find-file)
      (w3-find-file file))))


(defun bbdb:make-birthdays-list ()
  "Make my birthdays list."
  (interactive)
  (find-file (substitute-in-file-name "$SYNCED/var/birthdays"))
  (erase-buffer)
  (let ((people nil))
    (bbdb:apply-to-records-defining
     (function
      (lambda (record)
	(let ((date (bbdb-record-getprop record 'birth-date)))
	  (when (string-match "[0-9][0-9]-[0-9][0-9]$" date)
	    (push (cons (bbdb-record-name record)
			(substring date
				   -5))
		  people)))
	nil))
     'birth-date)
    (setq people (sort people (function (lambda (a b)
					  (string< (cdr a) (cdr b))))))
    (let ((prev-date ""))
      (dolist (person people)
	(let* ((raw-date (cdr person))
	       (date (concat (calendar-month-name (string-to-number raw-date) t)
			     " "
			     (substring raw-date -2)))
	       )
	  (insert
	   (if (and nil (string= date prev-date))
	       "      "
	     (setq prev-date date))
	   " "
	   (car person)
	   "\n"))))
    (basic-save-buffer)))

(defun bbdb:make-birth-dates-list ()
  "Make a list of birth dates."
  (interactive)
  (find-file (substitute-in-file-name "$SYNCED/var/birth-dates"))
  (erase-buffer)
  (let ((people nil))
    (bbdb:apply-to-records-defining
     (function
      (lambda (record)
	(let ((date (bbdb-record-getprop record 'birth-date)))
	  (when (string-match "[0-9][0-9]-[0-9][0-9]$" date)
	    (push (cons (bbdb-record-name record)
			date)
		  people)))
	nil))
     'birth-date)
    (dolist (person people)
      (insert (car person) "\t" (cdr person) "\n"))))

(defun bbdb:make-all-filed-lists ()
  "Remake all the bbdb-derived files"
  (interactive)
  (bbdb:make-mailrc)
  (bbdb:make-family-contact-page)
  (bbdb:make-family-contact-page))

;;; end of bbdb-export-lists.el
