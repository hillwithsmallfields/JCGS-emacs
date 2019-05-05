;; Emacs access to my contacts list
;; Compatible with the code in https://github.com/hillwithsmallfields/qs/tree/master/contacts

;; Written by John Sturdy, 2019-04-26

(provide 'csv-contacts)

(defconst csv-contacts-column-names
  '("Given name" "Middle names" "Surname" "Title" "Old name" "AKA"
    "Birthday" "Died"
    "First contact" "Last contact"
    "Gender"
    "ID" "Parents" "Offspring" "Siblings" "Partners" "Ex-partners" "Knows"
    "Nationality" "Notes"
    "Group Membership" "Flags" "Other groups" "Organizations"
    "Place met" "Subjects" "Jobs"
    "Primary email" "Other emails"
    "Primary phone Type" "Primary phone Value" "Secondary phone Type" "Secondary phone Value"
    "Street" "Village/District" "City" "County" "State" "Postal Code" "Country" "Extended Address")
  "The known column names.")

(defvar csv-contacts-column-alist nil
  "Alist giving the column positions.")

(make-variable-buffer-local 'csv-contacts-column-alist)

(defun csv-contacts-read-header ()
  "Read the header line of the current file."
  (interactive)                         ; mostly for testing
  (setq csv-contacts-column-alist nil)
  (save-excursion
    (goto-char (point-min))
    (let ((eol (line-end-position))
          (i 0))
      (while (re-search-forward "\\([^,]+\\),?" eol t)
        (push (cons (match-string-no-properties 1) i)
              csv-contacts-column-alist)
        (setq i (1+ i))))))

(defvar csv-contacts-raw-data nil
  "List of lists of cell data.")

(make-variable-buffer-local 'csv-contacts-raw-data)

(defun csv-contacts-raw-parse-buffer ()
  "Parse the buffer as a list of lists."
  (interactive)                         ; mostly for testing
  (csv-contacts-read-header)
  (goto-char (point-min))
  (forward-line 1)
  (setq csv-contacts-raw-data
        (mapcar (lambda (line)
                  (split-string line ","))
                (split-string (buffer-substring-no-properties
                               (point) (point-max))
                              "\n"))))

(defvar csv-contacts-name-to-id-hash
  nil
  "Hash table mapping contact names to IDs.")

(defvar csv-contacts-id-to-name-hash
  nil
  "Hash table mapping contact IDs to names.")

(defvar csv-contacts-given-name-alist
  nil
  "Alist of given names to surnames.")

(defvar csv-contacts-given-name-regexp
  nil
  "Regexp matching the given names of any of my contacts.")

(defvar csv-contacts-parsed-tick 0
  "For re-reading the data.")

(defun csv-contacts-register-name-pair (given-name surname id)
  "Register that GIVEN-NAME may be followed by SURNAME and they refer to ID."
  (let ((by-given-name-pair (assoc given-name csv-contacts-given-name-alist)))
    (unless by-given-name-pair
      (setq by-given-name-pair (cons given-name nil))
      (push by-given-name-pair csv-contacts-given-name-alist))
    (unless (assoc surname (cdr by-given-name-pair))
      (rplacd by-given-name-pair
            (cons (list surname name id)
                  (cdr by-given-name-pair))))))

(defun csv-contacts-add-other-names-to-tree (other-names-string id)
  "Parse OTHER-NAMES-STRING and add them all with ID."
  (let ((other-names (split-string other-names-string ";" t " +")))
    (dolist (other-name other-names)
      (puthash other-name id csv-contacts-name-to-id-hash)
      (let ((as-list (split-string other-name)))
        (csv-contacts-register-name-pair
         (car as-list)
         (mapconcat 'identity (cdr as-list) " ")
         id)))))

(defun csv-contacts-prepare (file)
  "Prepare the contacts lists based on FILE."
  (interactive "fContacts file: ")
  (save-window-excursion
    (save-excursion
      (find-file file)
      (when (or (null csv-contacts-name-to-id-hash)
                (null csv-contacts-id-to-name-hash)
                (null csv-contacts-given-name-alist)
                (> (buffer-modified-tick) csv-contacts-parsed-tick))
        (setq csv-contacts-name-to-id-hash (make-hash-table :test 'equal)
              csv-contacts-id-to-name-hash (make-hash-table :test 'equal)
              csv-contacts-parsed-tick (buffer-modified-tick))
        (csv-contacts-raw-parse-buffer)
        (let ((given-name-column (cdr (assoc "Given name" csv-contacts-column-alist)))
              (surname-column (cdr (assoc "Surname" csv-contacts-column-alist)))
              (old-names-column (cdr (assoc "Old name" csv-contacts-column-alist)))
              (aka-column (cdr (assoc "AKA" csv-contacts-column-alist)))
              (id-column (cdr (assoc "ID" csv-contacts-column-alist))))
          (dolist (person csv-contacts-raw-data)
            (let* ((given-name (nth given-name-column person))
                   (surname (nth surname-column person))
                   (name (concat given-name " " surname))
                   (old-names-string (nth old-names-column person))
                   (other-names-string (nth aka-column person))
                   (id (nth id-column person)))
              (puthash name id csv-contacts-name-to-id-hash)
              (puthash (subst-char-in-string 32 ?_ name) id csv-contacts-name-to-id-hash)
              (puthash id name csv-contacts-id-to-name-hash)
              (csv-contacts-register-name-pair given-name surname id)
              (when old-names-string
                (csv-contacts-add-other-names-to-tree old-names-string id))
              (when other-names-string
                (csv-contacts-add-other-names-to-tree other-names-string id))))))))
  (setq csv-contacts-given-name-regexp
        (regexp-opt (mapcar 'car csv-contacts-given-name-alist) 'words)))

(defvar csv-contacts-file (substitute-in-file-name "$ORG/contacts.csv")
  "The default location for my contacts data.")

(defun csv-contacts-name-to-id (name)
  "Get the ID corresponding to NAME."
  (csv-contacts-prepare csv-contacts-file)
  (gethash name csv-contacts-name-to-id-hash))

(defun csv-contacts-id-to-name (id)
  "Get the name correspoding to ID."
  (csv-contacts-prepare csv-contacts-file)
  (gethash id csv-contacts-id-to-name-hash))

(defun reset-contacts ()
  "Make the contacts data be re-read next time it is used."
  (interactive)
  (setq csv-contacts-name-to-id-hash nil
        csv-contacts-id-to-name-hash nil
        csv-contacts-given-name-alist nil))

(defun car-string-less-than-car (a b)
  "Return whether the car of A is less than the car of B."
  (string< (car a) (car b)))

(defun show-contacts-by-name ()
  "List the contacts by name."
  (interactive)
  (let ((pairs nil))
    (maphash (lambda (k v)
               (push (cons k v) pairs))
             csv-contacts-name-to-id-hash)
    (setq pairs (sort pairs 'car-string-less-than-car))
    (with-output-to-temp-buffer "*Contacts by name*"
      (let ((fmt (format "%%%ds %%s\n"
                         (1+ (apply 'max
                                    (mapcar 'length
                                            (mapcar 'car
                                                    pairs)))))))
        (dolist (pair pairs)
          (princ (format fmt (car pair) (cdr pair))))))))

(defun show-name-tree ()
  "Show the registered names, as a tree."
  (interactive)
  (setq csv-contacts-given-name-alist
        (sort csv-contacts-given-name-alist
              'car-string-less-than-car))
  (dolist (subtree csv-contacts-given-name-alist)
    (rplacd subtree (sort (cdr subtree) 'car-string-less-than-car)))
  (with-output-to-temp-buffer "*Name tree*"
  (dolist (subtree csv-contacts-given-name-alist)
    (princ (car subtree))
    (princ ":")
    (dolist (leaf (cdr subtree))
      (princ "\n  ") (princ (car leaf)))
    (princ "\n"))))

(defun handle-contacts-region (begin end)
  "Handle all contacts between BEGIN and END."
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char begin)
    (while (re-search-forward csv-contacts-given-name-regexp end t)
      (let* ((found-name (match-string 0))
             (by-surname (assoc found-name csv-contacts-given-name-alist))
             (chosen (if (= (length by-surname) 1) ; todo: I don't think this branch works
                         (car by-surname)
                       (let* ((next-word
                               (save-excursion
                                 (forward-word 1)
                                 (let ((next-word-end (point)))
                                   (backward-word 1)
                                   (buffer-substring-no-properties (point) next-word-end))))
                              (next-word-found (assoc next-word by-surname)))
                         (if next-word-found
                             (cdr next-word-found)
                           "ask"        ; todo: completing-read
                           )))))
        (message "Got %S, possibilities are %S, chosen is %S" found-name by-surname chosen)
        ;; todo: call a supplied function
        )
      )
    ))
