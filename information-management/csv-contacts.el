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

(defvar csv-contacts-parsed-tick 0
  "For re-reading the data.")

(defun csv-contacts-prepare (file)
  "Prepare the contacts lists based on FILE."
  (interactive "fContacts file: ")
  (save-window-excursion
    (save-excursion
      (find-file file)
      (when (or (null csv-contacts-name-to-id-hash)
                (null csv-contacts-id-to-name-hash)
                (> (buffer-modified-tick) csv-contacts-parsed-tick))
        (setq csv-contacts-name-to-id-hash (make-hash-table :test 'equal)
              csv-contacts-id-to-name-hash (make-hash-table :test 'equal)
              csv-contacts-parsed-tick (buffer-modified-tick))
        (csv-contacts-raw-parse-buffer)
        (let ((given-name-column (cdr (assoc "Given name" csv-contacts-column-alist)))
              (surname-column (cdr (assoc "Surname" csv-contacts-column-alist)))
              (id-column (cdr (assoc "ID" csv-contacts-column-alist))))
          (dolist (person csv-contacts-raw-data)
            (let ((name (concat (nth given-name-column person)
                                " "
                                (nth surname-column person)))
                  (id (nth id-column person)))
              (puthash name id csv-contacts-name-to-id-hash)
              (puthash (subst-char-in-string 32 ?_ name) id csv-contacts-name-to-id-hash)
              (puthash id name csv-contacts-id-to-name-hash))))))))

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
        csv-contacts-id-to-name-hash nil))

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

