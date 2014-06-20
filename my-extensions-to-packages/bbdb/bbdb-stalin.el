;;;; bbdb-stalin.el -- help purge unpersons
;;; started by John Sturdy, 2002-01-02
;;; Time-stamp: <2012-03-08 17:39:46 johnstu>

(provide 'bbdb-stalin)
(require 'bbdb-iterators)

(defvar bbdb-potential-unpersons nil
  "Working data for bbdb-stalin")

(defun bbdb-stalin ()
  "Display potential unpersons."
  (interactive)
  (setq bbdb-potential-unpersons nil)
  (bbdb:apply-to-records 'bbdb:collect-potential-unpersons)
  (bbdb-display-records bbdb-potential-unpersons))

(defun bbdb:collect-potential-unpersons (person)
  "Add PERSON to bbdb-potential-unpersons if they meet none of certain criteria."
  ;; (message "%s: %s, %S" (car (bbdb-record-net person)) (bbdb-record-company person) (bbdb-record-raw-notes record))
  (let ((notes (bbdb-record-raw-notes record)))
    (when (and (or (null notes)
		   (and (null (cdr notes))
			(eq (caar notes) 'timestamp))
		   )
	       (null (bbdb-record-company person))
	       (not (catch 'found
		      (dolist (address (bbdb-record-net person))
			(if (string-match-any '("harlequin" "ugsolutions" "sanger" "cb1" "citrix" "ul.ie")
					      address)
			    (throw 'found t)))
		      nil)))
      (push person bbdb-potential-unpersons)))
  nil)

(defun bbdb:unlist-phone-list-section (section)
  "Remove the phone-list property of all records whose phone-list property is a match for SECTION."
  (interactive "sUnlist phone section: ")
  (bbdb:apply-to-records-defining
   (function
    (lambda (record)
      (let ((old-phone-list (bbdb-record-getprop record 'phone-list)))
	(when (and (stringp old-phone-list)
		   (string-match section old-phone-list))
	  (message "unlisting %s" (bbdb-record-name record))
	  (bbdb-record-putprop record 'phone-list nil)))
      nil))
   'phone-list))

;;; end of bbdb-stalin.el
