

(defun fb-read ()
  (interactive)
  (setq allbuff (get-buffer "bball")
	all (save-window-excursion
	      (set-buffer allbuff)
	      (goto-char (point-max))
	      (backward-sexp 1)
	      (read allbuff))
	nall (length all))
  (message "Read %d entries" nall)
  )


(defun bsort (a b)
  (let ((ac (aref a 0))
	(bc (aref b 0))
	(as (aref a 1))
	(bs (aref b 1)))
    (if (and (stringp ac) (stringp bc)
	     (stringp as) (stringp bs)
	     )
	(if (string= as bs)
	    (string< ac bc)
	  (string< as bs))
      nil)))

(defun fb-sort ()
  (interactive)
  (setq sall (sort all 'bsort)
	nsall (length all))
  (message "%d entries after sorting" nsall)
  )

(defun fb-output-results ()
  (interactive)
  (set-buffer (get-buffer-create "bbnew"))
  (erase-buffer)
  (insert "(\n")
  (let ((a all))
    (while a
      (insert (format "%S\n" (car a)))
      (setq a (cdr a))))
  (insert ")\n")
  (goto-char (point-min))
  (setq readback (read (current-buffer)))
  (message "Read back %d, should be %d, was originally %d, there were %d clashes"
	   (length readback)
	   (- nall nclashes)
	   nall nclashes)
  )

(defun is-timestamp (x) (eq (car x) 'timestamp))

(defun fb-better (a b)
  (let ((pa (aref a 7))
	(pb (aref b 7)))
    (if (> (length pa) (length pb))
	a
      (if (> (length pb) (length pa))
	  b
	(let (
	      (pant (remove-if 'is-timestamp pa))
	      (pbnt (remove-if 'is-timestamp pb))
	      )
	  ;; (message "Clash not sortable by properties length: %S %S" a b)
	  (if (> (length pant) (length pbnt))
	      b
	    (if (equal pant pbnt)
		a
	      (progn
		(princ (format "Clash not sortable by removing timestamps:\n %S\n %S\n\n" a b))
		a))))))))

(defun fb-uniquify ()
  (interactive)
  (with-output-to-temp-buffer "*Clashes*"
    (setq nclashes 0)
    (let ((a sall))
      (while (and a (cdr a))
	(let* ((a0 (car a))
	       (ca0 (aref a0 0))
	       (sa0 (aref a0 1))
	       (a1 (car (cdr a)))
	       (ca1 (aref a1 0))
	       (sa1 (aref a1 1))
	       )
	  ;; (message "ca0=%s sa0=%s  ca1=%s sa1=%s" ca0 sa0 ca1 sa1)
	  (if (and (stringp sa0)
		   (stringp ca0)
		   (equal sa0 sa1)
		   (equal ca0 ca1))
	      (let ((best (fb-better a0 a1)))
		(setq nclashes (1+ nclashes))
		;; (princ (format "Clash:\n  %s\n  %s\n\n" a0 a1))
		(rplaca a best)
		(rplacd a (cdr (cdr a))))
	    (setq a (cdr a))))))))

(defun fb ()
  (interactive)
  (fb-read)
  (fb-sort)
  (fb-uniquify)
  (fb-output-results))

(defun fbrl (r) (bbdb-change-record r t)
  t)
 
(defun fb-reorder-live ()
  (interactive)
  (require 'bbdb-iterators)
  (bbdb:apply-to-records 'fbrl))

(defun fb-by-alist-desparately ()
  (interactive)
  (fb-read)
  (fb-sort)
  (setq fba nil uall nil nclashes 0)
  (with-output-to-temp-buffer "*u by alist*"
    (let ((a sall))
      (while a
	(let* ((a0 (car a))
	       (ac (aref a0 0))
	       (as (aref a0 1)))
	  (if (and (stringp ac) (stringp as))
	      (let* ((key (concat ac " " as))
		     (pair (assoc key fba)))
		(if pair
		    (progn
		      (incf nclashes)
		      (rplacd pair (fb-better a0 (cdr pair))))
		  (push (cons key a0) fba)))
	    (push (cons nil a0) fba)))
	(setq a (cdr a)))))
  (setq uall (nreverse (mapcar 'cdr fba)))
  (setq all uall)
  (fb-output-results)
  (message "%d total, should be %d (%d-%d)"
	   (length uall)
	   (- nall nclashes)
	   nall nclashes
	   )
  )
