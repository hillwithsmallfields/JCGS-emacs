;;;; dated-plot.el
;;; Time-stamp: <2002-07-09 12:53:50 jcgs>

(provide 'dated-plot)
(require 'time-date)

(defun dated-plot (days)
  "Plot the items in DAYS.
Each item is a list, the head of which is a date as a string, as from current-time-string.
The remaining items are numbers, producing successive lines of the plot.
The first item is a special one, containing the chart labels"
  (let* ((labels (car days))
	 (nslots (length labels))
	 (ndataslots (1- nslots))
	 (slots (make-vector nslots nil))
	 (base-day (date-to-day (car (second days))))
	 (prev-day-month-string ""))
    (dolist (day (cdr days))
      (let* ((date-of-day (car day))
	     (date-days (- (date-to-day date-of-day) base-day))
	     (month-string (month-string date-of-day)))
	(unless (string= month-string prev-day-month-string)
	  (push (cons date-days month-string)
		(aref slots 0)))
	(setq prev-day-month-string month-string)
	;; now accumulate the data points
	(dotimes (i ndataslots)
	  (let ((slot (1+ i)))
	    (push (cons date-days (nth slot day))
		  (aref slots slot))))))
    ;; reverse them
    (dotimes (i nslots) (aset slots i (nreverse (aref slots i))))
    ;; now output the data as PostScript
    (let* ()
      ;; first output the dates
      (insert "beginchart\n")
      (dolist (datepair (aref slots 0))
	(insert (format "%d (%s) date\n" (car datepair) (cdr datepair))))
      ;; now output each line
      (dotimes (i ndataslots)
	(let* ((slot (1+ i))
	       (slotdata (aref slots slot))
	       (slotvalues (mapcar 'nil-to-zero (mapcar 'cdr slotdata)))
	       (minval (apply 'min slotvalues))
	       (maxval (apply 'max slotvalues))
	       )
	  (insert (format "%d %d %d beginline\n" slot minval maxval))
	  (dolist (plotpair slotdata)
	    (insert (format "%d %d p\n" (car plotpair) (cdr plotpair))))
	  (insert (format "endline %% for line %d\n" slot))))
      (insert "endchart\n"))))

(defun dated-plots-separately (days)
  "Plot the items in DAYS.
Each item is a list, the head of which is a date as a string, as from current-time-string.
The remaining items are numbers, producing points on successive plots."
  (with-output-to-temp-buffer "*plots*"
    (let* ((nplots (1- (length (car days))))
	   ;; ()
	   )
      (dotimes (i nplots)
	(let* ((iplot (1+ i))
	       (plotlist (mapcar
			  (function
			   (lambda (full-list)
			     (list (car full-list) (nth iplot full-list))))
			  days)))
	  ;; (princ (format "Plotting chart %d, data is %S\n" iplot plotlist))
	  ;; (dolist (point plotlist) (princ (format "  %s: %d\n" (first point) (second point))))
	  (dated-plot plotlist)
	  )
	)
      )))

(defun month-string (date)
  "Return a month (with year) string for DATE."
  (concat (substring date 4 7)
	  " "
	  (substring date -4)))

(defun dated-plot-to-file (data file)
  "Produce a dated plot of DATA in FILE."
  (find-file file)
  (erase-buffer)
  (insert-file-contents "~/.datedplotpreamble.ps")
  (goto-char (point-max))
  (dated-plot data)
  (basic-save-buffer))

(defun dated-plots-to-file (data file)
  "Produce a family of dated plots of DATA in FILE."
  (find-file file)
  (erase-buffer)
  (insert-file-contents "~/.datedplotpreamble.ps")
  (goto-char (point-max))
  (dated-plots-separately data)
  (basic-save-buffer))

(defun invert-list-of-lists (original)
  "Convert structure like ((a b c) (d e f) (g h i) (j k l)) to ((a d g j) (b e h k) (c f i l))"
  (let* ((ndataslots (length (car original)))
	 (slots (make-vector ndataslots nil)))
    (dolist (old-outer original)
      (dotimes (i ndataslots)
	(push (nth i old-outer)
	      (aref slots i))))
    ;; reverse them
    (map 'list 'nreverse slots)))

(defun plot-each-against-each (data-lists)
  "Plot each of the DATA-LISTS against each other."
  (when (cdr data-lists)
    (let ((a (car data-lists))
	  (bs (cdr data-lists)))
    (plot-against-each a bs)
    (when (cdr bs)
      (plot-each-against-each bs)))))

(defun plot-against-each (a bs)
  "Plot data list A against each of BS."
  (plot-against a (car bs))
  (when (cdr bs) 
    (plot-against-each a (cdr bs))))

(defun nil-to-zero (n) (if (numberp n) n 0))

(defun plot-against (a b)
  "Plot list A against B.
They are assumed to have the same number of points."
  (let* ((adata (mapcar 'nil-to-zero (cdr a)))
	 (bdata (mapcar 'nil-to-zero (cdr b)))
	 (amax (apply 'max adata))
	 (bmax (apply 'max bdata)))
    (insert (format "(%s) (%s) %d %d beginpairsplot\n" (car a) (car b) amax bmax))
    (while (and adata bdata)
      (insert (format " %d %d pair\n" (car adata) (car bdata)))
      (setq adata (cdr adata) bdata (cdr bdata)))
    (insert "endpairsplot\n")))

(defun pair-plots-to-file (data file)
  "Produce a family of pair plots of DATA in FILE."
  (find-file file)
  (erase-buffer)
  (insert-file-contents "~/.datedplotpreamble.ps")
  (goto-char (point-max))
  (plot-each-against-each (invert-list-of-lists data))
  (basic-save-buffer))

