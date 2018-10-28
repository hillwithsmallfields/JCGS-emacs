(defun randalphanums (n)
  "Return a string of N alphanumerics."
  (let ((chars nil))
    (dotimes (i n)
      (let ((r (random 62)))
	(push (+ r (cond
		    ((<= r 9) ?0)
		    ((> r 36) (- ?a 36))
		    (t (- ?A 10))))
	      chars)))
    (apply 'string chars)))

(defun nrandomalnums (n least most)
  "Return N random alphanumeric sequences with between LEAST and MOST chars."
  (let ((alnums nil)
	(range (- most least)))
    (dotimes (i n)
      (push (randalphanums (+ least (random range)))
	    alnums))
    (mapconcat 'identity alnums " ")))
