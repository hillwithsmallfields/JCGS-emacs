;;;; type-break-adaptive.el
;;; Time-stamp: <2002-02-20 09:40:31 jcgs>

(provide 'type-break-adaptive)

(defun type-break-recent-history (period)
  "Return enough of the typing break history to cover the last PERIOD seconds."
  (let ((history type-break-history)
	(subhistory nil)
	(duration 0))
    (while (and (< duration period) (not (null history)))
      (let* ((pair (car history))
	     (last-typing (car pair))
	     (last-resting (cdr pair))
	     )
	(unless (numberp last-resting) (setq last-resting 0))
	(unless (numberp last-typing) (setq last-typing 0))
	;; (message "Adding %S to subhistory which is currently %S" (cons last-typing last-resting) subhistory)
	(setq duration (+ duration (+ last-typing last-resting))
	      subhistory (cons (cons last-typing last-resting) subhistory)
	      history (cdr history))))
    ;; (message "Got total subhistory %S" subhistory)
    (if subhistory
	(nreverse subhistory)
      nil)))

(defun type-break-longest-recent (period)
  "Return the length of the longest break in the last PERIOD seconds."
  (let ((history (type-break-recent-history period)))
    (if history
	(apply 'max (mapcar 'cdr history))
      0)))

(defvar type-break-adaptive-longer-after-seconds
  (* 60 20)
  "After this number of seconds since the last long break, take a longer break.")

(defvar type-break-adaptive-longer-length
  (* 60 10)
  "Length of longer breaks, which should occur at least every type-break-adaptive-longer-after-seconds.")

(defun type-break-adaptive-this-break-length ()
  "Return how long the break just starting should be."
  (if (< (type-break-longest-recent type-break-adaptive-longer-after-seconds)
	 type-break-adaptive-longer-length)
      type-break-adaptive-longer-length
    (if (zerop type-break-deferments)
	type-break-good-rest-interval
      (+ type-break-good-rest-interval
	 (* 30 type-break-deferments)))))

;;;; end of type-break-adaptive.el

