;;; voltage-divider.el --- find resistor values from a list

;; Copyright (C) 2012  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: hardware

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Choose the nearest values from a list of resistors, to make a particular voltage divider.

;;; Code:

(defvar resistor-ranges nil
  "Alist of resistor range names (symbols) to lists of values in ohms.")

(defun resistor-range-name (range)
  "Normalize RANGE into the format we use for resistor ranges."
  (cond 
   ((symbolp range) range)
    ((and (stringp range) (string-match "e[0-9]+" range))
	    (downcase range))
    (t range)))

(defun add-resistor (range value)
  "To RANGE add resistor of VALUE ohms."
  (setq range (resistor-range-name range))
  (let ((pair (assoc range resistor-ranges)))
    (unless pair
      (push (setq pair (cons range nil)) resistor-ranges))
    (let ((values (cdr pair)))
      (unless (memql value values)
	(rplacd pair (cons value (cdr pair)))))))

(dotimes (decade 8)
  (dolist (norm '(10 15 22 33 47 68))
    (add-resistor 'e6 (* norm (expt 10.0 decade)))))

(dotimes (decade 8)
  (dolist (norm '(10 12 15 18 22 27 33 39 47 56 68 82))
    (add-resistor 'e12 (* norm (expt 10.0 decade)))))

(add-resistor 'my-stock 1000.0)
(add-resistor 'my-stock 47000.0)
(add-resistor 'my-stock 270000.0)
(add-resistor 'my-stock 910000.0)

(defun resistor-to-string (ohms)
  "Return the conventional string describing OHMS."
  (let ((number ohms)
	(letter ?R))
    (cond
     ((> number (expt 10.0 6))
      (setq number (/ number (expt 10.0 6))
	    letter ?M))
     ((> number (expt 10.0 3))
      (setq number (/ number (expt 10.0 3))
	    letter ?K)))

    ;; todo: convert to string
    
    ))

(defun resistor-make-voltage-divider (range vin vout)
  "Choose values from RANGE to make a votage divider to take VIN down to VOUT."
  (interactive
   (let* ((range (completing-read "Resistor range: "
				  resistor-ranges))
	  (voltage-in (string-to-number (read-from-minibuffer "Input voltage: "))))
     (while (zerop voltage-in)
       (setq voltage-in (string-to-number
			 (read-from-minibuffer
			  "Input voltage (must be non-zero number): "))))
     (let ((voltage-out (string-to-number
			 (read-from-minibuffer "Output voltage: "))))
       (while (or (zerop voltage-out) (> voltage-out voltage-in))
	 (setq voltage-out (string-to-number
			    (read-from-minibuffer
			     (format
			      "Output voltage (must be non-zero number below %f): "
			      voltage-in)))))
       (list range voltage-in voltage-out))))
  (unless (consp range)
    (setq range (cdr (assoc (resistor-range-name range)
			    resistor-ranges))))
  (setq range (sort (copy-sequence range) '<))
  (let* ((below (/ vin vout))
	 (above (- vin below))
	 (ratio (/ below above))
	 (uppers range)
	 (best-upper nil)
	 (best-lower nil)
	 (least-error (expt 10.0 10)))
    (message "above=%f below=%f ratio=%f" above below ratio)
    (while uppers
      (let* ((upper (car uppers))
	     (ideal-lower (* upper ratio))
	     (log-ideal-lower (log ideal-lower))
	     (lowers range))
	(message "  trying upper %f, ideal lower is %f" upper ideal-lower)
	(while (and (cdr lowers)
		    (< (cadr lowers) ideal-lower))
	  (setq lowers (cdr lowers)))

	(let* ((a (car lowers))
	       (log-a (log a))
	       (a-error (abs (- log-ideal-lower log-a)))
	       (b (cadr lowers))
	       (log-b (log b))
	       (b-error (abs (- log-ideal-lower log-b))))
	  (message "    nearest two are %s %s" a b)
	  (when (< a-error least-error)
	    (setq least-error a-error
		  best-upper upper
		  best-lower a))
	  (when (< b-error least-error)
	    (setq least-error b-error
		  best-upper upper
		  best-lower b))))
      (setq uppers (cdr uppers)))
    (if (interactive-p)
	(message "Upper resistor: %f; lower resistor %f; log error %f" best-upper best-lower least-error))
    (list best-upper best-lower least-error)))

(provide 'voltage-divider)
;;; voltage-divider.el ends here
