;;;; weather.el
;;; Time-stamp: <2005-01-18 19:06:25 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(require 'cl)
(provide 'weather)

(defun weather:field (field-name)
  "Retrieve field called FIELD-NAME from the weather data."
  (require 'match-string)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^%s: +\\(.+\\)$" field-name)
			   (point-max) t)
	(match-string-no-properties 1)
      nil)))

(defstruct weather:snapshot
  "A set of data from the ORL weather station."
  timestamp
  numeric-timestamp
  temperature
  pressure
  humidity
  dewpoint
  wind
  sunshine
  rainfall)

(defvar weather:snapshots nil
  "Accumulated weather snapshots.")

;;;###autoload
(defun weather:page ()
  "Fetch the Cambridge weather status."
  (interactive)
  (require 'w3)
  (let ((w3-reuse-buffers 'reload)	; or 'yes
	(w3-notify 'meek))
    (w3-fetch "http://www.uk.research.att.com/cgi-bin/weathertext-html-cgi"
	      ;; "http://www.orl.co.uk/cgi-bin/weathertext-html-cgi"
	      )))
;; did do a w3-reload-document

;;;###autoload
(defun weather:display (snapshot)
  "Display SNAPSHOT in the minibuffer."
  (interactive (list (third (car weather:snapshots))))
  (when snapshot
    (message "Temperature %.1f; wind %s; rainfall %.2fmm; humidity %d%%"
	     (weather:snapshot-temperature snapshot)
	     (weather:snapshot-wind snapshot)
	     (weather:snapshot-rainfall snapshot)
	     (* 100 (weather:snapshot-humidity snapshot)))))

;;;###autoload
(defun weather:snapshot ()
  "Return a snapshot of the weather."
  (interactive)
  (save-window-excursion
    (weather:page)
    (let* ((numeric-timestamp (current-time))
	   (timestamp
	    (if (re-search-forward "Cambridge .+ Rooftop Weather at \\(.+\\):" (point-max) t)
		(match-string-no-properties 1)
	      (current-time-string)))
	   (snapshot (make-weather:snapshot
		      :timestamp timestamp
		      :numeric-timestamp numeric-timestamp
		      :temperature (string-to-number (weather:field "temperature"))
		      :pressure (string-to-number (weather:field "pressure"))
		      :humidity (/ (string-to-number (weather:field "humidity")) 100.0)
		      :dewpoint (string-to-number (weather:field "dewpoint"))
		      :wind (weather:field "wind")
		      :sunshine (weather:field "sunshine")
		      :rainfall (string-to-number (weather:field "rainfall")))))
      (push (list numeric-timestamp timestamp snapshot)
	    weather:snapshots)
      (weather:display snapshot)
      snapshot)))

;;;###autoload
(defun weather:raining-p ()
  "Describe changes that are happening in the weather."
  (interactive)
  (if weather:snapshots
      (let* ((previous-weather (cdar weather:snapshots))
	     (current-weather (weather:snapshot)))
	(if (string= (weather:snapshot-timestamp previous-weather)
		     (weather:snapshot-timestamp current-weather))
	    (error "Still on same weather snapshot")
	  (let* ((raining-p (> (weather:snapshot-rainfall current-weather)
			       (weather:snapshot-rainfall previous-weather))))
	    (message "%s%s"
		     (cond
		      ((> (weather:snapshot-temperature current-weather)
			  (weather:snapshot-temperature previous-weather))
		       "Warming up; ")
		      ((< (weather:snapshot-temperature current-weather)
			  (weather:snapshot-temperature previous-weather))
		       "Cooling down; ")
		      (t ""))
		     (format "has %s %s"
			     (if raining-p
				 "rained some time since"
			       "not rained since")
			     (weather:snapshot-timestamp previous-weather)))
	    raining-p)))
    (error "No previous rainfall level for comparison")))


;; http://www.jodc.jhd.go.jp/inf/data/format/pollution-code.html#windforce
; http://www.jodc.jhd.go.jp/inf/data/format/pollution-code.html#windforce
; Code   Description  Knots         m/s
;  0 Calm             0 - 0.9    0 - 0.2
;  1 Light air        1 -  3   0.3 - 1.5
;  2 Light breeze     4 -  6   1.6 - 3.3
;  3 Gentle breeze    7 - 10   3.4 - 5.4
;  4 Moderate breeze 11 - 16   5.5 - 7.9
;  5 Fresh breeze    17 - 21   8.0 - 10.7
;  6 Strong breeze   22 - 27  10.8 - 13.8
;  7 Near gale       28 - 33  13.9 - 17.1
;  8 Gale            34 - 40  17.2 - 20.7
;  9 Strong gale     41 - 47  20.8 - 24.4
; 10 Storm           48 - 55  24.5 - 28.4
; 11 Violent storm   56 - 63  28.5 - 32.6
; 12 Hurricane           >64        >32.7

;;; end of weather.el
