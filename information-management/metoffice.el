;;;; metoffice.el --- handle data from the UK's Meteorological Office
;;; Time-stamp: <2015-03-16 21:20:59 jcgs>

(require 'json)

(defvar metoffice-base-url "http://datapoint.metoffice.gov.uk/public/data/val/wxfcs/all/json/"
  "The base address of the Met Office data site.")

(defvar metoffice-config-file "~/.metoffice-config.el"
  "The name of the file containing your metoffice config.
This should set metoffice-api-key to the key you obtained from the web site.")

(defvar metoffice-api-key nil
  "The API key you got when you registered with the Met Office site.")

(defvar metoffice-home-location nil
  "The location name for reports.")

(defun metoffice-setup ()
  "Ensure you have your metoffice data set up."
  (when (or (not (boundp 'metoffice-api-key))
	    (not (stringp metoffice-api-key)))
    (load-file metoffice-config-file)))

(defun metoffice-get-string-data (data-point &optional extra)
  "Get the data of DATA-POINT from the Met Office datapoint, as a string.
EXTRA is another piece of URL, to go after the question mark."
  (metoffice-setup)
  (let ((command (concat "curl"
			 " --silent \""
			 metoffice-base-url
			 data-point
			 "?"
			 (or extra "")
			 "key="
			 metoffice-api-key "\"")))
    (message "Using command %s" command)
    (shell-command-to-string command)))

(defun metoffice-get-json-data (data-point &optional extra)
  "Get the data of DATA-POINT from the Met Office datapoint, as json.
EXTRA is another piece of URL, to go after the question mark."
  (json-read-from-string
   (metoffice-get-string-data data-point extra)))

(defvar metoffice-site-list nil
  "The Met Office site list.")

(defun metoffice-get-site-list (&optional force)
  "Get the metoffice site list.
Use cached data if available, unless optional FORCE argument given."
  (or (and (not force)
	   metoffice-site-list)
      (setq metoffice-site-list
	    (map 'list
		 (function
		  (lambda (raw-entry)
		    (cons (cdr (assoc 'name raw-entry))
			  raw-entry)))
		 (cdr (assoc 'Location
			     (cdr (assoc 'Locations
					 (metoffice-get-json-data "sitelist")))))))))

(defun metoffice-get-site-id-by-name (name &optional display-result)
  "Get the site id for NAME.
With optional DISPLAY-RESULT, show the result as a message."
  (interactive
   (list (completing-read "Find ID for Met Office site: "
			  (metoffice-get-site-list)
			  nil t)
	 t))
  (let ((site-data (assoc name (metoffice-get-site-list))))
    (if site-data
	(let ((id (cdr (assoc 'id site-data))))
	  (when display-result
	    (message "Site %s has id %s" name id))
	  id)
      (error "Could not find a Met Office site called %s" name))))

(defvar metoffice-weather-codes
  '((0 . "Clear night")
    (1 . "Sunny day")
    (2 . "Partly cloudy (night)")
    (3 . "Partly cloudy (day)")
    (4 . "Not used")
    (5 . "Mist")
    (6 . "Fog")
    (7 . "Cloudy")
    (8 . "Overcast")
    (9 . "Light rain shower (night)")
    (10 . "Light rain shower (day)")
    (11 . "Drizzle")
    (12 . "Light rain")
    (13 . "Heavy rain shower (night)")
    (14 . "Heavy rain shower (day)")
    (15 . "Heavy rain")
    (16 . "Sleet shower (night)")
    (17 . "Sleet shower (day)")
    (18 . "Sleet")
    (19 . "Hail shower (night)")
    (20 . "Hail shower (day)")
    (21 . "Hail")
    (22 . "Light snow shower (night)")
    (23 . "Light snow shower (day)")
    (24 . "Light snow")
    (25 . "Heavy snow shower (night)")
    (26 . "Heavy snow shower (day)")
    (27 . "Heavy snow")
    (28 . "Thunder shower (night)")
    (29 . "Thunder shower (day)")
    (30 . "Thunder"))
  "Weather codes from http://www.metoffice.gov.uk/datapoint/support/documentation/code-definitions")

(defvar metoffice-visibility-codes-alist
  '((UN . "Unknown")
    (VP . "Very poor - Less than 1 km")
    (PO . "Poor - Between 1-4 km")
    (MO . "Moderate - Between 4-10 km")
    (GO . "Good - Between 10-20 km")
    (VG . "Very good - Between 20-40 km")
    (EX . "Excellent - More than 40 km"))
  "Visibility codes from http://www.metoffice.gov.uk/datapoint/support/documentation/code-definitions")

;;; following instructions from http://www.metoffice.gov.uk/datapoint/product/uk-daily-site-specific-forecast/detailed-documentation

(defun metoffice-get-site-weather (&optional site)
  "Return an alist of forecasts for SITE.
SITE may be a site id or a name.
If SITE is not given, the value of `metoffice-home-location' is used.

The car of each entry is a date, and the cdr is an alist of the day
and night forecasts, mapping 'day and 'night to alists of characteristics.

The result list is in date order, the first entry being today, and five
days of forecast being given including today."
  (let* ((location (or site metoffice-home-location))
	 (raw-site-data (cdr
			 (assoc 'SiteRep (metoffice-get-json-data
					  (if (string-match "\\`[0-9]+\\'" location)
					      location
					    (metoffice-get-site-id-by-name location))
					  "res=daily&"))))
	 (parameter-descriptions (map 'list
				      (function 
				       (lambda (param)
					 (cons (intern (cdr (assoc 'name param)))
					       (intern
						(downcase 
						 (replace-regexp-in-string
						  " " "-"
						  (cdr (assoc '$ param))))))))
				      (cdr (assoc 'Param
						  (cdr (assoc 'Wx raw-site-data))))))
	 (forecast-data (cdr (assoc 'Period
				    (cdr (assoc 'Location
						(cdr (assoc 'DV raw-site-data))))))))
    (map 'list
	 (function
	  (lambda (raw-data)
	    (cons (cdr (assoc 'value raw-data))
		  (map 'list
		       (function
			(lambda (raw-array-entry)
			  (cons (intern (downcase (cdr (assoc '$ raw-array-entry))))
				(mapcar (function
					 (lambda (raw-pair)
					   (let ((data-name (or (cdr (assoc (car raw-pair)
									    parameter-descriptions))
								(car raw-pair)))
						 (data-string (cdr raw-pair)))
					     (if (eq data-name '$)
						 (cons 'period (intern (downcase data-string)))
					       (cons data-name
						     (cond
						      ((eq data-name 'weather-type)
						       (or (cdr (assoc (string-to-number data-string)
								       metoffice-weather-codes))
							   data-string))
						      ((eq data-name 'visibility)
						       (or (cdr (assoc (intern data-string)
								       metoffice-visibility-codes-alist))
							   data-string))
						      ((string-match "\\`[-0-9]+\\'" data-string)
						       (string-to-number data-string))
						      (t data-string)))))))
					raw-array-entry))))
		       (cdr (assoc 'Rep raw-data))))))
	 forecast-data)))

(defun metoffice-get-site-period-weather (&optional site offset-days period)
  "Get the weather for SITE (or the default) for OFFSET-DAYS ahead at PERIOD.
SITE defaults as for `metoffice-get-site-weather'.  OFFSET-DAYS is 0 by default,
meaning today, and PERIOD can be 'day or 'night, the default being chosen
according to the time this function was called at."
  ;;       The documentation says "The value of the Rep object($ in
  ;;       the JSON representation) denotes the number of minutes
  ;;       after midnight UTC on the day represented by the Period
  ;;       object in which the Rep object is found."
  ;;       However, there's nothing in what json.el has made of it
  ;;       that looks like a time in minutes, so I can't use that to
  ;;       decide when day and night meet in terms of the data.
  (cdr (assoc (or period
		  (let ((hour (nth 2 (decode-time))))
		    (if (and (>= hour 6) ; simple 12-hour split, in absence of sunrise/sunset data from ephemeris etc
			     (<= hour 18))
			'day
		      'night)))
	      (nth (or offset-days 0)
		   (metoffice-get-site-weather site)))))

(defun metoffice-weather-aspect (weather aspect)
  "From WEATHER get ASPECT."
  (cdr (assoc weather aspect)))

;;; metoffice.el ends here
