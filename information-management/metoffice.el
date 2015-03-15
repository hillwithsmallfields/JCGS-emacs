;;;; metoffice.el --- handle data from the UK's Meteorological Office
;;; Time-stamp: <2015-03-15 22:31:24 jcgs>

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
					   (cons
					    (or (cdr (assoc (car raw-pair)
							    parameter-descriptions))
						(car raw-pair))
					    (let ((data-string (cdr raw-pair)))
					      (if (string-match "\\`[-0-9]+\\'" data-string)
						  (string-to-number data-string)
						data-string)))))
					raw-array-entry))))
		       (cdr (assoc 'Rep raw-data))))))
	 forecast-data)))

(defun metoffice-get-site-period-weather (&optional site offset-days period)
  "Get the weather for SITE (or the default) for OFFSET-DAYS ahead at PERIOD.
SITE defaults as for `metoffice-get-site-weather'.  OFFSET-DAYS is 0 by default,
meaning today, and PERIOD can be 'day or 'night, the default being 'day."
  (cdr (assoc (or period 'day)
	      (nth (or offset-days 0)
		   (metoffice-get-site-weather site)))))

;;; metoffice.el ends here
