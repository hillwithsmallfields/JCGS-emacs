;;;; metoffice.el --- handle data from the UK's Meteorological Office
;;; Time-stamp: <2015-03-15 17:57:11 jcgs>

(require 'json)

(defvar metoffice-base-url "http://datapoint.metoffice.gov.uk/public/data/val/wxfcs/all/json/"
  "The base address of the Met Office data site.")

(defvar metoffice-config-file "~/.metoffice-config.el"
  "The name of the file containing your metoffice config.
This should set metoffice-api-key to the key you obtained from the web site.")

(defun metoffice-setup ()
  "Ensure you have your metoffice data set up."
  (when (or (not (boundp 'metoffice-api-key))
	    (not (stringp metoffice-api-key)))
    (load-file metoffice-config-file)))

(defvar metoffice-api-key nil
  "The API key you got when you registered with the Met Office site.")

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

(defun metoffice-get-site-weather (site)
  "Return an alist of forecasts for SITE.
The car of each entry is a date, and the cdr is a cons of the day
and night forecasts."
  (map 'list
       (function
	(lambda (raw-data)
	  (let ((day-night (cdr (assoc 'Rep raw-data))))
	    (cons (cdr (assoc 'value raw-data))
		  (cons (aref day-night 0)
			(aref day-night 1))))))
       (cdr
	(assoc 'Period
	       (cdr
		(assoc 'Location
		       (cdr
			(assoc 'DV
			       (cdr
				(assoc 'SiteRep
				       (metoffice-get-json-data
					(if (string-match "\\`[0-9]+\\'" site)
					    site
					  (metoffice-get-site-id-by-name site))
					"res=daily&")))))))))))

;;; metoffice.el ends here
