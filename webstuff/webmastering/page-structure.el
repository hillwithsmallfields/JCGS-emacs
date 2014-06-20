;;;; <title>Ensure page has structure markers</title>
;;; Time-stamp: <2005-01-18 12:05:56 john>

(provide 'page-structure)

(defvar html-head-section-start "<!-- start of controlled head -->"
  "Marker string for start of controlled section of head.")

(defvar html-head-section-end "<!-- end of controlled head -->"
  "Marker string for end of controlled section of head.")

(defvar html-tail-section-start "<!-- start of controlled tail -->"
  "Marker string for start of controlled section of tail.")

(defvar html-tail-section-end "<!-- end of controlled tail -->"
  "Marker string for end of controlled section of tail.")

;;; end of page-structure.el
