;;;; bbdb-families.el -- for working out who is related to whom
;;; Time-stamp: <2002-03-18 12:47:18 jcgs>

(provide 'bbdb-families)
(require 'bbdb-com)
(require 'add-lispdir)

(defun bbdb-raw-family (person)
  "Return the raw family string of PERSON, which may be a record or a name."
  (when (stringp person)
    (setq person (bbdb-search (bbdb-records) person)))
  (when (consp person) (setq person (car person)))
  (bbdb-record-getprop person 'family))

(defun bbdb-family-raw-string-list (person)
  "Return the comma-separated strings making up the family of PERSON."
  (let ((rawstring (bbdb-raw-family person)))
    (if (null rawstring)
	nil
      (let ((relatives nil))
	(while (string-match "\\([^,]+\\), \\(.+\\)$" rawstring)
	  (setq relatives (cons (substring rawstring (match-beginning 1) (match-end 1))
				relatives)
		rawstring (substring rawstring (match-beginning 2) (match-end 2))))
	(setq relatives (cons rawstring relatives))
	(nreverse relatives)))))

(defconst bbdb-relationship-symbols
  '(("w" . wife) ("wife" . wife)
    ("h" . husband) ("husband" . husband) ("hus" . husband)
    ("d" . daughter) ("dau" . daughter) ("daughter" . daughter)
    ("s" . "son or sister") ("son" . son)
    ("b" . "brother") ("bro". brother) ("brother" . brother)
    ("son" . "son")
    ("ex-wife" . ex-wife)
    ("ex-husband" . ex-husband))
  "Alist for turning relationship strings into names")

(defun bbdb-relative-strip-email (str)
  "If STR seems to have an email address in it, return it without it."
  (if (and (stringp str)
	   (string-match "^\\([^<]*\\) ?<\\(.*\\)> \\(.*\\)$" str))
      (concat (substring str 0 (match-end 1)) (substring str (match-beginning 3)))
    str))

(defun bbdb-relative-pair (string)
  "Turn STRING into a relationship pair, eg \"Mary (w)\" into (\"Mary\" . wife)"
  (setq string (bbdb-relative-strip-email string))
  (if (string-match "\\(.+\\) +(\\([-a-z]+\\))" string)
      (let* ((name (substring string (match-beginning 1) (match-end 1)))
	    (rawrel (substring string (match-beginning 2) (match-end 2)))
	    (looked-up (assoc rawrel bbdb-relationship-symbols)))
	(if looked-up
	    (cons name (cdr looked-up))
	  (cons name 'unknown)))
    (cons name 'unknown)))

(defun bbdb-surname (str)
  "Return the last part of the person name STR"
  (if (string-match ".+ \\([^ ]+\\)" str)
      (substring str (match-beginning 1) (match-end 1))
    str))

(defun bbdb-relative-ensure-surname (person relative-to)
  "Make sure PERSON has a surname; that of RELATIVE-TO is used if they don't yet have one."
  (if (consp person)
      (cons (bbdb-relative-ensure-surname (car person))
	    (cdr person))
    (if (string-match ".+ .+" person)
	person
      (let* ((relname (if (stringp relative-to)
			  relative-to
			(bbdb-record-name relative-to)))
	     (relsurname (bbdb-surname relname)))
	(concat person " " relsurname)))))

(defun bbdb-relatives (person)
  "Return the relatives of PERSON, annotated with their relationship to them."
  (mapcar (function
	   (lambda (pair)
	     (cons (bbdb-relative-ensure-surname (car pair) person)
		   (cdr pair))))
	  (mapcar 'bbdb-relative-pair
		  (bbdb-family-raw-string-list person))))

(defun bbdb-family-select (relatives type)
  "Return, from RELATIVES, just those of TYPE."
  (remove-if-not (function
		  (lambda (pair)
		    (eq (cdr pair) type)))
		 relatives))

(defun bbdb-family-select-brothers (relatives)
  "Return just the brothers in RELATIVES."
  (bbdb-family-select relatives 'brother))

(defun bbdb-family-select-sisters (relatives)
  "Return just the sisters in RELATIVES."
  (bbdb-family-select relatives 'sister))

(defun bbdb-family-select-daughters (relatives)
  "Return just the daughters in RELATIVES."
  (bbdb-family-select relatives 'daughter))

(defun bbdb-family-select-sons (relatives)
  "Return just the sons in RELATIVES."
  (bbdb-family-select relatives 'son))

(defstruct bbdb-family
  surname
  father
  mother
  sons
  daughters)

(defvar bbdb-families nil
  "List of known families, by surname.")

(defun bbdb-family (person)
  "Construct a family structure for PERSON."
  (let* ((surname (bbdb-surname person))
	 (family-pair (assoc surname bbdb-families))
	 (family)
	 (family-members (bbdb-relatives person))
	 )
    (if family-pair
	(setq family (cdr family-pair))
      (setq family (make-bbdb-family :surname surname)
	    family-pair (cons surname family)
	    bbdb-families (cons family-pair bbdb-families)))
    (let ((wife (rassq 'wife family-members))
	  (husband (rassq 'husband family-members))
	  (father (rassq 'father family-members))
	  (mother (rassq 'mother family-members))
	  (daughters (bbdb-family-select-daughters family-members))
	  (sons (bbdb-family-select-sons family-members))
	  (sisters (bbdb-family-select-sisters family-members))
	  (brothers (bbdb-family-select-brothers family-members))
	  )
      (when wife
	(setf (bbdb-family-father family) person
	      (bbdb-family-mother family) wife))
      (when husband
	(setf (bbdb-family-father family) husband
	      (bbdb-family-mother family) person))
      )))

;;; end of bbdb-families.el

