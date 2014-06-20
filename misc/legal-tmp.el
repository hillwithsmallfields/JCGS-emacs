(defun read-person (prompt)
  (completing-read prompt
		   '(("The Applicant")
		     ("Kennedy Reed")
		     ("Helen Kevan")
		     ("Simon Kelley")
		     ("Ed Griffiths")
		     ("David Davison")
		     ("Murray Cairns")
		     ("Dr Bulpitt")
		     ("Richard Durbin")
		     ("Kirsten Monk")
		     ("Dr Huskisson"))
		   nil
		   nil
		   nil))

(defun legal-add-letter (date type from to description)
  (interactive
   (let* ((date (read-from-minibuffer "Date: "))
	  (type (read-from-minibuffer "Type: "))
	  (from (read-person "From: "))
	  (to (read-person "To: "))
	  (description (read-from-minibuffer "Description: ")))
     (list date type from to description)))
  (insert "<tr><td class=\"seqn\">1</td><td class=\"appn\">.</td><td class=\"resn\">.</td><td class=\"doc\">")
  (when (and (not (string= from "")) (not (string= to "")))
    (insert type " from " from " to " to " "))
  (insert description
	  "</td><td class=\"date\">"
	  date
	  "</td><td class=\"refp\">.</td></tr>\n"))
