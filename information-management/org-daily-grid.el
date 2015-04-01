;;; org-daily-grid.el --- output a daily grid in PostScript, from an org subtree

;; Copyright (C) 2013, 2014, 2015  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, tools

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

;; Produce a PS or SVG file showing a grid of things to tick off each day.

;;; Code:

(require 'timezone)

(defun org-export-grid-count-headings (a)
  "Return the number of headings in A."
  (if (consp a)
      (1+ (reduce '+
		  (mapcar 'org-export-grid-count-headings
			  (cdr a))))
    0))

(defun org-export-grid-count-entries (a)
  "Return the number of non-heading entries in A."
  (cond
   ((consp a)
    (reduce '+
	    (mapcar 'org-export-grid-count-entries
		    (cdr a))))
   ((stringp a) 1)
   (t 0)))

(defvar org-export-grid-svg-y 0
  "The Y cursor.")

(defvar org-export-grid-svg-line-height 12
  "The Y cursor step for normal lines.")

(defvar org-export-grid-svg-heading-line-height 9
  "The Y cursor step for heading lines.")

(defvar org-export-grid-svg-top-margin 36
  "The margin to leave at the top.")

(defvar org-export-grid-indentation-step 6
  "The X step.")

(defvar org-export-grid-svg-days-column 120
  "Where to start the days.")

(defvar org-export-grid-svg-day-width 14
  "The space for each box.")

(defvar org-export-grid-svg-day-box-width 10
  "The width of each box drawing.")

(defvar org-export-grid-svg-day-box-height 10
  "The height of each box drawing.")

(defun org-export-grid-write-preamble (output-format year month)
  "Output the preamble for OUTPUT-FORMAT for YEAR MONTH."
  (cond
   ((eq output-format 'ps)
    (insert "%!PS\n")
    (insert "/indentfactor 10 def\n")
    (insert "/lineheight 10 def\n")
    (insert "/leftmargin 10 def\n")
    (insert "/rightmargin 800 def\n")
    (insert "/topmargin 20 def\n")
    (insert "/titleheight 20 def\n")
    (insert "/buffer 20 string def")
    (insert (format "/year %d def\n/month %d def\n" year month))
    (insert "/headingfont /Helvetica-Bold findfont 10 scalefont def\n")
    (insert "/entryfont /Helvetica findfont 10 scalefont def\n")
    (insert "/down { /textpos textpos lineheight sub def } def\n")
    (insert "/trackedge { dup textedge gt { /textedge exch def } { pop } ifelse } def\n")
    (insert "/heading { gsave headingfont setfont indentfactor mul textpos moveto show currentpoint pop grestore trackedge down} def\n")
    (insert "/entry { gsave entryfont setfont indentfactor mul textpos moveto show currentpoint pop grestore trackedge down} def\n")
    (insert "/daycolumns { dup rightmargin leftmargin sub exch div dup == /perday exch def 1 1 3 -1 roll { dup perday mul textedge add topmargin lineheight sub moveto buffer cvs show } for } def\n")
    (insert "/textedge 0 def\n")
    (insert "/textpos 0 def\n")
    (insert (format "/nheadings %d def /nentries %d def\n" headings entries))
    (insert "currentpagedevice /PageSize get dup 0 get /pagewidth exch def 1 get /pageheight exch def\n")
    (insert "leftmargin pageheight topmargin sub translate\n")
    (insert (format "0 leftmargin rightmargin add 2 div moveto (%s) dup stringwidth pop -2 div 0 rmoveto show\n"
		    (calendar-month-name month)))
    ;; (insert "0 titleheight sub translate\n")
    (insert "0 0 moveto\n")
    (insert "gsave\n"))
   ((eq output-format 'svg)
    (insert "<svg height=\"297mm\" width=\"210mm\">\n")
    ;; to show the supposed page edge, although it doesn't look right to me compared with the real printout
    ;; (insert "<rect x=\"0\" y=\"0\" height=\"296mm\" width=\"209mm\" style=\"stroke:blue;fill:white\"/>\n")
    (insert
     (format
      "<text font-weight=\"bold\" font-size=\"large\" x=\"%d\" y=\"%d\">%s: %s %d</text>"
      org-export-grid-svg-days-column
      (/ org-export-grid-svg-top-margin 2) ; todo: improve layout and calculation
      (user-full-name)
      (calendar-month-name month)
      year))
    (setq org-export-grid-svg-y org-export-grid-svg-top-margin))))

(defun org-export-grid-write-postamble (output-format year month)
  "Output the postamble for OUTPUT-FORMAT for YEAR MONTH."
  (cond
   ((eq output-format 'ps)
    (insert "grestore\n")
    (insert "textedge topmargin moveto textedge textpos lineto stroke\n")
    (insert (format "%d daycolumns\n" (calendar-last-day-of-month month year)))
    (insert "showpage\n"))
   ((eq output-format 'svg)
    (insert "</svg>\n"))))

(defun org-export-grid-write-day-columns (output-format n-days year month)
  "Output the day columns for OUTPUT-FORMAT for N-DAYS YEAR MONTH."
  (message "month is %S, year is %S" month year)
  (cond
   ((eq output-format 'ps)
    nil)
   ((eq output-format 'svg)
    (let* ((adjust-x (/ (- org-export-grid-svg-day-width
			   org-export-grid-svg-day-box-width)
			2))
	   (lines-x (- org-export-grid-svg-days-column
		       adjust-x)))
      (dotimes (i (1+ n-days))
	(message "i = %d" i)
	(let* ((this-time (encode-time 1 1 1 i month year))
	       (decoded-time (decode-time this-time))
	       (dom (nth 3 decoded-time))
	       (dow (nth 6 decoded-time)))
	  (insert (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke:%s\" stroke-width=\"0.5\"/>\n"
			  lines-x org-export-grid-svg-top-margin
			  lines-x org-export-grid-svg-y
			  (if (= dow 6) "red" "black")))
	  (unless (zerop i)
	    (insert (format "<text x=\"%d\" y=\"%d\" fill=\"%s\">%d</text>"
			    (+ (- lines-x
				  org-export-grid-svg-day-width)
			       adjust-x)
			    (+ org-export-grid-svg-top-margin
			       adjust-x) ; happens to be a suitable amount
			    (case dow
			      (0 "red")
			      (6 "darkgreen")
			      (t "black"))  
			    dom))))
	(setq lines-x (+ lines-x org-export-grid-svg-day-width))))

    ;; org-export-grid-svg-day-width
    ;; org-export-grid-svg-day-box-width
    )))

(defun org-export-grid-write-heading (heading output-format n-days depth)
  "Insert HEADING in OUTPUT-FORMAT for N-DAYS at DEPTH.."
  (cond
   ((eq output-format 'ps)
    (insert (format "(%s) %d heading\n" heading depth)))
   ((eq output-format 'svg)
    (insert (format "<text font-size=\"75%%\" font-weight=\"bold\" x=\"%d\" y=\"%d\">"
		    (* depth org-export-grid-indentation-step)
		    org-export-grid-svg-y)
	    heading
	    "</text>\n")
    (setq org-export-grid-svg-y (+ org-export-grid-svg-y org-export-grid-svg-heading-line-height)))))

(defun org-export-grid-write-entry (entry output-format n-days depth year month)
  "Insert ENTRY in OUTPUT-FORMAT for N-DAYS at DEPTH.
YEAR and MONTH are given so that days of the week can be
marked with colour."
  (cond
   ((eq output-format 'ps)
    (insert (format "(%s) %d entry\n" entry depth)))
   ((eq output-format 'svg)
    (insert (format "<text x=\"%d\" y=\"%d\">"
		    (* depth org-export-grid-indentation-step)
		    org-export-grid-svg-y)
	    entry
	    "</text>\n")
    (when nil
      (insert (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" />\n"
		      0 org-export-grid-svg-y
		      1000 org-export-grid-svg-y)))
    (dotimes (day n-days)
      (let* ((this-time (encode-time 1 1 1 day month year))
	       (decoded-time (decode-time this-time))
	       (dom (nth 3 decoded-time))
	       (dow (nth 6 decoded-time)))
	(insert (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" style=\"stroke:%s;fill:white\"/>\n"
			(+ org-export-grid-svg-days-column
			   (* day org-export-grid-svg-day-width))
			org-export-grid-svg-y
			org-export-grid-svg-day-box-width
			(- org-export-grid-svg-day-box-height)
			(case dow
			  (6 "red")
			  (5 "darkgreen")
			  (t "black"))))))
    (setq org-export-grid-svg-y (+ org-export-grid-svg-y org-export-grid-svg-line-height)))))

(defun org-export-grid-write-tree (tree output-format n-days depth year month)
  "Insert the PostScript or SVG for TREE in OUTPUT-FORMAT for N-DAYS at DEPTH.
YEAR and MONTH are given so that days of the week can be
marked with colour."
  (if (consp tree)
      (progn
	(org-export-grid-write-heading (car tree) output-format n-days depth)
	(let ((deeper (1+ depth)))
	  (dolist (entry (cdr tree))
	    (org-export-grid-write-tree entry output-format n-days deeper year month))))
    (org-export-grid-write-entry tree output-format n-days depth year month)))

(defun month-length (year month)
  "Return the number of days in YEAR's MONTH."
  (let ((this-month-start (encode-time 0 0 0
				       1
				       month
				       year))
	(next-month-start (encode-time 0 0 0
				       1
				       (% (1+ month) 12)
				       (if (= month 12)
					   (1+ year)
					 year))))
    (/ (time-to-number-of-days (time-subtract next-month-start
					      this-month-start))
       (* 24 60 60))))

(defconst org-export-grid-output-formats
  '(("SVG" . svg) ("PostScript" . ps))
  "The allowable output formats.")

(defun org-export-grid (file year month output-format)
  "Export the current subtree as a daily grid.
Argument FILE is the file to write the PostScript into.
YEAR and MONTH indicate the month to use.
Argument OUTPUT-FORMAT is 'svg; it might some day allow others."
  (interactive
   (let* ((completion-ignore-case t)
	  (now (decode-time))
	  (day (nth 3 now))
	  (now-month (nth 4 now))
	  (month (if (<= day 15)
		     now-month
		   (1+ (% now-month
			  12))))
	  (year (if (and (<= day 15)
			 (= now-month 12))
		    (1+ (nth 5 now))
		  (nth 5 now)))
	  (filename (read-file-name "Output grid to file: ")))
     (list filename
	   (string-to-number
	    (read-string (format "Year (default %d): " year)
			 nil nil (number-to-string year)))
	   (string-to-number
	    (read-string (format "Month (default %d): " month)
			 nil nil (number-to-string month)))
	   (cond
	    ((string-match "\\.ps$" filename)
	     'ps)
	    ((string-match "\\.svg$" filename)
	     'svg)
	    (t (cdr (assoc (completing-read "Output format: " org-export-grid-output-formats nil t)
			   org-export-grid-output-formats)))))))
  (let* ((tree (save-excursion
		 (org-export-grid-recursive 0)))
	 (headings (org-export-grid-count-headings tree))
	 (entries (org-export-grid-count-entries tree))
	 (n-days (timezone-last-day-of-month month year)))
    (message "tree is %S, with %d headings and %d entries, for %d days" tree headings entries n-days)
    (save-window-excursion
      (save-excursion
	(find-file file)
	(erase-buffer)
	(org-export-grid-write-preamble output-format year month)
	(org-export-grid-write-tree tree output-format n-days 0 year month)
	(org-export-grid-write-day-columns output-format n-days year month)
	(org-export-grid-write-postamble output-format year month)
	(basic-save-buffer)))))

(defun org-export-grid-recursive (depth)
  "Create a Lisp tree to export the current subtree as a daily grid.
DEPTH is for recursive use."
  (let ((level-heading (org-get-heading t t)))
    (if (org-goto-first-child)
	(let ((continue t)
	      (entry-start (point))
	      (level-entries nil)
	      (indent (make-string depth 32))
	      )
	  ;; (message "%slevel %d added heading %S" indent depth level-heading)
	  (while continue
	    (message "%slevel %d entry is %S" indent depth (buffer-substring-no-properties entry-start (line-end-position)))
	    (push (save-excursion
		    (org-export-grid-recursive (1+ depth)))
		  level-entries)
	    (setq continue (org-get-next-sibling)
		  entry-start continue))
	  (cons level-heading (nreverse level-entries)))
      (if (zerop depth)
	  (error "No children in tree")
	(org-get-heading t t)))))

(provide 'org-daily-grid)
;;; org-daily-grid.el ends here
