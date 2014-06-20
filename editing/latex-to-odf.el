;;;; latex-to-odf.el -- convert LaTeX to ODF
;;; Time-stamp: <2006-06-07 13:43:32 john>

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

(provide 'latex-to-odf)

(defvar odf-latex-preamble
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE office:document-content
  PUBLIC \"-//OpenOffice.org//DTD OfficeDocument 1.0//EN\"
  \"office.dtd\">
<office:document-content
  xmlns:office=\"http://openoffice.org/2000/office\"
  xmlns:style=\"http://openoffice.org/2000/style\"
  xmlns:text=\"http://openoffice.org/2000/text\"
  xmlns:table=\"http://openoffice.org/2000/table\"
  xmlns:draw=\"http://openoffice.org/2000/drawing\"
  xmlns:fo=\"http://www.w3.org/1999/XSL/Format\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  xmlns:number=\"http://openoffice.org/2000/datastyle\"
  xmlns:svg=\"http://www.w3.org/2000/svg\"
  xmlns:chart=\"http://openoffice.org/2000/chart\"
  xmlns:dr3d=\"http://openoffice.org/2000/dr3d\"
  xmlns:math=\"http://www.w3.org/1998/Math/MathML\"
  xmlns:form=\"http://openoffice.org/2000/form\"
  xmlns:script=\"http://openoffice.org/2000/script\"
  office:class=\"text\"
  office:version=\"1.0\">"
"Preamble for ODF files generated from LaTeX.")


(defun latex-to-odf (original)
  "Make an ODF approximation of ORIGINAL."
  (interactive "fLaTeX file to convert: ")
  (let ((odf-dir (file-name-sans-extension original))
	(odf-content (expand-file-name "content.xml" odf-dir)))
  (unless (file-directory-p odf-dir)
    (make-directory odf-dir))
  (copy-file original odf-content t)
  (save-excursion
    (find-file odf-content)
    (insert odf-latex-preamble)
    ))

;;; end of latex-to-odf.el
