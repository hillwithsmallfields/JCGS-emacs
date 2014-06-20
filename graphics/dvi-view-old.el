;;;; dvi-view.el -- View a DVI file in an Emacs buffer
;;; Time-stamp: <2007-08-19 14:24:12 jcgs>

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

;; Written 2007-08-18 by John C G Sturdy when he found himself on a
;; machine without xdvi.

;; todo: make this choose a format according to what converters are available

;; todo: find how to get page boundary information from AUCTeX, to make pages clickable

;; todo: parameters to pass to dvipng

(defgroup dvi-view
  nil
  "In-buffer viewer for TeX's dvi output.")

(defcustom dvi-view-image-options nil
  "How to display each image in dvi-view-file."
  :type 'list			      ; todo: real description will be hairy
  :group 'dvi-view)

(defcustom dvi-view-page-header "Page %d:\n"
  "String to put before each image."
  :type 'string
  :group 'dvi-view)

(defcustom dvi-view-page-footer "\n"
  "String to put after each image."
  :type 'string
  :group 'dvi-view)

(defun dvi-view-file (dvi-file)
  "View DVI-FILE in an Emacs buffer.
You can give the name of the TeX file, and it will use the DVI file anyway."
  (interactive "fView DVI file: ")
  (when (file-directory-p dvi-file)
    (setq dvi-file (buffer-file-name)))
  (let ((base-name (file-name-sans-extension (expand-file-name dvi-file)))
	(buffer (get-buffer-create " *dvipng output*")))
    (set-buffer buffer)
    (erase-buffer)
    (setq dvi-file (concat base-name ".dvi"))
    (message "file %S" dvi-file)
    (let ((png (call-process "/usr/bin/dvipng" ; program
			     nil	       ; infile
			     buffer	       ; buffer
			     nil	       ; display
			     dvi-file))
	  (pages nil))
      (if (/= png 0)
	  (error "Error %S in dvipng" png)
	(goto-char (point-min))
	(while (re-search-forward "\\[\\([0-9]+\\)\\]" (point-max) t)
	  (let* ((page (match-string-no-properties 1))
		 (page-file (concat base-name page ".png")))
	    (push (cons (string-to-int page) page-file) pages)))
	(let ((view-buffer (get-buffer-create (format "*View of %s*" (file-name-nondirectory base-name)))))
	  (set-buffer view-buffer)
	  (erase-buffer)
	  (dolist (page (nreverse pages))
	    (let ((image (create-image (cdr page) nil nil dvi-view-image-options)))
	      (when dvi-view-page-header
		(insert (format dvi-view-page-header (car page))))
	      (insert-image image)
	      (when dvi-view-page-footer
		(insert (format dvi-view-page-footer (car page))))))
	  (goto-char (point-min))
	  (pop-to-buffer view-buffer))))))

(provide 'dvi-view)

;;; end of dvi-view.el
