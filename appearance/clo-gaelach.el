;;; clo-gaelach.el --- experiments with Clo Gaelach fonts in Emacs
;;;; -*- coding: utf-8 -*-; clo-gaelach.el -- display Clo Gaelach in the Private Use Area of Unicode
;;; Time-stamp: <2008-01-06 13:20:48 jcgs>

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


;;; Commentary:
;; 


;;; History:
;; 

;;; Code:
(defvar clo-gaelach-fontset-spec
  "-*-mixed gaelach-*-*-*-*-*-*-*-*-*-*-fontset-mixedgaelach"
  "Fontset spec for Clo Gaelach.")

(defvar clo-gaelach-charset nil
  "The charset for clo gaelach.")

(defvar clo-gaelach-font
  "-outline-Bunchló Dubh GC-normal-r-normal-normal-*-*-96-96-p-*-iso10646-1" ; this one has the dots
  ;; "-outline-Kelt UNICODE-bold-r-normal-normal-*-*-96-96-p-*-iso10646-1"	; this one has the dots
  ;; "-outline-Seanchló Dubh GC-normal-r-normal-normal-*-*-96-96-p-*-iso10646-1"  ; possibly most legible?
  ;; "-outline-Everson Mono Unicode-normal-r-normal-normal-*-*-96-96-c-*-iso10646-1"
  "The font to use for Clo Gaelach.")

(defun clo-gaelach-setup ()
  "Set GNUemacs up to display Clo Gaelach for characters in part of the Private Use Area."
  (interactive)
  (let ((fontset (create-fontset-from-fontset-spec clo-gaelach-fontset-spec
						   nil
						   t)))
    (when nil
      ;; create a charset that we will use to describe the characters:
      ;; now not taking this approach, I couldn't find how to set
      ;; which characters are in the charset
      (setq clo-gaelach-charset (define-charset nil 'clo-gaelach
				  [ 1	; bytes per char
				    96	; chars per dimension
				    1	; width
				    0	; direction
				    nil	; iso-final-char
				    nil	; iso-graphic-plane
				    "gaelach" ; short-name
				    "Clo Gaelach" ; long-name
				    "Old-style Irish letters" ; description
				    ])))

    ;; set the font for the charset (and hence its characters):
    (set-fontset-font "fontset-mixedgaelach"
		      'latin-iso8859-1
		      "-outline-Everson Mono Unicode-normal-r-normal-normal-13-97-96-96-c-*-iso10646-1")
    (if nil
	(set-fontset-font "fontset-mixedgaelach"
			  (cons 315424 (+ 315424 255))
			  ;; clo-gaelach-charset
			  clo-gaelach-font))
    (if nil
	;; an experiment: try changing the font of a letter or a few letters
	(set-fontset-font "fontset-mixedgaelach"
			  ;; (cons ?a ?f)	; it says "Can't change font for a single byte character"
			  (make-char 'latin-iso8859-1 ?a) ; seems to work
			  "-outline-Aon Cari Celtic-normal-r-normal-normal-*-*-96-96-p-*-iso10646-1"))
    ;; (describe-fontset fontset)
    ;; (setq mixed-gaelach fontset)
    ;; it does that much, but I can't make the resulting fontset be the default font

    (setq clo-gaelach-face (make-face 'clo-gaelach-face t))
    (set-face-font 'clo-gaelach-face clo-gaelach-font)
    (set-face-attribute 'clo-gaelach-face nil :width 'expanded :height 1.6)
    fontset))

(defvar clo-gaelach-test nil)

(defun clo-gaelach-test ()
  "This works!"
  (interactive)
  (setq clo-gaelach-test (make-face 'clo-gaelach-test t))
  (set-face-font 'clo-gaelach-test clo-gaelach-font)
  (pop-to-buffer (get-buffer-create "*Clo Gaelach test*"))
  (erase-buffer)
  (insert "roimh; " (propertize "an rud;" 'face 'clo-gaelach-test) "tar eis.\n"))

(defun compare-letter-widths ()
  "Experiment with letter widths."
  (interactive)
  (pop-to-buffer (get-buffer-create "*test*"))
  (erase-buffer)
  (let ((letter ?\ ))
    (while (<= letter 127)
      (insert (make-string 72 letter) "\n")
      (setq letter (1+ letter)))))

(provide 'clo-gaelach)

;;; clo-gaelach.el ends here
