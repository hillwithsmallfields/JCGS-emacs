;;;; irish.el -*- coding: utf-8 -*-  -- support for Irish-language text
;;; Time-stamp: <2007-07-08 16:48:51 jcgs>

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

;;;; todo: possibly make this into a "language environment"?

(defvar irish-urÃº-regexp "bhf\\|ng\\|dt\\|mb\\|gc\\|ts\\|h[aÃ¡eÃ©iÃ­oÃ³uÃº]"
  ;; todo: initial h, initial n
  "Pattern for urÃº (eclipsis) in Irish.")

(defvar irish-not-really-urÃº-regexp "halla\\|hata"
  "Pattern for things that match irish-urÃº-regexp but do not take an urÃº.")

(defadvice capitalize-word (around irish-capitalization (nwords) activate)
  "Apply Irish capitalization rules."
  (if irish-text-mode
    (save-match-data
      (let ((countdown (if (zerop nwords)
			   1
			 nwords)))
	(setq nwords 1)			; for ad-do-it
	(while (> countdown 0)
	  (unless (looking-at "\\<")
	    (re-search-forward "\\<" (point-max) t))
	  (if (and (not (looking-at irish-not-really-urÃº-regexp))
		   ;; do them in this order to leave the match-data as wanted
		   (looking-at irish-urÃº-regexp))
	      (let* ((capital-place (1- (match-end 0))))
		(downcase-word 1)
		(goto-char capital-place)
		ad-do-it
		(goto-char capital-place)
		(forward-word 1))
	    ad-do-it)
	  (setq countdown (1- countdown)))))
    ad-do-it))

;; todo: capitalize
;; todo: capitalize-region

(make-variable-buffer-local 'irish-text-mode)

(defvar irish-text-mode nil
  "Flag for Irish text mode.
This modifies capitalization, and allows easy typing of the accented letters needed.")

(or (assoc 'irish-text-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(irish-text-mode " Gaeilge")
		minor-mode-alist)))

(defvar irish-prefix-char "'"
  "*String containing the character we use to mark an accent.")

(defvar irish-prefix-keymap (make-sparse-keymap "Irish")
  "Keymap for inserting Irish characters.")
(define-key irish-prefix-keymap "a" 'iso-transl-a-acute)
(define-key irish-prefix-keymap "e" 'iso-transl-e-acute)
(define-key irish-prefix-keymap "i" 'iso-transl-i-acute)
(define-key irish-prefix-keymap "o" 'iso-transl-o-acute)
(define-key irish-prefix-keymap "u" 'iso-transl-u-acute)
(define-key irish-prefix-keymap "A" 'iso-transl-A-acute)
(define-key irish-prefix-keymap "E" 'iso-transl-E-acute)
(define-key irish-prefix-keymap "I" 'iso-transl-I-acute)
(define-key irish-prefix-keymap "O" 'iso-transl-O-acute)
(define-key irish-prefix-keymap "U" 'iso-transl-U-acute)
(define-key irish-prefix-keymap "'" 'irish-insert-prefix)

(defvar old-binding-for-prime nil
  "The binding for the prime character, that we usurp as an accent.")

(defun irish-insert-prefix ()
  "Insert the character we usurped for an accent."
  (interactive)
  (if old-binding-for-prime
      (call-interactively old-binding-for-prime)))

(defun irish-text-mode (&optional arg)
  "Toggle Irish text mode.
With an argument, set Irish text mode if the argument is >= 0."
  ;;;; todo: why isn't this an input method?
  (interactive "P")
  (if (or (and arg
	       (< 0 (prefix-numeric-value arg)))
	  irish-text-mode)
      (setq irish-text-mode nil)
    (setq irish-text-mode t)
    (require 'iso-transl)
    (if (null old-binding-for-prime)
	(setq old-binding-for-prime (key-binding irish-prefix-char)))
    (local-set-key irish-prefix-char irish-prefix-keymap)))


(defvar irish-caol-leathan-mismatch-regexp
  "\\([aouáóú][bcdfghklmnpqrstvwxyz]+[eiéÂí]\\)\\|\\([eiéÂí][bcdfghklmnpqrstvwxyz]+[aouáóú]\\)"
  "Pattern for bad combinations of vowels before and after consonants.
This applies to the Irish (Gaelic) langauge.")

(defun irish-check-caol-leathan-region (begin end)
  "Check the \"caol le caol agus leathan le leathan\" rule between BEGIN and END."
  (interactive "r")
  (let ((old-point (point)))
    (goto-char (region-beginning))
    (let ((found (re-search-forward irish-caol-leathan-mismatch-regexp
				    (region-end) t)))
      (if found
	  (progn
	    (message "Found mismatch")
	    (goto-char found))
	(goto-char old-point)))))

(defun irish-utf8-to-html-region (start end)
  "Convert utf8 encodings to html between START and END."
  (interactive "r")
  (let ((crioch (copy-marker end ))
	(case-fold-search nil))
    (mapcar (lambda (pair)
	      (goto-char start)
	      (while (search-forward (car pair) end t)
		(replace-match (cdr pair))))
	    '(("Ã¡".  "&aacute;")
	      ("Ã©".  "&eacute;")
	      ("Ã­".  "&iacute;")
	      ("Ã³".  "&oacute;")
	      ("Ãº".  "&uacute;")
	      ("Ã".  "&Aacute;")
	      ("Ã‰".  "&Eacute;")
	      ("Ã".  "&Iacute;")
	      ("Ã“".  "&Oacute;")
	      ("Ãš".  "&Uacute;")))))

(provide 'irish)

;;; end of irish.el
