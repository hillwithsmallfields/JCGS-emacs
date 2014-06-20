;;;; Configuration for international things
;;; Time-stamp: <2013-10-15 12:22:11 johnstu>

;; Copyright (C) 2008, 2009, 2010, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2008
;; Keywords: setup

;; This file is NOT part of GNU Emacs.

;;;; Input methods

(add-to-list 'load-path (expand-file-name "natural-language" user-emacs-directory))

(load-library "mongolian")
(load-library "extra-input-methods")

(defvar input-methods-wanting-diagrams
  '(("mongolian-cyrillic" . quail-show-keyboard-layout-compactly)
    ("russian-computer" . quail-show-keyboard-layout-compactly)
    ("hebrew" . quail-show-keyboard-layout-compactly)
    ("japanese" . show-hiragana-table)
    ("czech-qwerty" . quail-show-keyboard-layout-compactly)
    ("icelandic-keyboard" . quail-show-keyboard-layout-compactly)
    )
  "Input methods for which I'd like diagrams displayed.")

(defun quail-keyboard-show-hands-row (left-keys right-letters)
  "Colour the background of LEFT-KEYS and RIGHT-LETTERS in this row."
  (back-to-indentation)
  (let* ((row-start (point))
	 (row-middle (search-forward "|"
				     (point-max) t left-keys))
	 (row-letters-end (search-forward "|"
					  (point-max) t right-letters))
	 (row-end (line-end-position)))
    (put-text-property row-start row-middle
		       'face (cons 'foreground-color "red"))
    (put-text-property row-middle row-letters-end
		       'face (cons 'foreground-color "blue"))
    (put-text-property row-letters-end row-end
		       'face (cons 'foreground-color "dark green"))))

(defvar quail-keyboard-layout-hand-rows-alist
  '(("standard" (6 5) (6 5) (6 4) (6 2)))
  "How many left-hand keys and right-hand letters there are for each row.
This is an alist with an entry for each keyboard type.")

(defun quail-show-keyboard-layout-compactly ()
  "Show the physical layout of the current keyboard type.
The variable `quail-keyboard-layout-type' holds the currently selected
keyboard type."
  (interactive)
  (let ((layout (assoc quail-keyboard-layout-type
		       quail-keyboard-layout-alist)))
    (or layout
	(error "Unknown keyboard type: %s"
	       quail-keyboard-layout-type))
    (save-excursion
      (let ((bufname "*Keyboard layout*"))
	(with-output-to-temp-buffer bufname
	  (with-current-buffer standard-output
	    (insert "Keyboard layout (keyboard type: "
		    quail-keyboard-layout-type
		    ")\n")
	    (quail-insert-kbd-layout (cdr layout))))
	(set-buffer bufname)
	(let ((inhibit-read-only t)
	      (layout-data
	       (cdr (assoc quail-keyboard-layout-type
			   quail-keyboard-layout-hand-rows-alist))))
	  (goto-char (point-min))
	  (search-forward "+")
	  (beginning-of-line 2)
	  (dolist (row layout-data)
	    (apply 'quail-keyboard-show-hands-row row)
	    (beginning-of-line 3)))))))

(defvar show-hirigana-table-landscape t)

(defun show-hiragana-table ()
  "Show a hiragana table."
  (interactive)
  (with-output-to-temp-buffer "*Keyboard layout*"
    (princ
     (if show-hirigana-table-landscape
	 "あ	か	さ	た	な	は	ま	や	ら	わ
a	ka	sa	ta	na	ha	ma	ya	ra	wa

い	き	し	ち	に	ひ	み		り
i	ki	si	chi	ni	hi	mi		ri

う	く	す	つ	ぬ	ふ	む	ゆ	る
u	ku	su	tsu	nu	fu	mu	yu	ru

え	け	せ	て	ね	へ	め		れ
e	ke	se	te	ne	he	me		re

お	こ	そ	と	の	ほ	も	よ	ろ	を
o	ko	so	to	no	ho	mo	yo	ro	wo
"
       "あ	い	う	え	お
a	i	u	e	o

か	き	く	け	こ
ka	ki	ku	ke	ko

さ	し	す	せ	そ
sa	shi	su	se	so

た	ち	つ	て	と
ta	chi	tsu	te	to

な	に	ぬ	ね	の
na	ni	nu	ne	no

は	ひ	ふ	へ	ほ
ha	hi	fu	he	ho

ま	み	む	め	も
ma	mi	mu	me	mo

や		ゆ		よ
ya		yu		yo

ら	り	る	れ	ろ
ra	ri	ru	re	ro

ゑ				を
wa				wo
"
       ))))

(defun jcgs-input-method-activate-hook ()
  "Show stuff concerning the new input method."
  (let ((pair (assoc current-input-method input-methods-wanting-diagrams)))
    ;; (message "current-input-method %s, input-methods-wanting-diagrams %S, result %S" current-input-method input-methods-wanting-diagrams pair)
    (if pair
	(funcall (cdr pair))
      (let ((buffer (get-buffer "*Keyboard layout*")))
	;; (message "got buffer %s" buffer)
	(when buffer
	  (let ((window (get-buffer-window buffer)))
	    ;; (message "got window %s" window)
	    (when window
	      (delete-window window))))))))
  
(add-hook 'input-method-activate-hook
	  'jcgs-input-method-activate-hook)

;; config-international.el ends here
