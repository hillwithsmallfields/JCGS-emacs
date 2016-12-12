;;;; unicode-fonts.el -- set up fonts for unicode
;;; Time-stamp: <2007-08-12 22:26:25 jcgs>

(set-language-environment "UTF-8")

(when t nil
;;;; from http://paste.lisp.org/display/7411
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Everson Mono Unicode - Unicode encoded TrueType font, version  Version 3.2b4, 4.899 glyphs -- simply the best!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (create-fontset-from-fontset-spec "-*-everson mono unicode-medium-r-*-*-9-*-*-*-*-*-fontset-09pt_everson_mono" t 'noerror)
  (set-fontset-font "fontset-09pt_everson_mono"
		    'latin-iso8859-1   '("everson mono unicode" . "iso8859-1"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'latin-iso8859-2   '("everson mono unicode" . "iso8859-2"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'latin-iso8859-3   '("everson mono unicode" . "iso8859-3"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'latin-iso8859-4   '("everson mono unicode" . "iso8859-4"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'cyrillic-iso8859-5   '("everson mono unicode" . "iso8859-5"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'hebrew-iso8859-8   '("everson mono unicode" . "iso8859-8"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'latin-iso8859-9   '("everson mono unicode" . "iso8859-9"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'latin-iso8859-15  '("everson mono unicode" . "iso8859-15"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'mule-unicode-0100-24ff  '("everson mono unicode" . "iso10646-1"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'mule-unicode-2500-33ff  '("everson mono unicode" . "iso10646-1"))
  (set-fontset-font "fontset-09pt_everson_mono"
		    'mule-unicode-e000-ffff  '("everson mono unicode" . "iso10646-1"))
					;
  )

(when nil t
  ;; this doesn't quite work either... I think I probably need to make a fontset for it too
  (add-to-list 'bdf-directory-list (substitute-in-file-name "$GATHERED/emacs/fonts/"))
  ;; (set-default-font "-Misc-Fixed-*-*-*--*-*-*-*-C-*-ISO10646-1")
  (setq w32-bdf-filename-alist
	(w32-find-bdf-fonts bdf-directory-list))
  ;; Then create fontsets for the BDF fonts
  (create-fontset-from-fontset-spec "-*-fixed-medium-r-normal-*-16-*-*-*-c-*-fontset-bdf, japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*, katakana-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*, latin-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*, japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*, thai-tis620:-misc-fixed-medium-r-normal--16-160-72-72-m-80-tis620.2529-1, lao:-misc-fixed-medium-r-normal--16-160-72-72-m-80-MuleLao-1, tibetan-1-column:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-80-MuleTibetan-1, ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--16-150-100-100-M-160-Ethiopic-Unicode, tibetan:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-160-MuleTibetan-0") 


  ;; Need to add some fonts to font-encoding-alist since the bdf fonts
  ;; are type 0 not the default type 1.
  (setq font-encoding-alist           (append '(("MuleTibetan-0" (tibetan . 0))
						("GB2312"        (chinese-gb2312 . 0))
						("JISX0208"      (japanese-jisx0208 . 0))
						("JISX0212"      (japanese-jisx0212 . 0))
						("VISCII"        (vietnamese-viscii-lower . 0))
						("KSC5601"       (korean-ksc5601 . 0))
						("MuleArabic-0"  (arabic-digit . 0))
						("MuleArabic-1"  (arabic-1-column . 0))
						("MuleArabic-2"  (arabic-2-column . 0))) font-encoding-alist))
  ;; Tell NT Emacs to use a font menu similar to the one it uses on Unix
  (setq w32-use-w32-font-dialog nil))

;; For NTemacs, see $GATHERED/emacs/fonts/FontSetsForNTEmacs.html
;; (from EmacsWiki, I think) and also $GATHERED/emacs/fonts/faq5.html

;; (set-frame-font "-outline-Lucida Sans Unicode-normal-r-normal-normal-27-202-96-96-p-*-iso10646-1") works but is proportionally spaced

;; this seems to be it!

(defvar everson-mono-sizes
  '((72 . "-outline-Everson Mono Unicode-normal-r-normal-normal-96-720-96-96-c-*-iso10646-1")
    (48 . "-outline-Everson Mono Unicode-normal-r-normal-normal-64-480-96-96-c-*-iso10646-1")
    (36 . "-outline-Everson Mono Unicode-normal-r-normal-normal-48-360-96-96-c-*-iso10646-1")
    (28 . "-outline-Everson Mono Unicode-normal-r-normal-normal-37-277-96-96-c-*-iso10646-1")
    (26 . "-outline-Everson Mono Unicode-normal-r-normal-normal-35-262-96-96-c-*-iso10646-1")
    (24 . "-outline-Everson Mono Unicode-normal-r-normal-normal-32-240-96-96-c-*-iso10646-1")
    (22 . "-outline-Everson Mono Unicode-normal-r-normal-normal-29-217-96-96-c-*-iso10646-1")
    (20 . "-outline-Everson Mono Unicode-normal-r-normal-normal-27-202-96-96-c-*-iso10646-1")
    (18 . "-outline-Everson Mono Unicode-normal-r-normal-normal-24-180-96-96-c-*-iso10646-1")
    (16 . "-outline-Everson Mono Unicode-normal-r-normal-normal-21-157-96-96-c-*-iso10646-1")
    (14 . "-outline-Everson Mono Unicode-normal-r-normal-normal-19-142-96-96-c-*-iso10646-1")
    (12 . "-outline-Everson Mono Unicode-normal-r-normal-normal-16-120-96-96-c-*-iso10646-1")
    (11 . "-outline-Everson Mono Unicode-normal-r-normal-normal-15-112-96-96-c-*-iso10646-1")
    (10 . "-outline-Everson Mono Unicode-normal-r-normal-normal-13-97-96-96-c-*-iso10646-1")
    (9 . "-outline-Everson Mono Unicode-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1")
    (8 . "-outline-Everson Mono Unicode-normal-r-normal-normal-11-82-96-96-c-*-iso10646-1"))
  "Alist of font sizes to Everson Mono Unicode font name for that size.")

(defun use-everson-mono (&optional size)
  "Select Everson Mono unicode font."
  (interactive 
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   12)))
  (let ((font (cdr (assoc size everson-mono-sizes))))
    (if font
	(set-frame-font font)
      (error "Everson Mono Unicode not defined at size %S" size))))

(defvar sampler-category-names nil)

(defun sampler-category-names (cat-desc)
  (let ((pair (assoc cat-desc sampler-category-names)))
    (if pair
	(cdr pair)
      (let ((new (mapconcat 'category-docstring cat-desc ", ")))
	(push (cons cat-desc new) sampler-category-names)
	new))))

(defun make-unicode-sampler (file)
  "Make a unicode sampler and put it in FILE."
  (interactive "FFile to put sampler in: ")
  (find-file file)
  (erase-buffer)
  (set-buffer-file-coding-system 'utf-8)
  (dotimes (i (* 256 256))
    (condition-case evar
	(let* ((cat-desc (condition-case ev
			     (category-set-mnemonics
			      (char-category-set i))
			   (error nil))))
	  (ucs-insert i)
	  (insert (format "\t%04x  %05d %s\n" i i
			  (sampler-category-names cat-desc))))
      (error
       (insert (format "-na-\t%04x  %05d\n" i i)))))
  (basic-save-buffer))

(provide 'unicode-fonts)

;;; end of unicode-fonts.el
