;;;; Switch colour themes
;;; Time-stamp: <2015-03-16 21:35:27 jcgs>

;;; Use nicer ones when I'm clocked in to some task, to encourage me
;;; to be clocked in more of the time.

;; (add-lispdir "$GATHERED/emacs/color-theme")

;; (require 'color-theme)

(load-file (substitute-in-file-name "$GATHERED/emacs/color-theme/color-theme.el"))

(defvar jcgs/org-task-color-themes
  [color-theme-wheat			; quite nice
   color-theme-whateveryouwant 		; a bit odd
   ;; color-theme-katester ; too pastel
   ;; color-theme-vellum ; todo: change the pale blue bit
   ;; color-theme-mistyday ; doesn't cancel old dark theme well enough
   ;; color-theme-pierson
   color-theme-robin-hood		; the dark green one
   color-theme-high-contrast
   ;; color-theme-emacs-nw
   ;; color-theme-scintilla
   ]
  "Colour themes I prefer.")

(defvar jcgs/org-no-task-color-themes
  [;; color-theme-euphoria ; doesn't change correctly from some
   ;; color-theme-calm-forest
   color-theme-blue-mood
   ;; color-theme-billw
   color-theme-jonadabian		; dark blue, I think it may leave traces afterwards
   ;; color-theme-lethe
   color-theme-kingsajz
   color-theme-retro-orange
   color-theme-retro-green
   color-theme-resolve
   ]
  "Colour themes I can endure but don't like much.")

(add-hook 'org-clock-in-hook
	  (function
	   (lambda ()
	     (condition-case problem
		 (let ((theme (aref jcgs/org-task-color-themes
				    (random (length jcgs/org-task-color-themes)))))
		   (message "Using %s as clocked-in theme" theme)
		   (funcall theme))
	       (message "Got error %S in changing colour theme" problem)))))

(add-hook 'org-clock-out-hook
	  (function
	   (lambda ()
	     (condition-case problem
		 (let ((theme (aref jcgs/org-no-task-color-themes
				    (random (length jcgs/org-no-task-color-themes)))))
		   (message "Using %s as clocked-out theme" theme)
		   (funcall theme))
	       (message "Got error %S in changing colour theme" problem)))))

;;;; an older attempt at this: sort out how it works, and merge anything useful from it

(defvar jcgs/org-dull-theme 'tango
  "The theme to use for making things look dull.
This is for use when not clocked into any task, to remind or
encourage me to clock in as much as possible.")

(defvar jcgs/org-nice-theme nil
  "The theme to use for making things look dull.
This is for use when not clocked into any task, to remind or
encourage me to clock in as much as possible.
If nil, it just reverts to the default appearance.")

(defun jcgs/org-dull-appearance ()
  "Make Emacs look dull.
This is for use when not clocked into any task, to remind or
encourage me to clock in as much as possible."
  (if (memq jcgs/org-dull-theme custom-enabled-themes)
      (enable-theme jcgs/org-dull-theme)
    (when (and jcgs/org-nice-theme
	       (memq jcgs/org-nice-theme custom-enabled-themes))
      (disable-theme jcgs/org-nice-theme))
    (load-theme jcgs/org-dull-theme)))

(defun jcgs/org-nice-appearance ()
  "Make Emacs look nice.
This is for use when clocked into a task, to remind or encourage
me to clock in as much as possible."
  (when (memq jcgs/org-dull-theme custom-enabled-themes)
    (disable-theme jcgs/org-dull-theme))
  (when jcgs/org-nice-theme
    (if (memq jcgs/org-nice-theme custom-enabled-themes)
	(enable-theme jcgs/org-nice-theme)
      (load-theme jcgs/org-nice-theme))))

(defun jcgs/org-clock-in-or-out ()
  "Clock in (if out) or out (if in)."
  (interactive)
  (if (org-clocking-p)
      (progn
	(org-clock-out)
	;; todo: use org-clock-out-hook instead
	(jcgs/org-dull-appearance))
    (org-clock-in)
    ;; todo: use org-clock-in-hook instead
    (jcgs/org-nice-appearance)))

(define-key org-mode-map [ f5 ] 'jcgs/org-clock-in-or-out)

(provide 'org-mode-task-colours)
