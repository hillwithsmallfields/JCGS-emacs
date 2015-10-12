;;;; Switch colour themes
;;; Time-stamp: <2015-10-12 11:26:41 johstu01>

;; Copyright (C) 2015  John Sturdy

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

;;; Use nicer colour themes when I'm clocked in to some task, to
;;; encourage me to be clocked in more of the time.

;; (add-lispdir "$GATHERED/emacs/color-theme")

;; (require 'color-theme)

;;; Code:

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
	  'jcgs/org-nice-appearance
	  ;; (function
	  ;;  (lambda ()
	  ;;    (condition-case problem
	  ;; 	 (let ((theme (aref jcgs/org-task-color-themes
	  ;; 			    (random (length jcgs/org-task-color-themes)))))
	  ;; 	   (message "Using %s as clocked-in theme" theme)
	  ;; 	   (funcall theme))
	  ;;      (message "Got error %S in changing colour theme" problem))))
	  )

(add-hook 'org-clock-out-hook
	  'jcgs/org-dull-appearance
	  ;; (function
	  ;;  (lambda ()
	  ;;    (condition-case problem
	  ;; 	 (let ((theme (aref jcgs/org-no-task-color-themes
	  ;; 			    (random (length jcgs/org-no-task-color-themes)))))
	  ;; 	   (message "Using %s as clocked-out theme" theme)
	  ;; 	   (funcall theme))
	  ;;      (message "Got error %S in changing colour theme" problem))))
	  )

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

(provide 'org-mode-task-colours)
