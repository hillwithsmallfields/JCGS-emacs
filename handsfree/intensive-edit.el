;;;; intensive-edit.el
;;; Time-stamp: <2006-03-09 15:00:33 john>
;;; Provides structure for doing a narrow range of things intensively via pedals
;;; This should typically be useful for editing software.

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

;; (require 'versor)
;; (provide 'intensive-edit)

;; (defvar intensive-editing nil
;;   "*Whether we are doing intense editing")

;; (defvar ending-intensive-editing nil
;;   "*Whether we are ending intense editing")

;; (defvar intensive-edit-commands
;;   '(
;;     ("End intensive editing" . intensive-edit-end)
;;     ("Set versor meta-dimension" . versor-select-named-meta-level)
;;     ("Set versor dimension" . versor-select-named-level)
;;     )
;;   "The commands available in the intensive edit loop.")

;; (defun intensive-edit-menu ()
;;   "Perform the intense edits"
;;   (interactive)
;;   (if (or intensive-editing
;; 	  (interactive-p))
;;       (progn
;; 	(catch 'done-intensive-edit
;; 	  (while t
;; 	    (let* ((command-name (choose-using-history 
;; 				  "Command: "
;; 				  (mapcar 'car intensive-edit-commands)
;; 				  "Choose intensive-edit-end to get the normal menus back")
;; 				 )
;; 		   (command (cdr (assoc command-name intensive-edit-commands)))
;; 		   )
;; 	      (let ((intensive-editing nil))
;; 		(call-interactively command)))))
;;     	(if ending-intensive-editing
;; 	    (progn
;; 	      (setq intensive-editing nil
;; 		    ending-intensive-editing nil)
;; 	      (message "Ended intensive editing")))
;; 	t)
;;     nil))

;; (defvar intensive-actions-map
;;   (make-sparse-keymap "Intensive actions")
;;   "Keymap for intensive actions, normally reached by the intensive-action-menu command.")

;; (defun intensive-edit-action ()
;;   "Perform the intense edit actions"
;;   (interactive)
;;   (if (or intensive-editing
;; 	  (interactive-p))
;;       (catch 'done-intensive-edit
;; 	(while t
;; 	  (define-key intensive-actions-map pedal-M-onward
;; 	    (if in-wander-yank
;; 		'move-sexp-from-point
;; 	      'wander-yank))
;; 	  (let* ((key (save-window-excursion
;; 			(pedals-draw-bindings intensive-actions-map)
;; 			(message "Intensive editing inner loop command: ")
;; 			(read-event)))
;; 		 (fn (lookup-key intensive-actions-map (vector key))))
;; 	    (message "Got key %S --> command %S" key fn)
;; 	    (if (commandp fn)
;; 		(call-interactively fn)
;; 	      (message "Not defined as a command"))))
;; 	t)
;;     nil))

;; (require 'handsfree)

;; (add-hook 'handsfree-main-menu-hook 'intensive-edit-menu)
;; (add-hook 'aux-pedal-hook 'intensive-edit-action)

;; (define-key intensive-actions-map pedal-onward 'statement-navigate-other)
;; (define-key intensive-actions-map pedal-aux 'toggle-following/surrounding)
;; (define-key intensive-actions-map pedal-C-aux 'create-or-surround)
;; (define-key intensive-actions-map pedal-M-aux 'statement-type)
;; (define-key intensive-actions-map pedal-C-menu 'intensive-edit-exit)
;; (define-key intensive-actions-map pedal-M-menu 'find-tag-other-window)
;; (define-key intensive-actions-map pedal-C-onward 'statement-navigate-next-head)
;; (define-key intensive-actions-map pedal-M-onward 'wander-yank)

;; (defun intensive-edit-begin ()
;;   "Turns on intensive editing"
;;   (interactive)
;;   (message "Beginning intensive editing")
;;   (setq intensive-editing t))

;; (defun intensive-edit-exit ()
;;   "Exits intensive editing inner loop"
;;   (interactive)
;;   (message "Ending intensive editing inner loop")
;;   (throw 'done-intensive-edit t))

;; (defun intensive-edit-end ()
;;   "Turns off intensive editing"
;;   (interactive)
;;   (message "Ending intensive editing")
;;   (setq ending-intensive-editing t)
;;   (throw 'done-intensive-edit t))
