;;;; voice-assist-key.el -- commands to run from a key or pedal, for helping the voice system
;;; Time-stamp: <2006-02-09 11:40:46 jcgs>

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

(provide 'voice-assist-key)

(defun voice-assist-key-command ()
  "Function to be run when the voice assist key or pedal is pressed."
  (interactive)
  ;; todo: interrupt current command (if in interactive-reader) and put text into the buffer 
  (message "voice-assist-key-command")
)

(defvar vr-key-debug-buffer (get-buffer-create "*vr key debug*")
  "Buffer for vr key debugging")

(require 'time-date)

(defvar vr-key-previous-time (time-to-seconds (current-time))
  "When we last logged a vr key.")

(defvar vr-key-previous-key ""
  "The most recent key typed that we are debugging.")

(defvar vr-key-this-key-count 1
  "How many times we have now had the same key that we are debugging.")

(defun vr-key-debug (key)
  "Debugging routine for the keystrokes issued by Correct That"
  (let ((now (time-to-seconds (current-time)))
	(char (char-after (point))))
    (setq vr-key-this-key-count
	  (if (equal key vr-key-previous-key)
	      (1+ vr-key-this-key-count)
	    1))
    (save-window-excursion
      (save-excursion
	(set-buffer vr-key-debug-buffer)
	(goto-char (point-max))
	(if (> (- now vr-key-previous-time) 1.0)
	    (progn
	      (insert "\n\n" (buffer-name) ": " (current-time-string) "\n")
	      (insert "[" (save-excursion
			    (view-lossage)
			    (set-buffer "*Help*")
			    (buffer-string))
		      "]\n")))
	(insert (format "%s(%d), now at %d:%S\n"
			key
			vr-key-this-key-count
			(point)
			(if (char-valid-p char)
			    (char-to-string char)
			  nil)))))
    (setq vr-key-previous-time now
	  vr-key-previous-key key)))

(defun vr-key-backspace ()
  "Handle backspace as used by Correct That from Dragon."
  (interactive)
  (vr-key-debug 'vr-key-backspace)
  ;; todo: fill this in
  (backward-delete-char-untabify 1)
)

(defun vr-key-kp-left ()
  "Handle kp-left as used by Correct That from Dragon."
  (interactive)
  (vr-key-debug 'vr-key-kp-left)
  ;; todo: fill this in
  (go-to-last-typein)
)

(defun vr-key-kp-right ()
  "Handle kp-right as used by Correct That from Dragon."
  (interactive)
  (vr-key-debug 'vr-key-kp-right)
  ;; todo: fill this in
  (forward-char 1)
)

(defvar last-insert-char-marker (make-marker)
  "A marker which points to the last character inserted by self-insert-command.")

(defadvice self-insert-command (after remember-position)
  "Remember where we were after the most recent typed character.
We set the marker last-insert-char-marker.
This is intended for adjusting the incorrect positioning that Dragon
sometimes does for Correct That."
  (message "at %d" (point))
  (set-marker last-insert-char-marker (point)))

;; this turned out not to work as expected; perhaps it is handled specially in the edit loop?
;; (ad-activate 'self-insert-command)

(defun remember-position-after-change (start end old-length)
  "Remember the position, after a change."
  (if (eq last-command 'self-insert-command)
      (set-marker last-insert-char-marker
		  (point)
		  ;; give the buffer explicitly, otherwise *vr* buffer gets in there
		  (window-buffer (selected-window)))))

(add-hook 'after-change-functions 'remember-position-after-change)

(defun go-to-last-typein ()
  "Go to the place remembered by remember-position-after-change."
  (set-buffer (marker-buffer last-insert-char-marker))
  (goto-char last-insert-char-marker))

(defun vr-key-S-kp-right ()
  "Handle S-kp-right as used by Correct That from Dragon."
  (interactive)
  (vr-key-debug 'vr-key-S-kp-right)
  ;; todo: fill this in
  (go-to-last-typein)
)

(defun vr-key-S-kp-left ()
  "Handle S-kp-left as used by Correct That from Dragon."
  (interactive)
  (vr-key-debug 'vr-key-S-kp-left)
  ;; todo: fill this in
  (go-to-last-typein)
)

(defun vr-keys-hook-function ()
  "Set up keys to suit VR."
  (interactive)
  (message "Setting up vr keys")
  (global-set-key [ M-S-kp-home ] 'voice-assist-key-command)

  ;; set up the keys used by "correct that";
  ;; these are the keys concerned:
  ;; (global-set-key [ backspace ] 'vr-key-backspace)
  (message "Setting up vr keys: previously %S"
	   (mapcar (lambda (key) (cons key (key-binding key)))
		   '([ backspace ] [ kp-left ] [ kp-right ] [ S-kp-right ] [ S-kp-left ] )))
  (global-set-key [ kp-left ] 'vr-key-kp-left)
  (global-set-key [ kp-right ] 'vr-key-kp-right)
  (global-set-key [ S-kp-right ] 'vr-key-S-kp-right)
  (global-set-key [ S-kp-left ] 'vr-key-S-kp-left)
  (message "Setting up vr keys: now %S"
	   (mapcar (lambda (key) (cons key (key-binding key)))
		   '([ backspace ] [ kp-left ] [ kp-right ] [ S-kp-right ] [ S-kp-left ] )))  )

(vr-keys-hook-function)
(message "Ran vr-keys-hook-function directly")
(add-hook 'vr-mode-setup-hook 'vr-keys-hook-function)
(message "Put vr-keys-hook-function onto vr-mode-setup-hook")

;;; end of voice-assist-key.el
