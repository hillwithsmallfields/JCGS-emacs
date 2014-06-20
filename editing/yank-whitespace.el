;;;; yank-whitespace.el -- advise kill-ring functions to tag items with what the surrounding whitespace was
;;; Time-stamp: <2005-11-23 15:00:08 jcgs>

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

;; Often, after pasting something, there is some tidying up of
;; whitespace to do, otherwise it just looks annoyingly wrong. This
;; feature puts some advice on killing, copying and yanking commands,
;; to make them have a go at fixing things up in a way that seems likely
;; to be what you wanted. That is, if your tastes are like mine!

(provide 'yank-whitespace)

;; todo: remember whether it starts within a comment
;; todo: advise more functions

(defun after-blank-line (where)
  "Return whether WHERE is at the start of a line, after a blank line."
  (save-excursion
    (goto-char where)
    (beginning-of-line 0)
    (skip-chars-forward " \t\n\r")
    (>= (point) where)))

(defun before-blank-line (where)
  "Return whether WHERE is at the end of a line, followed by a blank line."
  (save-excursion
    (goto-char where)
    (beginning-of-line 3)
    (skip-chars-backward " \t\n\r")
    (<= (point) where)))

(defun at-start-of-line (where)
  "Return whether WHERE is at the start of a line."
  (save-excursion
    (goto-char where)
    (beginning-of-line 1)
    (skip-chars-forward " \t\n\r")
    (>= (point) where)))

(defun after-whitespace (where)
  "Return whether WHERE follows whitespace."
  (if (< where (1+ (point-min)))
      nil				; not as far as we know
    (save-excursion
      (goto-char (1- where))
      (looking-at "[\t ]"))))

(defun after-single-space (where)
  "Return whether WHERE follows a single space."
  (if (< where (+ 2 (point-min)))
      nil				; not as far as we know
    (save-excursion
      (goto-char (- where 2))
      (looking-at "[^\t ] "))))

(defun at-end-of-line (where)
  "Return whether WHERE is at the end of a line (trailing whitespace allowed) ."
  (save-excursion
    (goto-char where)
    (looking-at "[ \t]*$")))

(defun before-whitespace (where)
  "Return whether WHERE is followed by whitespace."
  (save-excursion
    (goto-char where)
    (looking-at "[ \t]")))

(defun before-single-space (where)
  "Return whether WHERE is followed by a single space."
  (save-excursion
    (goto-char where)
    (looking-at " [^ \t]")))

(defvar preceding-whitespace nil
  "Description of the whitespace preceding the region examined.")

(defvar following-whitespace nil
  "Description of the whitespace following the region examined.")

(defun note-adjacent-whitespace (begin end)
  "Note the whitespace surrounding BEGIN and END in the current buffer."
  (let ((preceding (if (at-start-of-line begin)
		       (if (after-blank-line begin)
			   'blank-line
			 'start-of-line)
		     (if (after-whitespace begin)
			 (if (after-single-space begin)
			     'single-space
			   'whitespace)
		       nil)))
	(following (if (at-end-of-line end)
		       (if (before-blank-line end)
			   'blank-line
			 'end-of-line)
		     (if (before-whitespace end)
			 (if (before-single-space end)
			     'single-space
			   'whitespace)
		       nil))))
    (if (and (eq begin end) ; special hack for when we are called to insert at a blank line
	     (eq preceding 'start-of-line)
	     (eq following 'end-of-line))
	(setq preceding-whitespace 'blank-line
	      following-whitespace 'blank-line)
      (setq preceding-whitespace preceding
	    following-whitespace following))))

(defun label-with-adjacent-whitespace (object)
  "Label OBJECT with descriptions of the whitespace surrounding the region given to note-adjacent-whitespace."
  (let ((end (length object)))
    ;; (message "Labelling %S as far as %d with %S %S" object end preceding-whitespace following-whitespace)
    (put-text-property 0 end
		       'preceded-by
		       preceding-whitespace
		       object)
    (put-text-property 0 end
		       'followed-by
		       following-whitespace
		       object)))

(defun label-kill-with-adjacent-whitespace ()
  "Label the top item of the kill ring, with descriptions of the region given to note-adjacent-whitespace."
  (label-with-adjacent-whitespace (current-kill 0 t)))

(defadvice kill-region (before remember-whitespace)
  "Tag the text in the kill ring with what the surrounding whitespace was."
  ;; todo: this should really be around-advice, and not set properties in the buffer!
  (note-adjacent-whitespace (ad-get-arg 0)
			    (ad-get-arg 1)))

(defadvice kill-region (after label-whitespace)
  "Tag the text in the kill ring with what the surrounding whitespace was."
  ;; todo: this should really be around-advice, and not set properties in the buffer!
  (label-kill-with-adjacent-whitespace)
  ;; todo: this should optionally tidy up after the kill, e.g. if there was a single blank line before the deleted text, and a single blank line after it, there should be a single blank line left in the result
  )

(defadvice kill-ring-save (before remember-whitespace)
  "Tag the text in the kill ring with what the surrounding whitespace was."
  ;; todo: this should really be around-advice, and not set properties in the buffer!
  (note-adjacent-whitespace (ad-get-arg 0)
			    (ad-get-arg 1)))

(defadvice kill-ring-save (after label-whitespace)
  "Tag the text in the kill ring with what the surrounding whitespace was."
  ;; todo: this should really be around-advice, and not set properties in the buffer!
  (label-kill-with-adjacent-whitespace))

(ad-activate 'kill-region)
(ad-activate 'kill-ring-save)

(defvar combine-preceding-space nil
  "Hook-style definition of how to combine the whitespace at the beginning of an insertion.
Each function takes two arguments, describing the whitespace:
  that was already in the buffer before the insertion
  that was originally before the inserted text in the place where that text came from.
It should return nil if it does not want to control the whitespace adjustment for that
particular pair of values, or a function to call, with no arguments, if it does.")

(defvar combine-following-space nil
  "Hook-style definition of how to combine the whitespace at the end of an insertion.
Each function takes two arguments, describing the whitespace:
  that was originally after the inserted text in the place where that text came from.
  that was already in the buffer before the insertion
It should return nil if it does not want to control the whitespace adjustment for that
particular pair of values, or a function to call, with no arguments, if it does.")

(defun combine-space-single-spaces (a b)
  "A hook function for combine-preceding-space and combine-following-space.
If both the arguments are single spaces, ensure a single space in the result."
  (if (and (eq a 'single-space)
	   (eq b 'single-space))
      'just-one-space
    nil))

(add-hook 'combine-preceding-space 'combine-space-single-spaces)
(add-hook 'combine-following-space 'combine-space-single-spaces)

(defun just-one-blank-line ()
  "Ensure there is just one blank line at point."
  (interactive)
  (delete-blank-lines)
  (delete-blank-lines)
  (open-line 1))

(defun combine-space-blank-lines (a b)
  "A hook function for combine-preceding-space and combine-following-space.
If both the arguments are blank lines, ensure a blank line in the result."
  (if (and (eq a 'blank-line)
	   (eq b 'blank-line))
      'just-one-blank-line
    nil))

(add-hook 'combine-preceding-space 'combine-space-blank-lines)
(add-hook 'combine-following-space 'combine-space-blank-lines)

(defadvice yank (before remember-whitespace)
  "Remember the whitespace around the point where an insertion is about to happen."
  (note-adjacent-whitespace (point) (point)))

(defadvice yank (after adjust-whitespace)
  (let* ((extant-preceding preceding-whitespace)
	 (extant-following following-whitespace)
	 (incoming-preceding (get-text-property (region-beginning) 'preceded-by))
	 (incoming-following (get-text-property (1- (region-end)) 'followed-by))
	 (preceding-action (run-hook-with-args-until-success 'combine-preceding-space
							     extant-preceding incoming-preceding))
	 (following-action (run-hook-with-args-until-success 'combine-following-space
							     incoming-following extant-following)))
    (message "Whitespace pattern {%S:%S ... %S:%S} leading to actions {%S %S}"
	     extant-preceding incoming-preceding
	     incoming-following extant-following
	     preceding-action following-action)
    ;; The last shall be first, and the first last.
    ;; In this case, it is to avoid disturbing the
    ;; buffer downstream of a change we are about to make.
    (if following-action
	(save-excursion
	  (goto-char (region-end))
	  (funcall following-action)))
    (if preceding-action
	(save-excursion
	  (goto-char (region-beginning))
	  (funcall preceding-action)))))

(ad-activate 'yank)

;;; end of yank-whitespace.el
