;;;; config-ratpoison.el
;;; Time-stamp: <2012-09-05 15:28:00 johnstu>

(add-to-list 'load-path (substitute-in-file-name "$GATHERED/source/x/ratpoison/ratpoison-1.4.2/contrib/") t)

(require 'ratpoison)
(require 'ratpoison-cmd)

(defun ratpoison-frames-list (&optional screen-number)
  "Return the list of frames managed by ratpoison.
If optional SCREEN-NUMBER is given, do it for that screen instead of the current one."
  (split-string
   (car
    (split-string
     (shell-command-to-string
      (if screen-number
	  (format "ratpoison -c \"fdump %d\"" screen-number)
	"ratpoison -c fdump"))
     "\n"))
   ","))

(defun ratpoison-screens-list (&optional detailed)
  "Return the list of ratpoison-managed screens.
With optional DETAILED non-nil, make each entry a cons of the screen description
and a list of the descriptions of its frames."
  (let ((basic (split-string
		(car
		 (split-string
		  (shell-command-to-string "ratpoison -c sdump") "\n")) ",")))
    (if detailed
	(let ((i 0)
	      (result nil))
	  (dolist (scr basic)
	    (push (cons scr (ratpoison-frames-list i)) result)
	    (setq i (1+ i)))
	  (nreverse result))
      basic)))

(defvar other-window-frame-or-screen-starting-window nil
  "Which window a sequence of other-window-frame-or-screen commands started on,
or switched frames.")

(defun other-window-frame-or-screen (&optional count)
  "Select another window, or frame, or screen.
COUNT can be passed in to make it negative."
  (interactive)
  (if (eq this-command last-command)
      (progn
	(other-window count)
	(when (eq (selected-window other-window-frame-or-screen-starting-window))
	  (ratpoison-focus)
	  (when (eq (window-configuration-frame (current-window-configuration))
		    other-window-frame-or-screen-starting-frame)
	    ;; todo: possibly move to another screen
	    )))
    (setq other-window-frame-or-screen-starting-window (selected-window)
	  other-window-frame-or-screen-starting-frame (window-configuration-frame
						       (current-window-configuration)))
    (other-window count)))

;;; config-ratpoison.el ends here
