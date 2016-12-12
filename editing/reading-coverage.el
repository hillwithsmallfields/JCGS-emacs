;;;; reading-coverage.el -- keep track of what source you have looked at
;;; Time-stamp: <2006-11-11 10:01:15 jcgs>

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

(defvar seen-functions-files nil
  "Alist of source files the user has looked at, to alist of which functions in them they have read.")

(defvar newly-seen-file-hook nil
  "Functions to run on finding a file that we haven't seen the user looking at before.
The functions are run until one returns non-nil; in which case it should return a list
of the functions defined in that file, each consed with nil.")

(defun I-have-seen-function (function file)
  "Note that you have seen FUNCTION in FILE."
  (interactive "sFunction: 
fFile containing %s: ")
  (setq function (intern function))
  (let ((file-pair (assoc file seen-functions-files)))
    (unless file-pair
      (setq file-pair (cons file
                            (run-hook-with-args-until-success
                             'newly-seen-file-hook
                             file))
            seen-functions-files (cons file-pair
                                       seen-functions-files)))
    (let ((function-pair (assoc function (cdr file-pair))))
      (if function-pair
          (rplacd function-pair (current-time-string))
        (setq function-pair (cons function (current-time-string)))
        (rplacd file-pair (cons function-pair (cdr file-pair)))))))

(defvar latest-defun-start nil
  "The latest defun start position noted.")

(make-variable-buffer-local 'latest-defun-start)

(defun record-function-seen ()
  "Record that we have seen the current function."
  (condition-case evar
      (when (and buffer-file-name
                 (maybe-in-different-defun))
        (save-excursion
          (forward-char 1)
	  (beginning-of-defun 1)
	  (let* ((function-name (get-function-name))
		 (start (point)))
	    (when function-name
	      (I-have-seen-function function-name
				    (file-name-nondirectory buffer-file-name)))
	    (end-of-defun)
	    (coverage-mark-seen start (point)))))
    (error "Error in record-function-seen: %S" evar)))

(defun newly-seen-source-file-function (file)
  "Function to run when adding a source file to the reading coverage list.
Should return a list of the functions in the file."
  (cond
   ((string-match "\\.el$" file)
    (save-excursion
      (goto-char (point-min))
      (let ((functions nil))
        (while (re-search-forward "^(def[-a-z0-9]+\\s +\\(\\S +\\)"
                                  (point-max) t)
          (setq functions
                (cons (cons (intern (match-string-no-properties 1))
                            nil)
                      functions)))
        functions)))
   ((string-match "\\.c$" file)
    (save-excursion
      (goto-char (point-max))
      (let ((functions nil))
        (while (not (bobp))
          (beginning-of-defun)
          (backward-sexp 2)
          (when (looking-at "[a-z][a-z0-9_]*")
	    (setq functions
		  (cons (cons (intern (match-string-no-properties 0))
			      nil)
			functions))))
        functions)))))

(add-hook 'newly-seen-file-hook 'newly-seen-source-file-function)

(defvar coverage-unseen-scale-factor .75
  "*How much to shrink code you've not yet looked at.")

(defun coverage-mark-unseen (from to)
  "Mark that we have not seen the text between FROM and TO."
  (interactive "r")
  (let ((modified (buffer-modified-p))
	(buffer-read-only nil))
    (put-text-property from to 'display (list 'height coverage-unseen-scale-factor))
    ;;; (put-text-property from to 'face (cons ':background "grey"))
    (set-buffer-modified-p modified)))


(defun coverage-mark-seen (from to)
  "Mark that we have seen the text between FROM and TO."
  (interactive "r")
  (let ((modified (buffer-modified-p))
	(buffer-read-only nil))
    (put-text-property from to 'display (list 'height nil))
    (set-buffer-modified-p modified)))

(defun start-coverage ()
  "Start recording coverage in the current buffer.
Put this function on your mode hook to use it automatically.
Currently implemented for Lisp and C families."
  (interactive)
  (add-hook 'post-command-hook 'record-function-seen)
  (coverage-mark-unseen (point-min) (point-max))
  (when buffer-file-name
    (let ((functions (cdr (assoc (file-name-nondirectory buffer-file-name)
				 seen-functions-files)))
	  (defun-end (point-max)))
      (save-excursion
	(goto-char (point-max))
	(while (not (bobp))
	  (beginning-of-defun)
	  (when (cdr (assoc (get-function-name) functions))
	    (coverage-mark-seen (point) defun-end))
	  (setq defun-end (point)))))))

(defun get-function-name ()
  "Get the name of the function point is in. Assumes beginning-of-defun has just been done."
  (save-excursion
    (cond 
     ((memq major-mode '(emacs-lisp-mode lisp-mode))
      (if (looking-at "^(def[uma][-a-z0-9]+\\s +\\(\\S +\\)")
          (match-string-no-properties 1)
        nil))
     ((memq major-mode '(c-mode c++-mode java-mode))
      (save-excursion
        (if (re-search-backward "\\([a-z][a-z0-9_]*\\)(" (point-min) t)
            (match-string-no-properties 1)
          nil))))))

(defun maybe-in-different-defun ()
  "Return whether we are in a different defun from the one we were in last time this function was called."
  (save-excursion
    ;; (goto-char (min (1+ (point)) (point-max)))
    (beginning-of-defun 1)
    (prog1
        (not (eq (point) latest-defun-start))
      (setq latest-defun-start (point)))))

(defvar seen-functions-file-name "~/.seen-functions"
  "The file in which to save the seen functions list.")

(defun save-seen-functions ()
  "Save the list of functions you have seen.
This goes in seen-functions-file-name."
  (interactive)
  (find-file seen-functions-file-name)
  (erase-buffer)
  (insert ";;; saved at " (current-time-string) " by " user-mail-address "\n\n")
  (insert "(setq seen-functions-files\n '(")
  (mapcar #'(lambda (file-pair)
              (insert "(\"" (car file-pair) "\"\n(")
              (mapcar #'(lambda (function-pair)
                          (let* ((namer (car function-pair))
                                 (name (cond
                                        ((and (consp namer) (symbolp (car namer))) (symbol-name (car namer)))
                                        ((symbolp namer) (symbol-name namer)))))
                            (if name
                              (if (cdr function-pair)
                                  (insert "(" name " . t) ")
                                (insert "(" name ") "))
                              (insert (format ";; null name in %S\n" function-pair)))))
                      (cdr file-pair))
              (insert "))\n"))
          seen-functions-files)
  (insert "))\n;;; end of reading coverage list\n ")
  (basic-save-buffer))


;; `:stipple'
;;      The background stipple, a bitmap.

;;      The value can be a string; that should be the name of a file
;;      containing external-format X bitmap data.  The file is found in
;;      the directories listed in the variable `x-bitmap-file-path'.

;;      Alternatively, the value can specify the bitmap directly, with a
;;      list of the form `(WIDTH HEIGHT DATA)'.  Here, WIDTH and HEIGHT
;;      specify the size in pixels, and DATA is a string containing the
;;      raw bits of the bitmap, row by row.  Each row occupies (WIDTH + 7)
;;      / 8 consecutie bytes in the string (which should be a unibyte
;;      string for best results).

;;      If the value is `nil', that means use no stipple pattern.

;;      Normally you do not need to set the stipple attribute, because it
;;      is used automatically to handle certain shades of gray.

;; (defun foo ()
;;   (interactive)
;;   (put-text-property (region-beginning) (region-end) 'display (list 'height .6)))


;; (defun bar ()
;;   (interactive)
;;   (put-text-property (region-beginning) (region-end) 'display (list 'height nil)))

(provide 'reading-coverage)

;;; end of reading-coverage.el
