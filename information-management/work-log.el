;;; work-log.el --- keep track of things I've done
;; Based on my earlier tracked-compile.el

;; Copyright (C) 2011, 2012, 2013, 2014  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience

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

;; Experimental package for doing code experiments, in the XenClient (BB/OE) environment

;; TODO: re-write to use org-datetree

;;; Code:

(defvar work-log-file (expand-file-name "~/Dropbox/notes/hackery.org-log")
  "The file into which you log your work.
You could set this per-buffer for local logs.")

;;;;;;;;;;;;;;;;;;;;;;;
;; Date-based filing ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun work-log-open-date (date)
  "Ensure there is an open work-log record for DATE."
  (interactive
   (list
    (read-from-minibuffer "Date (YYYY_MM_DD): "
			  (format-time-string "%Y_%m_%d"))))
  (find-file work-log-file)
  ;; we must be in something based on org-mode for some org-mode
  ;; functions we use to work; we mustn't call the mode setup
  ;; function each time, because it kills all local variables
  (unless (eq major-mode 'work-log-mode)
    (work-log-mode))
  (jcgs/org-open-hierarchical-date date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logged shell commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar work-log-recent-shell-commands nil
  "Recent shell commands.
Used while selecting a shell command to log.")

(defun work-log-read-recent-shell-command (prompt)
  "Select a recent shell command from the history.
Argument PROMPT is passed to `read-from-minibuffer'."
  (setq work-log-recent-shell-commands (ring-elements comint-input-ring))
  (when nil
    (mapcar (function
	     (lambda (str)
	       (set-text-properties 0 (length str) nil str)
	       str))
	    work-log-recent-shell-commands)
    (message "recent-commands are now %S" work-log-recent-shell-commands))
  (read-from-minibuffer prompt
			(car work-log-recent-shell-commands)
			nil		; keymap
			nil		; read
			'work-log-recent-shell-commands
			))

(defun work-log-recent-shell-command (command)
  "Record a recent shell COMMAND in your work log.
For use from the comint (shell) buffer."
  (interactive
   (list
    (work-log-read-recent-shell-command "Record shell command: ")))
  (save-window-excursion
    (save-excursion
      (tracking-open-date (tracking-format-current-date))
      (goto-char (point-max))
      (insert "\n        $ " command "\n\n"))))

(require 'shell)			; for shell-mode-map
(define-key shell-mode-map (kbd "C-<return>") 'work-log-recent-shell-command)

;;;;;;;;;;;;;;;;
;; Major mode ;;
;;;;;;;;;;;;;;;;

(define-derived-mode work-log-mode org-mode
  "Work log"
  "Major mode for making notes on what I've done while developing software.
Organizes the log hierarchically by date (day, month, year)."
  (make-local-variable 'org-archive-location)
  (setq org-archive-location "~/work-org/archive/%s::"))

(define-key work-log-mode-map "\C-c\C-d" 'work-log-open-date)

(add-to-list 'auto-mode-alist (cons "work.org-log" 'work-log-mode))

(provide 'work-log)
;;; work-log.el ends here
