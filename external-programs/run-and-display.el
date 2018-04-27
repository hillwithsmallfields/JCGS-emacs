;;; run-and-display.el --- Run a command and show files it produces  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: processes, files, convenience

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

;; Lets you define a buffer-local shell command to run, displaying a
;; file named in the output and doing some highlighting on it.

;;; Code:

(defvar rad-command nil
  "The command for `run-and-display' to use in this buffer.")

(defvar rad-files-regexp nil
  "Regexp describing the files for `run-and-display' to display.")

(defvar rad-highlight-regexps-alist nil
  "Alist of regexps and faces for `run-and-display' to highlight.")

(defvar rad-found-buffers nil
  "List of buffers containing files found by `run-and-display'.")

(defvar rad-start-from-one-window t
  "Whether `run-and-display' should delete other windows before running.
If you don't do this, it may end up producing excessive windows cumulatively.")

(defvar rad-use-view-mode t
  "Whether `run-and-display' should use `view-mode' for the files it finds.")

(defvar rad-kill-previous-buffers 'ask
  "Whether to kill previous buffers when doing a new run.
Can be nil, t or `ask'.")

(mapcar 'make-variable-buffer-local
        '(rad-command
          rad-files-regexp
          rad-highlight-regexps-alist
          rad-found-buffers
          rad-start-from-one-window
          rad-use-view-mode
          rad-kill-previous-buffers))

(defun rad-preserve-lastest-buffers ()
  "Prevent `run-and-display' from killing the buffers it made last time.
Meant for use when `rad-kill-previous-buffers' is t."
  (interactive)
  (setq rad-found-buffers nil))

(defun run-and-display (&optional edit-options)
  "Run a shell command and display indirect results; optionally EDIT-OPTIONS.
The standard output of the command is shown, and is searched for
filenames matching `rad-files-regexp'.  Those files are visited
and the buffers are displayed, in view mode if
`rad-use-view-mode' is set, with highlighting from
`rad-highlight-regexps-alist'.  These settings are per-buffer,
and this command prompts to set their values the first time it is
run in a buffer, or when called with a prefix argument."
  (interactive "P")
  (when (or edit-options
            (null rad-command)
            (null rad-files-regexp))
    (setq rad-command (read-from-minibuffer "Command to run: "
                                            rad-command)
          rad-start-from-one-window (y-or-n-p "Start from a single window? ")
          rad-files-regexp (read-from-minibuffer "Regexp for files to display: "
                                                 rad-files-regexp)
          rad-found-buffers nil
          rad-highlight-regexps-alist nil)
    (catch 'done
      (while t
        (let ((raw-regexp (read-regexp "Regexp to highlight (. to finish)" 'regexp-history-last)))
          (if (or (string= raw-regexp "")
                  (string= raw-regexp "."))
              (throw 'done t)
            (push (cons (hi-lock-regexp-okay raw-regexp)
                        (hi-lock-read-face-name))
                  rad-highlight-regexps-alist)))))
    (setq rad-use-view-mode (y-or-n-p "Use view-mode on files? ")))
  (when (and rad-found-buffers
             rad-kill-previous-buffers
             ;; are there any such buffers?
             (reduce (lambda (a b) (or a b)) ; can't use builtin `or' for this
                     (mapcar 'get-buffer rad-found-buffers))
             (or (eq rad-kill-previous-buffers t)
                 (and (eq rad-kill-previous-buffers 'ask)
                      (y-or-n-p (format "Kill buffers from previous run (%s)? "
                                        (mapconcat 'buffer-name rad-found-buffers " "))))))
    (mapcar 'kill-buffer rad-found-buffers))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save buffer %s? " (buffer-name))))
    (save-buffer))
  (when rad-start-from-one-window
    (delete-other-windows))
  (let* ((original-buffer (current-buffer))
         (output-buffer (get-buffer-create
                         (format "*RAD output for %s*" (buffer-name))))
         ;; put these in locals as we're about to change buffer and
         ;; they are buffer-local
         (command rad-command)
         (files-regexp rad-files-regexp)
         (highlight-regexps-alist rad-highlight-regexps-alist)
         (found-buffers  (list output-buffer)))
    (set-buffer output-buffer)
    (erase-buffer)
    (shell-command command output-buffer)
    (pop-to-buffer output-buffer)
    (goto-char (point-min))
    (while (re-search-forward files-regexp (point-max) t)
      (let ((filename (ffap-file-at-point)))
        (when (and (stringp filename)
                   (file-exists-p filename))
          (save-excursion
            (find-file-other-window filename)
            (push (current-buffer) found-buffers)
            (dolist (highlighter highlight-regexps-alist)
              (hi-lock-face-buffer (car highlighter)
                                   (cdr highlighter)))
            (when rad-use-view-mode
              (view-mode 1))))))
    (save-excursion
      (set-buffer original-buffer)
      (setq rad-found-buffers found-buffers))
    (let ((window (get-buffer-window output-buffer)))
      (select-window window)
      (shrink-window-if-larger-than-buffer window))
    (select-window (get-buffer-window original-buffer))
    (message "")))

(provide 'run-and-display)
;;; run-and-display.el ends here
