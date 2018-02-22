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

(defvar rad-use-view-mode t
  "Whether `run-and-display' should use `view-mode' for the files it finds.")

(mapcar 'make-variable-buffer-local
        '(rad-command
          rad-files-regexp
          rad-highlight-regexps-alist
          rad-found-buffers
          rad-use-view-mode))

(defun run-and-display (&optional edit-options)
  "Run a shell command and display indirect results; optionally EDIT-OPTIONS."
  (interactive "P")
  (when (or edit-options
            (null rad-command)
            (null rad-files-regexp))
    (setq rad-command (read-from-minibuffer "Command to run: ")
          rad-files-regexp (read-from-minibuffer "Regexp for files to display: ")
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
  (when (y-or-n-p "Kill buffers from previous run? ")
    (mapcar 'kill-buffer rad-found-buffers))
  (setq rad-found-buffers nil)
  (let ((output-buffer (get-buffer-create
                        (format "*RAD output for %s*" (buffer-name))))
        ;; put these in locals as we're about to change buffer and
        ;; they are buffer-local
        (command rad-command)
        (files-regexp rad-files-regexp)
        (highlight-regexps-alist rad-highlight-regexps-alist))
    (set-buffer output-buffer)
    (erase-buffer)
    (shell-command command output-buffer)
    (pop-to-buffer output-buffer)
    (shrink-window-if-larger-than-buffer (get-buffer-window output-buffer))
    (goto-char (point-min))
    (let ((files-to-find nil))
      ;; get all the filenames from this buffer before finding any of
      ;; them, so we stay in the same buffer while looking for
      ;; filenames
      (while (re-search-forward files-regexp (point-max) t)
        (let ((filename (ffap-file-at-point)))
          (when (and (stringp filename)
                     (file-exists-p filename))
            (push filename files-to-find))))
      (dolist (filename files-to-find)
        (find-file-other-window filename)
        (push (current-buffer) rad-found-buffers)
        (dolist (highlighter highlight-regexps-alist)
          (hi-lock-face-buffer (car highlighter)
                               (cdr highlighter)))
        (when rad-use-view-mode
          (view-mode 1))))
    t))

(provide 'run-and-display)
;;; run-and-display.el ends here
