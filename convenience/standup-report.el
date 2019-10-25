;;; standup-report.el --- Prepare standup reports without having to edit in slack  -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
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

;; 

;;; Code:

(require 'org-jcgs-journal)

;;;; Copying to slack

(defvar standup-report-line-pattern "^\\*\\([-A-Za-z _0-9]+\\)\\*: \\(.*\\)"
  "Regexp describing a standup report line.")

(defun standup-report-copy-line ()
  "Copy this line for pasting into slack.
Then move onto the next line."
  (interactive)
  (unless (save-excursion
            (re-search-forward standup-report-line-pattern (point-max) t))
    (goto-char (point-min)))
  (beginning-of-line 1)
  (when (re-search-forward standup-report-line-pattern (point-max) t)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (beginning-of-line 2)
    (while (and (not (eobp))
                (looking-at "^$"))
      (beginning-of-line 2))))

;;;; Add text

(defun standup-report-add-to-yesterday (begin end)
  "Add the region between BEGIN and END to the report for yesterday."
  (interactive "r")
  (let ((text (buffer-substring begin end)))
    (set-buffer (get-buffer "*Report*"))
    (save-excursion
      (goto-char (point-min))
      (end-of-line 1)
      (let ((dest-start (point)))
        (insert text)
        ;; Regularize whitespace (including collapsing newlines and margins) in the added text
        (let ((dest-end (point-marker)))
          (goto-char dest-start)
          (while (re-search-forward "\\s-+" dest-end t)
            (replace-match " "))
          (goto-char dest-start)
          (while (search-forward ". " dest-end t)
            (replace-match ".  ")))
        (goto-char dest-start)
        (just-one-space)))))

;;;; Buffer setup

(defun standup-report-init-buffer ()
  "Prepare a fresh standup report buffer."
  (interactive)
  (erase-buffer)
  (dolist (heading (cons (if (= (nth 6 (decode-time)) 1)
                             "Friday"
                           "Yesterday")
                         '("Today" "Blockers")))
    (insert "*" heading "*: \n\n"))
  (goto-char (point-min))
  (end-of-line))

;;;; Mode

(defvar standup-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" 'standup-report-copy-line)
    (define-key map "\C-c\C-n" 'standup-report-init-buffer)
    map))

(define-derived-mode standup-report-mode fundamental-mode "Standup report"
  "Major mode for writing standup reports.")

(defun standup-report ()
  "Make a report skeleton for reporting in slack."
  (interactive)
  ;; todo: start using emacs-slack, and define C-c C-c to post the report
  (switch-to-buffer (get-buffer-create "*Report*"))
  (standup-report-mode)
  (standup-report-init-buffer))

(defun daily-start ()
  "Do some of my daily actions.
For my work environment for now."
  (interactive)
  (delete-other-windows)
  (jcgs/org-journal-open-journal "shell-commands")
  (jcgs/org-journal-last-day nil)
  (jcgs/org-journal-open-journal "work")
  (jcgs/org-journal-last-day t)
  (jcgs/org-journal-open-date)
  (basic-save-buffer)
  (split-window-right)
  (other-window 1)
  (standup-report))

(provide 'standup-report)
;;; standup-report.el ends here
