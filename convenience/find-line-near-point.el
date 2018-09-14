;;; find-line-near-point.el --- find file and line mentioned before point  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by find-file-at-point, but getting the line number as
;; well, and looking backwards from point, to get the most recently
;; mentioned file and line.  Written originally for working with gdb
;; in subshells.

;;; Code:

(defun file-and-line-before-point (&optional where forward)
  "Return the file, line, and optionally column, described before WHERE.
With optional argument FORWARD, look after WHERE instead of before it."
  (save-excursion
    (goto-char (or where (point)))
    ;; ffap-file-at-point doesn't do what I hoped it would
    (if (funcall (if forward
                     're-search-forward
                   're-search-backward) "[^-_.a-z0-9/:]\\([-_.a-z0-9/]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?"
                   (if forward (point-max) (point-min))
                   t)
        (list (match-string-no-properties 1)
              (string-to-number(match-string 2))
              (if (match-string 3)
                  (string-to-number (match-string 4))
                nil)))))

(defun locate-file-in-tree (file &optional tree)
  "Locate FILE within TREE."
  (unless tree (setq tree default-directory))
  (let ((here (expand-file-name file tree)))
    (if (file-exists-p here)
        here
      (catch 'found
        (dolist (dirent (directory-files tree nil "^[^.]" t))
          (let ((exp (expand-file-name dirent tree)))
            (if (file-directory-p exp)
                (let ((found-under (locate-file-in-tree file exp)))
                  (when found-under (throw 'found found-under))))))
        nil))))

(defun find-place (&optional where forward)
  "Find a buffer visiting the file before WHERE, at line and column.
With optional argument FORWARD, look after WHERE instead of before it.
Looks in the buffers you are visiting, as well trying the
filename as such, as the filename may be relative to the wrong
directory."
  (interactive "dP")
  (let* ((place (file-and-line-before-point where forward))
         (raw-name (first place))
         (name (if (= (aref raw-name 0) ?/)
                            (substring raw-name 1)
                          raw-name))
         (line (second place))
         (column (third place))
         (buffer (or (find-buffer-visiting name)
                     (and (file-exists-p name) (find-file-noselect name))
                     (get-buffer (file-name-nondirectory name))
                     (locate-file-in-tree (file-name-nondirectory name)))))
    (if (null buffer)
        (error "Could not find %s" name)
      (switch-to-buffer-other-window buffer)
      (goto-line line)
      (when column
        (move-to-column column)))))

(defun find-place-forward (&optional where)
  "Find a buffer visiting the file after WHERE, at line and column."
  (interactive "d")
  (find-place where t))

(provide 'find-line-near-point)
;;; find-line-near-point.el ends here
