;;; go-tools.el --- my golang-specific things        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

;; Author: John Sturdy <jsturdy@ccsl.com>
;; Keywords: languages, convenience

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

;; 

;;; Code:

(defun go-exports-buffer ()
  "List the exports from the current buffer."
  (interactive)
  (let ((exports nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "func\\s-+\\(?:([^)]+)\\)\\s-*\\([A-Z][A-Za-z_]+\\)" (point-max) t)
        (push (match-string-no-properties 1) exports)))
    (setq exports (nreverse exports))
    (when (interactive-p)
      (with-output-to-temp-buffer (format "*Exports from %s*" (buffer-name))
        (dolist (exp exports)
          (princ (format "%s\n" exp)))))
    exports))

(defun go-callers-in-repo ()
  "List the files that use functions exported from this buffer."
  (interactive)
  (grep (format "grep -r -f %s -F %s"
                (make-temp-file "go-exports-" nil nil
                                (mapconcat 'identity
                                           (go-exports-buffer)
                                           "\n"))
                (locate-dominating-file "." ".git"))))

(provide 'go-tools)
;;; go-tools.el ends here
