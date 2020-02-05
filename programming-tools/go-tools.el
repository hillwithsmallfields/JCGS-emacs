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

(defun go-exports-buffer (&optional format-string combining)
  "List the exports from the current buffer.
If interactive, output it to a temporary buffer.
If optional FORMAT-STRING is given, use that for the output.
With optional COMBINING, output to ambient stdout instead of its own buffer."
  (interactive)
  (let ((case-fold-search nil)
        (exports nil)
        (package (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "package\\s-+\\(.+\\)" (point-max) t)
                       (match-string-no-properties 1)
                     "unknown"))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "func\\s-+\\(?:([^)]+)\\)\\s-*\\([A-Z][A-Za-z_]+\\)" (point-max) t)
        (push (concat package "." (match-string-no-properties 1)) exports)))
    (setq exports (nreverse exports))
    (unless format-string
      (setq format-string "%s\n"))
    (if combining
        (princ "outputting to stdout\n")
        (dolist (exp exports)
          (princ (format format-string exp)))
      (when (interactive-p)
        (with-output-to-temp-buffer (format "*Exports from %s*" (buffer-name))
          (dolist (exp exports)
            (princ (format format-string exp))))))
    exports))

(defun go-exports-directory (dir &optional format-string)
  "Output the exports from go files in DIR using FORMAT-STRING."
  (interactive "DList exports from directory: ")
  (let ((outputter (function
                    (lambda (dir format-string)
                      (dolist (file (directory-files dir t "\\.go$"))
                        (save-excursion
                          (find-file file)
                          (princ (format "Exports from %s\n" file))
                          (go-exports-buffer (current-buffer) format-string)))))))
    (if (interactive-p)
        (with-output-to-temp-buffer (format "*Exports from %s*" dir)
          (funcall outputter dir format-string))
      (funcall outputter dir format-string))))

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
