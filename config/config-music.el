;;; config-music.el --- anything to do with emacs and music  -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: multimedia

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

;; Set up lilypond, might move emms setup into here too, anything like that

;;; Code:

(autoload 'LilyPond-mode "lilypond-mode"
  "Major mode for editing LilyPond music files.

This mode knows about LilyPond keywords and line comments, not about
indentation or block comments.  It features easy compilation, error
finding and viewing of a LilyPond source buffer or region." t)

(add-to-list 'auto-mode-alist (cons ".ly" 'LilyPond-mode))

(defun jcgs/lilypond-compile ()
  "My wrapper around running Lilypond."
  (interactive)
  (LilyPond-command-lilypond)
  (let* ((lilypond-pdf (concat (file-name-sans-extension (LilyPond-get-master-file))
                      ".pdf")))
    (walk-windows (function
                   (lambda (window)
                     (when (eq (buffer-file-name (window-buffer window))
                               lilypond-pdf)
                       (revert-buffer t t))))))
  (LilyPond-command-current-midi))

(defun jcgs/lilypond-mode-load-function ()
  "My Lilypond extras."
  (define-key LilyPond-mode-map "\C-c\C-a" 'jcgs/lilypond-compile))

(add-to-list 'after-load-alist '(lilypond-mode jcgs/lilypond-mode-load-function))

(provide 'config-music)
;;; config-music.el ends here
