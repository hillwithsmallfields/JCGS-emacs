;;; config-ergonomics.el --- set up ergonomic improvements  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, extensions

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

(let ((god-mode-dir (substitute-in-file-name "$OPEN_PROJECTS/github.com/emacsorphanage/god-mode")))
  (when (file-directory-p god-mode-dir)
    (add-to-list god-mode-dir 'load-path)
    (autoload 'god-mode "god-mode"
      "Toggle global `god-local-mode'.")
    (autoload god-mode-all "god-mode"
      "Toggle `god-local-mode' in all buffers.")))

(provide 'config-ergonomics)
;;; config-ergonomics.el ends here
