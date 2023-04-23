;;; config-ergonomics.el --- set up ergonomic improvements  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023  John Sturdy

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

(defun jcgs/god-mode-enabled-function ()
  (set-face-attribute 'mode-line nil
                      :foreground "#604000"
                      :background "#fff29a")
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#3f3000"
                      :background "#fff3da"))

(defun jcgs/god-mode-disabled-function ()
  (set-face-attribute 'mode-line nil
		      :foreground "#0a0a0a"
		      :background "#d7d7d7")
  (set-face-attribute 'mode-line-inactive nil
		      :foreground "#404148"
		      :background "#efefef"))

(when nil
  (let ((god-mode-dir (substitute-in-file-name "$OPEN_PROJECTS/github.com/emacsorphanage/god-mode")))
    (when (file-directory-p god-mode-dir)

      (setq god-mode-enable-function-key-translation nil)

      (add-to-list 'load-path god-mode-dir)
      (require 'god-mode)

      (global-set-key (kbd "<escape>") #'god-mode-all)
      (require 'god-mode-isearch)

      (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
      (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)
      (define-key god-local-mode-map (kbd "z") #'repeat)
      (define-key god-local-mode-map (kbd "i") #'god-local-mode)
      (global-set-key (kbd "C-x C-1") #'delete-other-windows)
      (global-set-key (kbd "C-x C-2") #'split-window-below)
      (global-set-key (kbd "C-x C-3") #'split-window-right)
      (global-set-key (kbd "C-x C-0") #'delete-window)
      (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
      (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
      (add-hook 'god-mode-enabled-hook 'jcgs/god-mode-enabled-function)
      (add-hook 'god-mode-disabled-hook 'jcgs/god-mode-disabled-function)
      (let ((c-m (assoc "G" god-mode-alist)))
        (when c-m (rplaca c-m "h"))))))

(provide 'config-ergonomics)
;;; config-ergonomics.el ends here
