;;; org-mouse-extras.el --- Extra mousable stuff for org-mode, aimed at the n900

;; Copyright (C) 2011  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience, outlines

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

(require 'org-mouse)

(defvar org-mouse-features nil)

(add-to-list 'org-mouse-features 'activate-status)

(defvar org-mouse-status-map
  (let ((map (make-sparse-keymap "Org mouse status")))
    (define-key map [mouse-1] 'org-todo)
    map))

(defun jcgs-org-mouse-stuff ()
  "Add mousable status to `org-mode'."
  ;; notionally belongs in the org-agenda-mode-hook setting in org-mouse.el
  (when (memq 'activate-status org-mouse-features)
       (font-lock-add-keywords
	nil
	`((,org-todo-regexp
	   0 `(face org-link mouse-face highlight keymap ,org-mouse-status-map)
	   'prepend))
	t)))

(provide 'org-mouse-extras)
;;; org-mouse-extras.el ends here
