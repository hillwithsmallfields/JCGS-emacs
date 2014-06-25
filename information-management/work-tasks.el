;;; work-tasks.el --- org mode variant for handling work tasks

;; Copyright (C) 2014  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: outlines, convenience

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

;; Initially, this is just to make work-related tasks get archived in
;; a work directory rather than a general one.

;;; Code:

(define-derived-mode work-tasks-mode org-mode
  "Work tasks"
  "Major mode for handling my work tasks."
  (make-local-variable 'org-archive-location)
  (setq org-archive-location "~/work-org/archive/%s::"))

(add-to-list 'auto-mode-alist
	     (cons "work-org/work-tasks.org" 'work-tasks-mode))

(provide 'work-tasks)
;;; work-tasks.el ends here
