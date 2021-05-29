;;; dashboard-emacs-query.el --- batch driver for dashboard queries  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: 

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

(require 'cl)

(add-to-list 'load-path (substitute-in-file-name "$MY_PROJECTS//github.com/hillwithsmallfields/JCGS-org-mode/lisp/"))

(load-file (substitute-in-file-name "$MY_ELISP/information-management/metoffice.el"))
(load-file (substitute-in-file-name "$MY_ELISP/config/config-org-mode.el"))
(load-file (substitute-in-file-name "$MY_PROJECTS/JCGS-org-mode/lisp/org-ql-to-json.el"))

(jcgs/org-ql-views-to-json (substitute-in-file-name "$COMMON/var/views.json") '("Current"))

(provide 'dashboard-emacs-query)
;;; dashboard-emacs-query.el ends here
