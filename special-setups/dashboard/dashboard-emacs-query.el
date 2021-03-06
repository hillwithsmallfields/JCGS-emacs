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
(dolist (dir '("~/emacs-packages/org-ql-20191105.2141/"
               "~/emacs-packages/dash-20191109.1327/"
               "~/emacs-packages/dash-functional-20191109.1327/"
               "~/emacs-packages/ts-20191010.210/"
               "~/emacs-packages/s-20180406.808/"
               "~/emacs-packages/peg-20150708.641"
               "~/emacs-packages/org-super-agenda-20190925.958"
               "~/emacs-packages/ht-20190924.704/"
               "~/emacs-packages/ov-20150312.528/"))
  (add-to-list 'load-path (expand-file-name dir)))
(message "loading org-ql")
;; (load-library "org-ql")
(require 'org-ql)
(require 'org-ql-view)
(message "loaded org-ql")
(setq package-user-dir (substitute-in-file-name "$EHOME/emacs-packages"))
(load-file (substitute-in-file-name "$MY_ELISP/information-management/metoffice.el"))
(load-file (substitute-in-file-name "$MY_PROJECTS/JCGS-org-mode/lisp/org-ql-to-json.el"))
(message "(boundp 'org-ql-views) = %s" (boundp 'org-ql-views))
(load-file (substitute-in-file-name "$MY_ELISP/config/config-org-mode.el"))

(jcgs/org-ql-views-to-json (substitute-in-file-name "$COMMON/var/views.json")
                           '("Current"
                             "Supermarket"
                             "Physical making"
                             "Programming"
                             "Mending"))

(provide 'dashboard-emacs-query)
;;; dashboard-emacs-query.el ends here
