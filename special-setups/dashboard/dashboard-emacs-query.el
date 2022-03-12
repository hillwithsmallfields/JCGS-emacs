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

;; The Emacs part of updating my dashboard
;; (https://github.com/hillwithsmallfields/qs/tree/master/dashboard)

;;; Code:

(require 'cl)

(add-to-list 'load-path (substitute-in-file-name "$MY_PROJECTS//github.com/hillwithsmallfields/JCGS-org-mode/lisp/"))

(require 'org-ql)
(require 'org-ql-view)
(setq package-user-dir (substitute-in-file-name "$EHOME/emacs-packages"))
(load-file (substitute-in-file-name "$MY_ELISP/information-management/metoffice.el"))
(load-file (substitute-in-file-name "$MY_PROJECTS/JCGS-org-mode/lisp/org-parcels.el"))
(load-file (substitute-in-file-name "$MY_PROJECTS/JCGS-org-mode/lisp/org-ql-to-json.el"))
(message "(boundp 'org-ql-views) = %s" (boundp 'org-ql-views))
(load-file (substitute-in-file-name "$MY_ELISP/config/config-org-mode.el"))

(jcgs/org-ql-views-to-json (substitute-in-file-name "$SYNCED/var/views.json")
                           '("Current"
                             "Supermarket"
                             "Online"
                             "Physical making"
                             "Programming"
                             "Mending"))

(jcgs/org-expected-dates-to-json-file (substitute-in-file-name "$SYNCED/var/parcels-expected.json"))

(provide 'dashboard-emacs-query)
;;; dashboard-emacs-query.el ends here
