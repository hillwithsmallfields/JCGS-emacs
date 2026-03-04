;;; dashboard-emacs-query.el --- batch driver for dashboard queries  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2023, 2024, 2026  John Sturdy

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

(load-file  (substitute-in-file-name "$MY_PROJECTS/JCGS-emacs/basics/add-lispdir.el"))

(dolist (repo '("$MY_PROJECTS/JCGS-org-mode/lisp"
                "$OPEN_PROJECTS/github.com/alphapapa/org-ql"
                "$OPEN_PROJECTS/github.com/alphapapa/ts.el"
                "$OPEN_PROJECTS/github.com/alphapapa/org-super-agenda"
                "$OPEN_PROJECTS/github.com/magnars/dash.el"
                "$OPEN_PROJECTS/github.com/magnars/s.el"
                "$OPEN_PROJECTS/github.com/emacsmirror/peg"
                "$OPEN_PROJECTS/github.com/Wilfred/ht.el"
                "$OPEN_PROJECTS/github.com/emacsorphanage/ov"
                "$OPEN_PROJECTS/github.com/magit/transient/lisp"
                "$OPEN_PROJECTS/github.com/emacs-compat/compat"
                "$OPEN_PROJECTS/github.com/rejeep/f.el"
                ;; "$OPEN_PROJECTS/github.com/"
                ;; "$OPEN_PROJECTS/github.com/"
                ;; "$OPEN_PROJECTS/github.com/"
                
                ))
        (add-to-list 'load-path (substitute-in-file-name repo)))

(require 'transient)
(require 'org-ql)
(require 'org-ql-view)
(require 'org-ql-search)

(require 'org-parcels)
(require 'org-ql-to-json)

(load-file (substitute-in-file-name "$MY_ELISP/config/config-org-mode.el"))

(jcgs/org-ql-views-to-json (substitute-in-file-name "$SYNCED/var/views.json")
                           '("Current"
                             "Supermarket"
                             "Mackays"
                             "Online"
                             "Today"
                             "Imminent"
                             "Weekend"
                             "Marmalade"
                             "Physical making"
                             "Programming"
                             "Mending"))

(jcgs/org-expected-dates-to-json-file (substitute-in-file-name "$SYNCED/var/parcels-expected.json"))

(provide 'dashboard-emacs-query)
;;; dashboard-emacs-query.el ends here
