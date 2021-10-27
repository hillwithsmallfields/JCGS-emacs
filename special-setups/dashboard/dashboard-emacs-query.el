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
(dolist (lib-spec '(("org-ql" .  "alphapapa/org-ql")
                    ("dash" . "magnars/dash.el")
                    ("dash-functional" . "magnars/dash.el")
                    ("ts" . "alphapapa/ts.el")
                    ("s" . "magnars/s.el")
                    ("peg" . "emacsmirror/peg")
                    ("org-super-agenda" . "alphapapa/org-super-agenda")
                    ("ht" . "Wilfred/ht.el")
                    ("ov" . "emacsorphanage/ov")))
  (unless (locate-file (concat (car lib-spec) ".el"))
    (add-to-list 'load-path (substitute-in-file-name (expand-file-name (cdr lib-spec) "$EHOME/open-projects")))))
(message "loading org-ql")
;; (load-library "org-ql")
(require 'org-ql)
(require 'org-ql-view)
(message "loaded org-ql")
(setq package-user-dir (substitute-in-file-name "$EHOME/emacs-packages"))
(load-file (substitute-in-file-name "$MY_ELISP/information-management/metoffice.el"))
(load-file (substitute-in-file-name "$MY_PROJECTS/JCGS-org-mode/lisp/org-parcels.el"))
(load-file (substitute-in-file-name "$MY_PROJECTS/JCGS-org-mode/lisp/org-ql-to-json.el"))
(message "(boundp 'org-ql-views) = %s" (boundp 'org-ql-views))
(load-file (substitute-in-file-name "$MY_ELISP/config/config-org-mode.el"))

(jcgs/org-ql-views-to-json (substitute-in-file-name "$COMMON/var/views.json")
                           '("Current"
                             "Supermarket"
                             "Online"
                             "Physical making"
                             "Programming"
                             "Mending"))

(jcgs/org-expected-dates-to-json-file (substitute-in-file-name "$COMMON/var/parcels-expected.json"))

(provide 'dashboard-emacs-query)
;;; dashboard-emacs-query.el ends here
