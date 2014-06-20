;;;; search-engines.el -- query AV via w3
;;; Time-stamp: <2004-11-23 12:51:45 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; todo: add more search engines
;; todo: parse result pages

(defvar altavista:search-mode "aq"
  "*The search mode for altavista.");

(defun altavista (search-expression)
  "Do an altavista search for SEARCH-EXPRESSION."
  (interactive "sAltavista search for: ")
  (require 'w3)
  (w3-fetch
   (format
    "http://altavista.com/cgi-bin/query?pg=%s&text=yes&what=web&fmt=d&q=%s"
    altavista:search-mode
    search-expression)))

;;; end of search-engines.el
