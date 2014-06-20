;;; generic-text.el -- generic things that get passed to TeX, LaTeX, html, troff, texinfo etc
;;; Time-stamp: <2006-11-10 12:05:34 jcgs>

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

(provide 'generic-text)
(require 'modal-functions)
(require 'text-sentences)

(defvar generic-text-before-hooks nil
  "Hooks to run at the start of each generic text function.
Each hook function is called with the current command (symbol) as its argument.
If any of them returns non-nil, the rest of the list is not evaluated.")

(defvar generic-text-after-hooks nil
  "Hooks to run at the end of each generic text function.
Each hook function is called with the current command (symbol) as its argument.
If any of them returns non-nil, the rest of the list is not evaluated.")

(require 'gentext-models)
(require 'gentext-bindings)

(require 'gentext-tex)
(require 'gentext-texinfo)
(require 'gentext-html)
(require 'gentext-roff)
(require 'gentext-muse)
(require 'gentext-plaintext)

;;; end of generic-text.el
