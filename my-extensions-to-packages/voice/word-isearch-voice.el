;;;; word-isearch-voice.el -- voice interface to word-isearch, which is designed mostly for voice
;;; Time-stamp: <2006-01-25 10:09:42 jcgs>

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

(provide 'word-isearch-voice)
(require 'word-isearch)

(defvar vr-word-isearch-commands
  '(("word search" . word-isearch))
  "Voice commands for searching by whole words.")

;;; end of word-isearch-voice.el
