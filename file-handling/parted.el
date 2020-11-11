;;; parted.el --- functions for working with parted  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: convenience, hardware

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

(defun parted-mb-to-sectors (mb)
  "Convert MB to sectors."
  (* mb 2048))

(defun parted-gb-to-mb (gb)
  "Convert GB to MB."
  (* gb 1024))

(defun parted-gb-to-sectors (gb)
  "Convert GB to sectors."
  (* (parted-gb-to-mb gb) 2048))

(provide 'parted)
;;; parted.el ends here
