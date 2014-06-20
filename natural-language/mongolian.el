;;; mongolian.el --- mongolian input method

;; Copyright (C) 2008, 2012  John Sturdy

;; Author: John Sturdy <john.sturdy@ul.ie>
;; Keywords: languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; According to the layout diagrams available from the OLPC project,
;; and others, the Mongolian Cyrillic keyboard layout is quite, but
;; not completely, different from the Russian one.

;; Because my keyboard (US-style) doesn't have a printing character
;; key to the right of "=+", I've transferred that letter to "`~"
;; (i.e. the other end of the same row).  I'd welcome comment and
;; corrections from a regular Mongolian computer user.

;; Note:

;; I wrote this at an early stage of learning Mongolian, so that I
;; could use it for making grammar notes, etc.  It should be checked
;; by someone who is accustomed to typing in Mongolian!

;;; Code:

(quail-define-package
 "mongolian-cyrillic" "Mongolian" "MN" nil
 "Mongolian computer layout"
 nil t t t t nil nil nil nil nil t)

;; Keyboard diagram from OLPC:
;; =+ 1№ 2- 3" 4₮ 5: 6. 7_ 8, 9% 0? Е Щ 
;;   Ф  Ц  У  Ж  Э  Н  Г  Ш  ү  З  К  Ъ
;;    Й  Ы  Б  Ө  А  Х  Р  О  Л  Д  П !|
;;     Я  Ч  Ё  С  М И  Т  Ь  В  Ю 

;;; This is "russian-computer", for comparison
;;  1! 2" 3№ 4; 5% 6: 7? 8* 9( 0) -_ =+ ёЁ
;;   Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  Ъ
;;    Ф  Ы  В  А  П  Р  О  Л  Д  Ж  Э
;;     Я  Ч  С  М  И  Т  Ь  Б  Ю  .,

(quail-define-rules
 ;; top row
 ("`" ?=)
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?е)
 ("=" ?щ)
 ;; upper letter row
 ("q" ?ф)
 ("w" ?ц)
 ("e" ?у)
 ("r" ?ж)
 ("t" ?э)
 ("y" ?н)
 ("u" ?г)
 ("i" ?ш)
 ("o" ?ү)
 ("p" ?з)
 ("[" ?к)
 ("]" ?ъ)
 ;; home row
 ("a" ?й)
 ("s" ?ы)
 ("d" ?б)
 ("f" ?ө)
 ("g" ?а)
 ("h" ?х)
 ("j" ?р)
 ("k" ?о)
 ("l" ?л)
 (";" ?д)
 ("'" ?п)
 ("\\" ?!)
 ;; lower letter row
 ("z" ?я)
 ("x" ?ч)
 ("c" ?ё)
 ("v" ?с)
 ("b" ?м)
 ("n" ?и)
 ("m" ?т)
 ("," ?ь)
 ("." ?в)
 ("/" ?ю)
 ;; top row, shifted
 ("~" ?+)
 ("!" ?№)
 ("@" ?-)
 ("#" ?\")
 ("$" ?₮)
 ("%" ?:)
 ("^" ?.)
 ("&" ?_)
 ("*" ?,)
 ("(" ?%)
 (")" ??)
 ("_" ?Е)
 ("+" ?Щ)
 ;; upper letter row, shifted
 ("Q" ?Ф)
 ("W" ?Ц)
 ("E" ?У)
 ("R" ?Ж)
 ("T" ?Э)
 ("Y" ?Н)
 ("U" ?Г)
 ("I" ?Ш)
 ("O" ?Ү)
 ("P" ?З)
 ("{" ?К)
 ("}" ?Ъ)
 ;; home row, shifted
 ("A" ?Й)
 ("S" ?Ы)
 ("D" ?Б)
 ("F" ?Ө)
 ("G" ?А)
 ("H" ?Х)
 ("J" ?Р)
 ("K" ?О)
 ("L" ?Л)
 (":" ?Д)
 ("\"" ?П)
 ("|" ?|)
 ;; lower letter row, shifted
 ("Z" ?Я)
 ("X" ?Ч)
 ("C" ?Ё)
 ("V" ?С)
 ("B" ?М)
 ("N" ?И)
 ("M" ?Т)
 ("<" ?Ь)
 (">" ?В)
 ("?" ?Ю))

;;; mongolian.el ends here
