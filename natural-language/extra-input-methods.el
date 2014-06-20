;;;; extra-input-methods.el -- my own input methods -*- coding: iso-2022-7bit; -*-
;;; Time-stamp: <2010-07-07 23:01:44 jcgs>

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

(provide 'extra-input-methods)

;; emacs seems to expect iso-2022-7bit as the coding system for elisp files

(quail-define-package
 "old-irish-prefix" "Latin-1" "GA>" t
 "Old Irish input method with prefix modifiers
Key translation rules are the long vowels:
 'A -> ,AA(B   'E -> ,AI(B   'I -> ,AM(B   'O -> ,AS(B   'U -> ,AZ(B
and also the lenited consonants
 BH -> ,_!(B   CH -> ,_$(B   DH -> ,_&(B   GH -> ,_2(B   MH -> ,_4(B
 PH -> ,_7(B   SH -> ,_;(B   TH -> ,_W(B 
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,AA(B)
 ("'E" ?,AI(B)
 ("'I" ?,AM(B)
 ("'O" ?,AS(B)
 ("'U" ?,AZ(B)
 ("'a" ?,Aa(B)
 ("'e" ?,Ai(B)
 ("'i" ?,Am(B)
 ("'o" ?,As(B)
 ("'u" ?,Az(B)
 ("BH" ?,_!(B)
 ("CH" ?,_$(B)
 ("DH" ?,_&(B)
 ("FH" ?,_0(B)
 ("GH" ?,_2(B)
 ("MH" ?,_4(B)
 ("PH" ?,_7(B)
 ("SH" ?,_;(B)
 ("TH" ?,_W(B)
 ("Bh" ?,_!(B)
 ("Ch" ?,_$(B)
 ("Dh" ?,_&(B)
 ("Fh" ?,_0(B)
 ("Gh" ?,_2(B)
 ("Mh" ?,_4(B)
 ("Ph" ?,_7(B)
 ("Sh" ?,_;(B)
 ("Th" ?,_W(B)
 ("bh" ?,_"(B)
 ("ch" ?,_%(B)
 ("dh" ?,_+(B)
 ("fh" ?,_1(B)
 ("gh" ?,_3(B)
 ("mh" ?,_5(B)
 ("ph" ?,_9(B)
 ("sh" ?,_?(B)
 ("th" ?,_w(B)
 ("' " ?')
)

(quail-define-package
 "tone-pinyin-as-such" "Chinese" "TONE-PY" t
 "Tonal Pinyin input method that enters tonal pinyin into the buffer.")

(quail-define-rules
 ("a1" ?$,1 !(B)
 ("a2" ?,Aa(B)
 ("a3" ?$,1 #(B)
 ("a4" ?,A`(B)
 ("e1" ?$,1 3(B)
 ("e2" ?,Ai(B)
 ("e3" ?$,1 5(B)
 ("e4" ?,Ah(B)
 ("i1" ?$,1 K(B)
 ("i2" ?,Am(B)
 ("i3" ?$,1 M(B)
 ("i4" ?,Al(B)
 ("o1" ?$,1 m(B)
 ("o2" ?,As(B)
 ("o3" ?$,1 o(B)
 ("o4" ?,Ar(B)
 ("u1" ?$,1!+(B)
 ("u2" ?,Az(B)
 ("u3" ?$,1!-(B)
 ("u4" ?,Ay(B))

(quail-define-package
 "ogham" "Irish" "OG>" t
 "Ogham input method.
The nearest Latin letters are used."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; names in comments derived from table in Unicode section of
 ;; http://en.wikipedia.org/wiki/Ogham
 (" " ?$,1Y@(B)				; ogham space mark
 ("b" ?$,1YA(B)				; beith
 ("l" ?$,1YB(B)				; luis
 ("w" ?$,1YC(B)				; fearn
 ("s" ?$,1YD(B)				; sail
 ("n" ?$,1YE(B)				; nion
 ("y" ?$,1YF(B)				; uath
 ("d" ?$,1YG(B)				; dair
 ("t" ?$,1YH(B)				; tinne
 ("k" ?$,1YI(B)				; coll
 ("q" ?$,1YJ(B)				; ceirt
 ("m" ?$,1YK(B)				; muin
 ("g" ?$,1YL(B)				; gort
 ("ng" ?$,1YM(B)				; ngeadal
 ("z" ?$,1YN(B)				; straif
 ("r" ?$,1YO(B)				; ruis
 ("a" ?$,1YP(B)				; ailm
 ("o" ?$,1YQ(B)				; onn
 ("u" ?$,1YR(B)				; ur
 ("e" ?$,1YS(B)				; eadhadh
 ("i" ?$,1YT(B)				; iodhadh
 ("ea" ?$,1YU(B)				; eabhadh
 ("oi" ?$,1YV(B)				; or
 ("ui" ?$,1YW(B)				; uilleann
 ("io" ?$,1YX(B)				; ifin
 ("ae" ?$,1YY(B)				; eamhancholl
 ("p" ?$,1YZ(B)				; peith
 ("`" ?$,1Y[(B)				; feather mark
 ;; ("'" ?$,1Y\(B)				; reversed feather mark
 )

(quail-define-package
 "runic" "Norse" "RUN>" t
 "Runic input method.
The nearest Latin letters are used."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; names in comments extracted from table in Unicode section of
 ;; http://en.wikipedia.org/wiki/Runic_alphabet todo: sort out the
 ;; long and short versions; they should have their own input methods
 ;; (as well as this, or perhaps instead of this)
 ("f" ?$,1Y`(B)				; fehu feoh fe f
 ("/on" ?$,1Yp(B)				; on
 (".n" ?$,1Z (B)				; dotted-n
 ("t" ?$,1Z0(B)				; short-twig-tyr t
 ("/ear" ?$,1Z@(B)				; ear
 ("v" ?$,1Ya(B)				; v
 ("r" ?$,1Yq(B)				; raido rad reid r
 ("i" ?$,1Z!(B)				; isaz is iss i
 ("d" ?$,1Z1(B)				; d
 ("/ior" ?$,1ZA(B)				; ior
 ("u" ?$,1Yb(B)				; uruz ur u
 ("k" ?$,1Yr(B)				; kauna
 ("e" ?$,1Z"(B)				; e
 ("b" ?$,1Z2(B)				; berkanan beorc bjarkan b
 ("q" ?$,1ZB(B)				; cweorth
 ("/yr" ?$,1Yc(B)				; yr
 ("/cen" ?$,1Ys(B)				; cen
 ("j" ?$,1Z#(B)				; jeran j
 ("b" ?$,1Z3(B)				; short-twig-bjarkan b
 ("/calc" ?$,1ZC(B)				; calc
 ("y" ?$,1Yd(B)				; y
 ("k" ?$,1Yt(B)				; kaun k
 ("/get" ?$,1Z$(B)				; ger
 (".p" ?$,1Z4(B)				; dotted-p
 ("/cealc" ?$,1ZD(B)				; cealc
 ("w" ?$,1Ye(B)				; w
 ("g" ?$,1Yu(B)				; g
 ("ae" ?$,1Z%(B)				; long-branch-ar ae
 ("p" ?$,1Z5(B)				; open-p
 ("/stan" ?$,1ZE(B)				; stan
 ("th" ?$,1Yf(B)				; thurisaz thurs thorn
 ("/eng" ?$,1Yv(B)				; eng
 (".a" ?$,1Z&(B)				; short-twig-ar a
 ("e" ?$,1Z6(B)				; ehwaz eh e
 ("/y" ?$,1ZF(B)				; long-branch-yr
 ("/th" ?$,1Yg(B)				; eth
 ("g" ?$,1Yw(B)				; gebo gyfu g
 ("/iwaz" ?$,1Z'(B)				; iwaz eoh
 ("m" ?$,1Z7(B)				; mannaz man m
 (".y" ?$,1ZG(B)				; short-twig-yr
 ("a" ?$,1Yh(B)				; ansuz a
 ("\gar" ?$,1Yx(B)				; gar
 ("p" ?$,1Z((B)				; pertho peorth p
 ("/m" ?$,1Z8(B)				; long-branch-madr m
 ("/yr" ?$,1ZH(B)				; icelandic-yr
 ("o" ?$,1Yi(B)				; os o
 ("w" ?$,1Yy(B)				; wunjo wynn w
 ("/algiz" ?$,1Z)(B)				; algiz eolhx
 (".m" ?$,1Z9(B)				; short-twig-madr m
 ("q" ?$,1ZI(B)				; q
 ("a" ?$,1Yj(B)				; ac a
 ("h" ?$,1Yz(B)				; haglaz h
 ("s" ?$,1Z*(B)				; sowilo s
 ("l" ?$,1Z:(B)				; laukaz lagu logr l
 ("x" ?$,1ZJ(B)				; x
 ("/aesc" ?$,1Yk(B)				; aesc
 ("h" ?$,1Y{(B)				; haegl h
 ("/s" ?$,1Z+(B)				; sigel long-branch-sol s
 (".l" ?$,1Z;(B)				; dotted-l
 ("/o" ?$,1Yl(B)				; long-branch-oss o
 ("/h" ?$,1Y|(B)				; long-branch-hagall h
 (".s" ?$,1Z,(B)				; short-twig-sol s
 ("/ingwaz" ?$,1Z<(B)				; ingwaz
 (".o" ?$,1Ym(B)				; short-twig-oss o
 (".h" ?$,1Y}(B)				; short-twig-hagall h
 ("c" ?$,1Z-(B)				; c
 ("" ?$,1Z=(B)				; ing
 ("o" ?$,1Yn(B)				; o
 ("n" ?$,1Y~(B)				; naudiz nyd naud n
 ("z" ?$,1Z.(B)				; z
 ("d" ?$,1Z>(B)				; dagaz daeg d
 ("oe" ?$,1Yo(B)				; oe
 ("n" ?$,1Y(B)				; short-twig-naud n
 ("t" ?$,1Z/(B)				; tiwaz tir tyr t
 ("/o" ?$,1Z?(B)				; othalan ethel o
 )

(quail-define-package
 "Dalrunic" "Swedish" "Dal>" t
 "Runic for Dalarna runes input method.
The nearest Latin letters are used."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; Derived from http://en.wikipedia.org/wiki/Rune#Dalrune
  ("a" ?$,1Z&(B)
  ("b" ?$,1Z2(B)
  ("c" ?$,1Z-(B)
  ("d" ?$,1Z1(B)
  ("th" ?$,1Yf(B)
  ("/th" ?$,1Yg(B)
  ("e" ?$,1Z"(B)
  ("f" ?$,1Y`(B)
  ("g" ?$,1Yu(B)
  ("h" ?$,1Y|(B)
  ("i" ?$,1Z!(B)
  ("k" ?$,1Yt(B)
  ("l" ?$,1Z:(B)
  ("m" ?$,1Z8(B)
  ("n" ?$,1Y(B)
  ("o" ?$,1Yn(B)
  ("p" ?$,1Z4(B)
  ("r" ?$,1Yq(B)
  ("s" ?$,1Z,(B)
  ("t" ?$,1Z0(B)
  ("u" ?$,1Yb(B)
  ("v" ?$,1Ya(B)
  ("y" ?$,1ZF(B)
  ("z" ?$,1Z.(B)
  ("ae" ?$,1Z%(B)
  ("o/" ?$,1Yo(B))

(when nil
  (quail-define-package
   "hebrew-biblical" "Biblical Hebrew" ",Hr(B" nil "Hebrew (ISO 8859-8) input method.

Based on Hebrew typewriter keys.
Hebrew letters are assigned to lowercases.
Niqqud are included
" nil t t t t nil nil nil nil nil t)

  ;;  1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0) -_ =+ ;~
  ;;   /Q 'W ,Hw(BE ,Hx(BR ,H`(BT ,Hh(BY ,He(BU ,Ho(BI ,Hm(BO ,Ht(BP [{ ]}
  ;;    ,Hy(BA ,Hc(BS ,Hb(BD ,Hk(BF ,Hr(BG ,Hi(BH ,Hg(BJ ,Hl(BK ,Hj(BL ,Hs(B: ," \|
  ;;     ,Hf(BZ ,Hq(BX ,Ha(BC ,Hd(BV ,Hp(BB ,Hn(BN ,Hv(BM ,Hz(B< ,Hu(B> .?
  ;;

  (quail-define-rules
   ("`" ?\;)
   ("q" ?/)
   ("w" ?')
   ("e" ?,Hw(B)
   ("r" ?,Hx(B)
   ("t" ?,H`(B)
   ("y" ?,Hh(B)
   ("u" ?,He(B)
   ("i" ?,Ho(B)
   ("o" ?,Hm(B)
   ("p" ?,Ht(B)
   ("a" ?,Hy(B)
   ("s" ?,Hc(B)
   ("d" ?,Hb(B)
   ("f" ?,Hk(B)
   ("g" ?,Hr(B)
   ("h" ?,Hi(B)
   ("j" ?,Hg(B)
   ("k" ?,Hl(B)
   ("l" ?,Hj(B)
   (";" ?,Hs(B)
   ("z" ?,Hf(B)
   ("x" ?,Hq(B)
   ("c" ?,Ha(B)
   ("v" ?,Hd(B)
   ("b" ?,Hp(B)
   ("n" ?,Hn(B)
   ("m" ?,Hv(B)
   ("," ?,Hz(B)
   ("." ?,Hu(B)
   ("~" ?\x05B0) ;; HEBREW POINT SHEVA
   ("!" ?\x05B1) ;; HEBREW POINT HATAF SEGOL
   ("\"" ?\x05B2) ;; HEBREW POINT HATAF PATAH
   ("#" ?\x05B3)  ;; HEBREW POINT HATAF QAMATS
   ("$" ?\x05B4)  ;; HEBREW POINT HIRIQ
   ("%" ?\x05B5)  ;; HEBREW POINT TSERE
   ("^" ?\x05B6)  ;; HEBREW POINT SEGOL
   ("&" ?\x05B7)  ;; HEBREW POINT PATAH
   ("*" ?\x05B8)  ;; HEBREW POINT QAMATS
   ("-" ?\x05B9)  ;; HEBREW POINT HOLAM
   ("|" ?\x05BB)  ;; HEBREW POINT QUBUTS
   ("+" ?\x05BC)  ;; HEBREW POINT DAGESH
					;	 (?\M . ?\x05BD) ;; HEBREW POINT METEG
					;	 (?\N . ?\x05BE) ;; HEBREW POINT MAQAF
					;	 (?\O . ?\x05BF) ;; HEBREW POINT RAFE
					;	 (?\P . ?\x05C0) ;; HEBREW PUNCTUATION PASEQ
   (")" ?\x05C1)			;; HEBREW POINT SHIN DOT
   ("(" ?\x05C2)			;; HEBREW POINT SIN DOT
					;	 (?\S . ?\x05C3) ;; HEBREW PUNCTUATION SOF PASUQ



   ("/" ?.)
   ("'" ?,)))

(quail-define-package
 "icelandic-prefix" "Icelandic" "ICE>" t
 "Icelandic input method with prefix modifiers."
 ;; Non-native, invented by JCGS for his own use, 2010-07-07
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'a" ?,Aa(B)
 ("'A" ?,AA(B)
 ("/d" ?,Ap(B)
 ("/D" ?,AP(B)
 ("'e" ?,Ai(B)
 ("'E" ?,AI(B)
 ("'i" ?,Am(B)
 ("'I" ?,AM(B)
 ("'o" ?,As(B)
 ("'O" ?,AS(B)
 ("'u" ?,Az(B)
 ("'U" ?,AZ(B)
 ("'y" ?,A}(B)
 ("'Y" ?,A](B)
 ("/o" ?,Av(B)
 ("/O" ?,AV(B)
 ("/t" ?,A^(B)
 ("/T" ?,A~(B)
 ("ae" ?,Af(B)
 ("AE" ?,AF(B))

(quail-define-package
 "maltese-prefix" "Maltese" "MLS>" t
 "Maltese input method with prefix modifiers."
 ;; Non-native, invented by JCGS for his own use, 2010-07-07
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 (".c" ?,Ce(B)
 (".C" ?,CE(B)
 (".g" ?,Cu(B)
 (".G" ?,CU(B)
 ("/h" ?,C1(B)
 ("/H" ?,C!(B)
 (".z" ?,B?(B)
 (".Z" ?,B/(B))

;;; end of extra-input-methods.el
