;;;; swadesh-numbers.el -- further stuff for swadesh lists
;;; Time-stamp: <2006-01-25 10:41:34 jcgs>

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

(provide 'swadesh-numbers)

(defvar swadesh-parts-of-speech
  [
   type
   pronoun				; 1: I
   pronoun				; 2: thou
   pronoun				; 3: he
   pronoun				; 4: we
   pronoun				; 5: you
   pronoun				; 6: they
   part					; 7: this
   part					; 8: that
   part					; 9: here
   part					; 10: there
   part					; 11: who
   part					; 12: what
   part					; 13: where
   part					; 14: when
   part					; 15: how
   conjunction				; 16: not
   part					; 17: all
   part					; 18: many
   part					; 19: some
   part					; 20: few
   part					; 21: other
   number				; 22: one
   number				; 23: two
   number				; 24: three
   number				; 25: four
   number				; 26: five
   adjective				; 27: big
   adjective				; 28: long
   adjective				; 29: wide
   adjective				; 30: thick
   adjective				; 31: heavy
   adjective				; 32: small
   adjective				; 33: short
   adjective				; 34: narrow
   adjective				; 35: thin
   noun					; 36: woman
   noun					; 37: man
   noun					; 38: person
   noun					; 39: child
   noun					; 40: wife
   noun					; 41: husband
   noun					; 42: mother
   noun					; 43: father
   noun					; 44: animal
   noun					; 45: fish
   noun					; 46: bird
   noun					; 47: dog
   noun					; 48: louse
   noun					; 49: snake
   noun					; 50: worm
   noun					; 51: tree
   noun					; 52: forest
   noun					; 53: stick
   noun					; 54: fruit
   noun					; 55: seed
   noun					; 56: leaf
   noun					; 57: root
   noun					; 58: bark
   noun					; 59: flower
   noun					; 60: grass
   noun					; 61: rope
   noun					; 62: skin
   noun					; 63: meat
   noun					; 64: blood
   noun					; 65: bone
   noun					; 66: fat
   noun					; 67: egg
   noun					; 68: horn
   noun					; 69: tail
   noun					; 70: feather
   noun					; 71: hair
   noun					; 72: head
   noun					; 73: ear
   noun					; 74: eye
   noun					; 75: nose
   noun					; 76: mouth
   noun					; 77: tooth
   noun					; 78: tongue
   noun					; 79: fingernail
   noun					; 80: foot
   noun					; 81: leg
   noun					; 82: knee
   noun					; 83: hand
   noun					; 84: wing
   noun					; 85: belly
   noun					; 86: guts
   noun					; 87: neck
   noun					; 88: back
   noun					; 89: breast
   noun					; 90: heart
   noun					; 91: liver
   verb					; 92: drink
   verb					; 93: eat
   verb					; 94: bite
   verb					; 95: suck
   verb					; 96: spit
   verb					; 97: vomit
   verb					; 98: blow
   verb					; 99: breathe
   verb					; 100: laugh
   verb					; 101: see
   verb					; 102: hear
   verb					; 103: know
   verb					; 104: think
   verb					; 105: smell
   verb					; 106: fear
   verb					; 107: sleep
   verb					; 108: live
   verb					; 109: die
   verb					; 110: kill
   verb					; 111: fight
   verb					; 112: hunt
   verb					; 113: hit
   verb					; 114: cut
   verb					; 115: split
   verb					; 116: stab
   verb					; 117: scratch
   verb					; 118: dig
   verb					; 119: swim
   verb					; 120: fly
   verb					; 121: walk
   verb					; 122: come
   verb					; 123: lie
   verb					; 124: sit
   verb					; 125: stand
   verb					; 126: turn
   verb					; 127: fall
   verb					; 128: give
   verb					; 129: hold
   verb					; 130: squeeze
   verb					; 131: rub
   verb					; 132: wash
   verb					; 133: wipe
   verb					; 134: pull
   verb					; 135: push
   verb					; 136: throw
   verb					; 137: tie
   verb					; 138: sew
   verb					; 139: count
   verb					; 140: say
   verb					; 141: sing
   verb					; 142: play
   verb					; 143: float
   verb					; 144: flow
   verb					; 145: freeze
   verb					; 146: swell
   noun					; 147: sun
   noun					; 148: moon
   noun					; 149: star
   noun					; 150: water
   noun					; 151: rain
   noun					; 152: river
   noun					; 153: lake
   noun					; 154: sea
   noun					; 155: salt
   noun					; 156: stone
   noun					; 157: sand
   noun					; 158: dust
   noun					; 159: earth
   noun					; 160: cloud
   noun					; 161: fog
   noun					; 162: sky
   noun					; 163: wind
   noun					; 164: snow
   noun					; 165: ice
   noun					; 166: smoke
   noun					; 167: fire
   noun					; 168: ashes
   verb					; 169: burn
   noun					; 170: road
   noun					; 171: mountain
   noun					; 172: red
   noun					; 173: green
   noun					; 174: yellow
   noun					; 175: white
   noun					; 176: black
   noun					; 177: night
   noun					; 178: day
   noun					; 179: year
   adjective				; 180: warm
   adjective				; 181: cold
   adjective				; 182: full
   adjective				; 183: new
   adjective				; 184: old
   adjective				; 185: good
   adjective				; 186: bad
   adjective				; 187: rotten
   adjective				; 188: dirty
   adjective				; 189: straight
   adjective				; 190: round
   adjective				; 191: sharp
   adjective				; 192: dull
   adjective				; 193: smooth
   adjective				; 194: wet
   adjective				; 195: dry
   adjective				; 196: right
   adjective				; 197: near
   adjective				; 198: far
   part					; 199: right
   part					; 200: left
   part					; 201: at
   part					; 202: in
   part					; 203: with
   conjunction				; 204: and
   part					; 205: if
   part					; 206: because
   noun					; 207: name
   ]
  "The part of speech for each Swadesh word.")

(defun swadesh-add-parts-of-speech ()
  "Add the parts of speech to a Swadesh csv file."
  (interactive)
  (goto-char (point-min))
  (unless (looking-at "\"#TYPE\"") (insert "\"#TYPE\","))
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    (when (looking-at "^\\(\"[a-z]+\",\\)?\"?\\([0-9]+\\)\"?")
      (let ((number (string-to-int (match-string-no-properties 2)))
	    (string (match-string-no-properties 1)))
	(message "number=%d string=%s" number string)
	(when string
	  (message "removing old type string")
	  (delete-region (match-beginning 1) (match-end 1)))
	(insert (format "\"%s\"," (symbol-name (aref swadesh-parts-of-speech number))))))))

;;; end of swadesh-numbers.el
