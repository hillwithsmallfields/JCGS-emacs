;;; n900-flashcards.el --- Flashcards adaptation for n900

;; Copyright (C) 2011  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;;;;;;;;;;;;;;
;; flashcards ;;
;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/home/user/library/emacs/flashcard/")
(require 'flashcard)
(require 'flashcard-sm5)
(setq flashcard-coding-system 'utf-8)
(set-face-foreground 'flashcard-question-face "blue")
(set-face-attribute 'flashcard-question-face nil :height 400)
(set-face-attribute 'flashcard-answer-face nil :height 400)
(set-face-foreground 'flashcard-answer-face "dark green")
(add-hook 'flashcard-mode-hook
	  (lambda ()
	    (define-key flashcard-mode-map "\C-c" 'flashcard-extra-map)
	    (flashcard-method-sm5-js)))
(add-hook 'flashcard-mode-hook
	  'flashcard-add-scroll-to-bottom)

(defvar flashcard-displayable-count 0
  "How many questions have been asked.")

(defun flashcard-increment-displayable-count ()
  "Display how many questions have been asked."
  (setq flashcard-displayable-count (1+ flashcard-displayable-count)))

(add-hook 'flashcard-post-question-hook
	  'flashcard-increment-displayable-count)

(defun flashcard-add-display-counter ()
  (add-to-list 'mode-line-format '(" Q" flashcard-displayable-count)
	       t))

(add-hook 'flashcard-mode-hook
	  'flashcard-add-display-counter)

(defvar flashcard-method-sm5-js-answer-code-meanings
  '((?0 . 0)
    (?1 . 1)
    (?2 . 2)
    (?3 . 3)
    (?4 . 4)
    (?5 . 5)
    (?q . 1)
    (?w . 2)
    (?e . 3)
    (?r . 4)
    (?t . 5)
    (?p . 0)
    (?a . 0)
    (PinkieBtn-up . 0)
    (Trigger-up . 1)
    (ThumbBtn-up . 2)
    (ThumbBtn2-up . 3)
    (TopBtn-up . 4)
    (BaseBtn2-up . 5))
  "Alist of meanings of reply codes.")

(defun flashcard-method-sm5-js-check-answer (card answer)
  "Insert the answer, ask the user for a quality point.
This refers to CARD and the given ANSWER."
  (flashcard-insert "The correct answer is:\n"
                    (propertize (flashcard-card-answer card)
                                'face 'flashcard-answer-face
                                'rear-nonsticky t) "\n")
  (catch 'done
    (while t
      (let* ((code (read-event "Quality of your answer (0-5): "))
	     (meaning (cdr (assoc code flashcard-method-sm5-js-answer-code-meanings))))
	(when meaning
	  (message "Quality %d" meaning)
	  (throw 'done meaning))))))


(defun flashcard-method-sm5-js-correct-p (code)
  "Indicate whether CODE indicates a correct answer."
  (>= code 4))

(defun flashcard-method-sm5-js ()
  "A joystick-extended version of flashcard-method-sm5."
  (interactive)
  (flashcard-method-sm5)
  (setq flashcard-method-check-answer-function 'flashcard-method-sm5-js-check-answer
	flashcard-method-correct-p-function 'flashcard-method-sm5-js-correct-p)
  (define-key flashcard-mode-map [ PinkieBtn-up ] 'flashcard-input))

(add-to-list 'auto-mode-alist (cons "\\.deck" 'flashcard-mode))

(provide 'n900-flashcards)
;;; n900-flashcards.el ends here
