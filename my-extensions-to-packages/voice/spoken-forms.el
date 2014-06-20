;;;; spoken-forms.el -- spoken forms of command names
;;; Time-stamp: <2006-01-25 15:51:29 jcgs>

(provide 'spoken-forms)

;; At first, I put the spoken forms for all my new modules into the
;; modules themselves, but that made me load them to have the spoken
;; forms available, and as part of my move towards using autoload
;; more, I have now gathered them here.

(defvar vr-journal-commands
  '(("new day" . journal-new-day)
    ("following day" . journal-next-day)
    ("reflect" . journal-reflection)
    journal-today))

(defvar vr-nested-blocks-voice-commands
  '(("enter block" . nested-blocks-enter)
    ("leave block" . nested-blocks-leave)
    ("another block" . nested-blocks-another)
    ("forward block" . nested-blocks-forward)
    ("backward block" . nested-blocks-backward))
  "Voice commands for handling nested blocks.")

;;; end of spoken-forms.el
