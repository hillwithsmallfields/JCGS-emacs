;;;; jcgs-vr-mode-setup.el -- my vr-mode configuration (commands loader etc)
;;; Time-stamp: <2007-06-30 17:52:44 jcgs>

(defvar voice-required-features
  '(spoken-forms
    versor-voice
    gentext-voice
    emacs-lisp-voice
    flexi-choose-voice
    general-voice
    insert-voice
    text-structures
    dired-voice
    rpn-edit
    voicescript
    languide
    embedded-commands
    type-break-voice
    misc-voice
    movements-voice
    prefix-voice
    texinfo-voice
    word-isearch-voice
    voice-assist-key
    ;; sidebrain-voice
    planner-voice)
  "Things that must be required to use with the voice system.")

(defun jcgs-vr-mode-setup ()
  "Set up my vr-mode settings, load files needed, etc.
Run as a hook by vr-mode as it starts."
  (setq vr-voice-command-list
	'(
	  vr-almost-default-voice-command-list ; modified from vr-default-voice-command-list provided with vr-mode.el
	  vr-dired-commands
	  vr-general-commands	     ; assorted things that I've added

	  vr-versor-command-list  ; movements within current dimension
	  vr-versor-dimension-command-list ; names of dimensions

	  vr-flexi-choose-top-commands ; commands that use flexi choose
	  vr-flexi-choose-commands	; shortcuts directly to lines
	  vr-prefix-commands

	  rpn-voice-commands		; editing using a stack
	  vs-command-list		; control of  voice scripting
	  sensible-languages-commands ; navigation according to the sense of programming languages
	  vr-nested-blocks-voice-commands ; generalized in/out/over nested things
	  vr-embedded-commands-top-commands ; recognize command phrases in text as it is entered

	  vr-gentext-commands ; markup constructs in various markup languages
	  vr-emacs-lisp-commands	; for programming in elisp

	  vr-insert-commands ; parentheses, comment, latest, expand etc
	  vr-text-structure-commands	; parentheses etc
	  ;; vr-insert-menu-commands

	  vr-movement-commands
	  vr-journal-commands	; journalling stuff -- start a new day

	  type-break-voice-commands

	  ;; stuff that came in the original pack
	  vr-cachepad-command-list
	  ;; vr-else-command-list
					; vr-emacs-command-list
					; vr-python-command-list
					; vr-setnu-command-list
	  ;; vr-translate-mode-command-list
	  ;; vr-voicegrip-command-list
	  ;; vr-vr-command-list

	  vr-sidebrain-commands

	  vr-texinfo-commands
	  vr-muse-commands

	  vr-word-isearch-commands

	  vr-planner-commands

	  misc-voice-command-list

	  vr-vr-help-command-list
	  )
	vr-command (substitute-in-file-name "$GATHERED/emacs/voice/vrmode/vr.exe")
	vr-log-send t
	vr-log-read t
	vr-log-heard t
	vr-log-do t
	vr-active-minibuffers nil
	;; vr-win-class "Emacs"
	vr-activation-list '("^\\*scratch\\*$" "\\.txt$"))
(mapcar 'require voice-required-features))

;;; end of jcgs-vr-mode-setup.el
