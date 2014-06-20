;;;; just-voice-setup.el
;;; Time-stamp: <2013-10-15 12:22:08 johnstu>

(require 'cl)

(setq stack-trace-on-error t
      message-log-max t)
(unless (getenv "COMMON")
  (setenv "COMMON" "i:/common"))
(unless (getenv "GATHERED")
  (setenv "GATHERED" "j:/users/jcgs/library"))

(message "In just-voice-setup.el")

(load-file (expand-file-name "basics/add-lispdir.el" user-emacs-directory))

(defun load-and-find-file (file)
  (message "Loading and finding %S" file)
  (find-file file)
  (load-file file))

(setq old-voice t)

(setq voice-el-file
      (expand-file-name "mode-setups/setup-voice.el" user-emacs-directory))
(find-file (expand-file-name "special-setups/just-voice/just-voice-setup.el" user-emacs-directory))
(find-file voice-el-file)
;; (load-file voice-el-file)

(defun jcgs-report-vr-setup (whence)
  "Report my vr setup, as seen when called WHENCE."
  (message "Starting VR connection: at %s, vr-command=%S vr-host=%S vr-port=%S" whence vr-command vr-host vr-port))

(progn
  (message "Choosing voice system")

  (if (and (boundp 'old-voice) old-voice)
      (progn
	(message "Using old version of voice system")
	(load-and-find-file (substitute-in-file-name "$GATHERED/emacs/voice/vrmode/vr.el")))
    (let ((voice-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-vr-mode/vrmode/")))
      (message "Using new version of voice system")
      (add-lispdir voice-dir)
      (load-and-find-file (expand-file-name "vr.el" voice-dir))))

  (message "Chosen voice system and loaded library")

  ;; the collection of voice commands provided in the original pbvkit:
  (message "just a message: %S" (substitute-in-file-name "$GATHERED/emacs/voice/elisp/vr-mode-commands.el"))
  (load-and-find-file (substitute-in-file-name "$GATHERED/emacs/voice/elisp/vr-mode-commands.el"))

;   (add-lispdir "$COMMON/emacs/my-extensions-to-packages/voice")
;   ;; (add-lispdir "$GATHERED/emacs/voice/else")
;   (add-lispdir "$GATHERED/emacs/voice/elisp")
;   ;; (add-lispdir "$GATHERED/emacs/voice/voicegrip/EmacsMacros")

;   (message "Adding features needed to support my voice commands")

;   ;; The following packages are loaded to make their contributions to the
;   ;; voice command list.
;   (require 'versor-voice)
;   (require 'html-helper-voice)
;   (require 'journal)
;   (require 'emacs-lisp-voice)
;   (require 'flexi-choose-voice)
;   (require 'general-voice)
;   (require 'insert-voice)
;   (require 'dired-voice)
;   (require 'rpn-edit)
;   (require 'voicescript)
;   (require 'languide)
;   (require 'nested-blocks)
;   (require 'embedded-commands)
;   (require 'type-break-voice)

;   (message "Required features loaded for voice")

;   ;; (load-library "else-mode.el")
;   (load-library "cachepad.el")
;   ;; (load-library "translate-mode.el")
;   ;; (load-library "vgEmacs.el")

;   (setq vr-voice-command-list
; 	'(
; 	  vr-default-voice-command-list ; provided with vr-mode.el
; 	  vr-dired-commands
; 	  vr-general-commands		; assorted things that I've added

; 	  vr-versor-command-list	; movements within current dimension
; 	  vr-versor-dimension-command-list ; names of dimensions

; 	  vr-flexi-choose-top-commands	; commands that use flexi choose
; 	  vr-flexi-choose-commands	; shortcuts directly to lines
; 	  ;; vr-prefix-commands

; 	  rpn-voice-commands		; editing using a stack
; 	  vs-command-list		; control of  voice scripting
; 	  sensible-languages-commands	; navigation according to the sense of programming languages
; 	  vr-nested-blocks-voice-commands ; generalized in/out/over nested things
; 	  vr-embedded-commands-top-commands ; major mode that recognises command phrases in text as it is entered

; 	  ;; vr-html-helper-commands ; the machine-generated set -- not sure I like all this many
; 	  vr-html-helper-top-commands	; insert selected HTML tags
; 	  vr-emacs-lisp-commands	; for programming in elisp

; 	  vr-insert-commands		; parentheses, comment, latest, expand etc
; 	  ;; vr-insert-menu-commands

; 	  vr-journal-commands		; journalling stuff -- start a new day

; 	  type-break-voice-commands

; 	  ;; stuff that came in the original pack
; 	  vr-cachepad-command-list
; 	  ;; vr-else-command-list
; 					; vr-emacs-command-list
; 					; vr-python-command-list
; 					; vr-setnu-command-list
; 	  ;; vr-translate-mode-command-list
; 	  ;; vr-voicegrip-command-list
; 	  ;; vr-vr-command-list
; 	  ))

;   (setup-flexi-choose-minibuffer)

  (setq vr-command
	(substitute-in-file-name
	 (if (and (boundp 'old-voice) old-voice)
	     "$GATHERED/emacs/voice/vrmode/vr.exe"
	   "$GATHERED/emacs/voice/vrmode9/vr.exe")))

  (add-hook 'vr-mode-setup-hook (lambda () (jcgs-report-vr-setup "from vr-mode-setup-hook")))
  (add-hook 'vr-mode-startup-hook (lambda () (jcgs-report-vr-setup "from vr-mode-startup-hook")))
  (message "About to start vr mode, using %s" vr-command)
  (message "Command list is %S" vr-voice-command-list)
  (vr-mode 1)
  (message "Started vr mode")
  ;; (create-cache-window)
  (message "Created cache window")
  )

;;; end of just-voice-setup.el
