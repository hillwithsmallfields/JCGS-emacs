;;;; vr-mode-commands.el
;;; Time-stamp: <2007-06-28 13:35:42 jcgs>

; CachePad voice commands

(defvar vr-cachepad-command-list 
      '(
      create-cache-window
	("cache buffer".  insert-selection-into-cache-pad)
	("cache region".  insert-region-into-cache-pad)
	("cache word".  insert-cache-pad)
	("recent 1".  insert-recent-1)
	("recent 2".  insert-recent-2)
	("recent 3".  insert-recent-3)
	("recent 4".  insert-recent-4)
	("recent 5".  insert-recent-5)
	("recent 6".  insert-recent-6)
	("recent 7".  insert-recent-7)
	("recent 8".  insert-recent-8)
	("recent 9".  insert-recent-9)
	("recent 10".  insert-recent-10)
	("recent 11".  insert-recent-11)
	("recent 12".  insert-recent-12)
	("recent 13".  insert-recent-13)
	("recent 14".  insert-recent-14)
	("recent 15".  insert-recent-15)
	("recent 16".  insert-recent-16)
	("recent 17".  insert-recent-17)
	("recent 18".  insert-recent-18)
	("recent 19".  insert-recent-19)
	("recent 20".  insert-recent-20)
	))

; functions that support CachePad commands

(defun insert-recent-1 () (interactive) (insert-nth-cache 1))
(defun insert-recent-2 () (interactive) (insert-nth-cache 2))
(defun insert-recent-3 () (interactive) (insert-nth-cache 3))
(defun insert-recent-4 () (interactive) (insert-nth-cache 4))
(defun insert-recent-5 () (interactive) (insert-nth-cache 5))
(defun insert-recent-6 () (interactive) (insert-nth-cache 6))
(defun insert-recent-7 () (interactive) (insert-nth-cache 7))
(defun insert-recent-8 () (interactive) (insert-nth-cache 8))
(defun insert-recent-9 () (interactive) (insert-nth-cache 9))
(defun insert-recent-10 () (interactive) (insert-nth-cache 10))
(defun insert-recent-11 () (interactive) (insert-nth-cache 11))
(defun insert-recent-12 () (interactive) (insert-nth-cache 12))
(defun insert-recent-13 () (interactive) (insert-nth-cache 13))
(defun insert-recent-14 () (interactive) (insert-nth-cache 14))
(defun insert-recent-15 () (interactive) (insert-nth-cache 15))
(defun insert-recent-16 () (interactive) (insert-nth-cache 16))
(defun insert-recent-17 () (interactive) (insert-nth-cache 17))
(defun insert-recent-18 () (interactive) (insert-nth-cache 18))
(defun insert-recent-19 () (interactive) (insert-nth-cache 19))
(defun insert-recent-20 () (interactive) (insert-nth-cache 20))

; ELSE Mode voice commands

(defvar vr-else-command-list 
  '(
    ("toggle else mode" . else-mode)
    ("compile buffer templates" . else-compile-buffer)
    ("delete placeholder" . else-kill-placeholder)
    ("expand placeholder" . else-expand-placeholder)
    ("next placeholder" . else-next-placeholder)
    ("previous placeholder" . else-previous-placeholder)
    ))

; Emacs voice commands

; help mode commands
(defvar vr-help-mode-command-list 
  '(
    ("follow hyperlink" . help-follow)
    ("follow next hyperlink" . help-follow-next-ref)
    ("follow previous hyperlink" . help-follow-previous-ref)
    ("next hyperlink" . help-next-ref)
    ("previous hyperlink" . help-previous-ref)
    ("go back" . help-go-back)
    ))

(defun help-follow-next-ref () 
  (interactive) 
  (help-next-ref)
  (help-follow (point)))

(defun help-follow-previous-ref () 
  (interactive) 
  (help-previous-ref)
  (help-follow (point)))

; info mode commands
; TBD remove 'info' from these commands when we can activate commands
; based on the mode
(defvar vr-info-mode-command-list 
  '(
    ("quit info mode" . Info-exit)
    ("next node" . Info-next)
    ("previous node" . Info-prev)
    ("up node" . Info-up)
    ("top node" . Info-top-node)
    info-menu
    ("info go back" . Info-last)
    info-directory
    info-index
    ("follow info link" . Info-follow-nearest-node)
    ("next info link" . Info-next-reference)
    ("previous info link" . Info-prev-reference)
    ("follow specific info link" . Info-follow-reference)
    ("goto node" . Info-goto-node)
    Info-search
    choose-info-menu-1
    choose-info-menu-2
    choose-info-menu-3
    choose-info-menu-4
    choose-info-menu-5
    choose-info-menu-6
    choose-info-menu-7
    choose-info-menu-8
    choose-info-menu-9
    ))
(defun choose-info-menu-1 () (interactive) (Info-goto-node (Info-extract-menu-counting 1)))
(defun choose-info-menu-2 () (interactive) (Info-goto-node (Info-extract-menu-counting 2)))
(defun choose-info-menu-3 () (interactive) (Info-goto-node (Info-extract-menu-counting 3)))
(defun choose-info-menu-4 () (interactive) (Info-goto-node (Info-extract-menu-counting 4)))
(defun choose-info-menu-5 () (interactive) (Info-goto-node (Info-extract-menu-counting 5)))
(defun choose-info-menu-6 () (interactive) (Info-goto-node (Info-extract-menu-counting 6)))
(defun choose-info-menu-7 () (interactive) (Info-goto-node (Info-extract-menu-counting 7)))
(defun choose-info-menu-8 () (interactive) (Info-goto-node (Info-extract-menu-counting 8)))
(defun choose-info-menu-9 () (interactive) (Info-goto-node (Info-extract-menu-counting 9)))

(defvar vr-emacs-command-list
  '(
    help-for-help
    append-next-kill
    append-to-buffer
    append-to-file
    append-to-register
    auto-fill-mode
    backward-kill-paragraph
    backward-kill-sentence
    ("backward kill expression". backward-kill-sexp)
    backward-kill-word
    backward-line
    backward-list
    backward-page
    backward-paragraph
    backward-sentence
    backward-word
    ("backward character" . backward-char)
    ("backward expression".  backward-sexp) 
    beginning-of-buffer 
    beginning-of-buffer-other-window
    ("beginning of function" . beginning-of-defun)
    beginning-of-line
    byte-compile-file
    capitalize-region
    capitalize-word
    center-line
    center-paragraph
    center-region
    ("center current line" .  recenter)
    comment-region
    copy-to-buffer
    ("copy that" . copy-region-as-kill)
    ("cut that" . kill-region)
    delete-blank-lines
    delete-horizontal-space
    delete-indentation
    delete-other-windows
    delete-rectangle
    delete-region
    delete-window 
    ("delete backward character" . delete-backward-char)
    ("delete character" . delete-char)
    describe-bindings
    describe-function
    describe-key
    describe-mode
    describe-syntax
    describe-variable
    ("describe keyword" . apropos-command)
    end-of-buffer
    end-of-buffer-other-window
    ("end of function". end-of-defun)
    end-of-line 
    enlarge-window
    enlarge-window-horizontally
    ("enter" . [?\C-j])
    erase-buffer
    ("evaluate current buffer" . eval-current-buffer)
    ("evaluate buffer" . eval-buffer)
    ("evaluate function" . eval-defun)
    ("evaluate region" . eval-region)
    ("evaluate expression" .eval-expression)
    exchange-point-and-mark
    execute-extended-command
    exit-recursive-edit
    fill-paragraph
    fill-region
    fill-region-as-paragraph
    find-file
    find-file-read-only
    find-file-read-only-other-frame
    find-file-read-only-other-window
    find-alternate-file
    find-alternate-file-other-window
    find-file 
    find-file-other-frame
    find-file-other-window 
    forward-line
    forward-list
    forward-page
    forward-paragraph
    forward-sentence
    forward-word
    ("forward character" . forward-char)
    ("forward expression".  forward-sexp) 
    ("go to character".  goto-char)
    goto-line
    indent-region
    indent-rigidly
    ("indent expression" . indent-sexp)
    insert-buffer
    insert-file
    insert-parentheses
    justify-current-line
    kill-buffer 
    kill-comment
    kill-line
    kill-paragraph
    kill-rectangle
    kill-region
    kill-sentence
    kill-word
    ("kill expression".  kill-sexp)
    kill-this-buffer
    ("lower case region" .downcase-region)
    ("lower case word" . downcase-word)
    mark-paragraph
    mark-whole-buffer
    mark-word
    mark-page
    ("mark function" . mark-defun)
    ("mark expression" .mark-sexp)
    next-line
    ("next match" . [?\C-s])
    ("numeric argument".  universal-argument)
    other-window 
    other-frame
    ("page down" . scroll-up) 
    ("page up" . scroll-down)
    ("paste that" . yank)
    ("prefix argument".  universal-argument)
    previous-line
    ("previous match" . [?\C-r])
    query-replace
    ("quit" . [?\C-g])
    ("quote".  [?\C-q])
    ("regular expression search forward".  re-search-forward)
    ("regular expression search backward".  re-search-backward)
    replace-string
    reposition-window
    reverse-region
    save-buffer 
    ("save file" . save-buffer) 
    scroll-down 
    scroll-left
    scroll-right
    scroll-up
    scroll-other-window
    ("scroll other window up" .scroll-other-window-down)
    search-forward
    search-backward
    set-comment-column
    set-fill-column
    set-fill-prefix
    shell-command
    shell-command-on-region
    shrink-window
    shrink-window-horizontally
    sort-columns
    sort-fields
    sort-lines
    sort-pages
    sort-paragraphs
    ("space" . [? ])
    ("split window" . split-window-vertically)
    split-window-horizontally
    switch-to-buffer 
    switch-to-buffer-other-frame
    switch-to-buffer-other-window
    ("tab" . [?\C-i])
    toggle-read-only
    ("transpose characters" . transpose-chars)
    transpose-lines
    transpose-paragraphs
    transpose-sentences
    ("transpose expressions". transpose-sexps)
    transpose-words
    ("undo that" . undo)
    universal-argument
    ("upper case region" . upcase-region)
    ("upper case word" . upcase-word)
    view-file
    view-file-other-frame
    view-file-other-window
    word-search-forward
    word-search-backward
    write-file
    write-region
    yank
    yank-pop
    yank-rectangle
    ("zap to character".  zap-to-char)

    ; mode-specific commands
    ; TBD activate these only when the mode is active
    vr-help-mode-command-list
    vr-info-mode-command-list
    ))

; Python mode voice commands

(defvar vr-python-command-list 
  '(
    ("start python shell" . py-shell)
    ("toggle python shells" . py-toggle-shells)
    ("execute region" . py-execute-region)
    ("execute string" . py-execute-string)
    ("execute buffer" . py-execute-buffer)
    ("import buffer" . py-execute-import-or-reload)
    ))

; Setnu voice commands

(defvar vr-setnu-command-list 
  '(
    ("toggle line numbers" . setnu-mode)
    ))

; Translate mode commands

(defvar vr-translate-mode-command-list 
  '(
    translate-mode
    ))

; VoiceGrip voice commands

(defvar vr-voicegrip-command-list 
  '(
	("translate line".  vg-translate-line)
	("translate region".  vg-translate-region)
	("translate file".  vg-translate-buffer)
	("correct symbol".  vg-correct-marked-sym)
	("compile source".  vg-compile)
	("compile project".  vg-compile-project)
	("clear dictionary".  vg-clear-dictionary)
	("find code forward".  vg-find-forward-statement)
	("find code backward".  vg-find-backward-statement)
	("abbreviation style".  vg-set-abbrev-style)
	("restart VoiceGrip".  vg-restart)
      ("start cache pad". vg-start-cache-pad)
      ("start CachePad". vg-start-cache-pad)
	("kill VoiceGrip".  vg-kill)
	))

; VR Mode voice commands

(defvar vr-vr-command-list
  '(
	("activate buffer" . vr-add-to-activation-list)
	("quit voice mode" . vr-quit)
	("quit emacs" . vr-quit-emacs)
))

; functions that support vr mode voice commands

(defun vr-quit-emacs ()
  "vr-quit then save-buffers-kill-emacs"
  (interactive)
  (vr-quit)
  (sleep-for 3)
  (save-buffers-kill-emacs)
  )

(defun vr-restart ()
  "restart VR mode, usually to install new VoiceCommands"
  (interactive)
  (vr-quit)
  (vr-mode 1)
  )

;; this is a utility function for maintaining this file.  mark a list
;; of voice commands in this buffer (excluding the surrounding parens)
;; and invoke this function to sort the list alphabetically.
;; sort-lines doesn't work because it does not ignore the (" that
;; occurs at the beginning of many entries.

(defun sort-voice-commands ()
  "sort VoiceCommands"
  (interactive)
  (sort-regexp-fields nil "^\\s-*\\((\"\\)?\\(\\w+\\).*$" "\\2"
	(region-beginning) (region-end))
  )

;;; end of vr-mode-commands.el
