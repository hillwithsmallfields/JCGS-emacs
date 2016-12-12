;;;; general-voice.el
;;; Time-stamp: <2007-03-18 18:54:51 jcgs>

(provide 'general-voice)

(defvar vr-general-commands
  '(("push mark" . set-mark-command)
    yank
    pop-mark
    find-tag
    font-lock-mode
    ("save all" . save-all-buffers-no-ask)
    ("voice help" . vr-command-help)
    ("voice groups" . vr-grouped-command-help)
    ("show spoken command" . vr-whereis)
    ("show spoken keys" . vr-whereis-key)
    ("mode help" . vr-modal-command-help)
    ("eval buffer" . eval-current-buffer)
    ("copy region" . kill-ring-save)
    ("delete region" . kill-region)
    ("comment" . indent-for-comment)
    ("remove white space" . delete-horizontal-space)
    ("remove blank lines" . delete-blank-lines)

    recenter
    ("refresh" . recenter)


    downcase-word
    downcase-region
    upcase-word
    upcase-region
    capitalize-word
    capitalize-region

    comment-region

    just-one-space
    ("delete whitespace" . delete-horizontal-space)
    delete-trailing-whitespace

    insert-parentheses
    enclose-with-parentheses

    ("new environment" . LaTeX-environment) ; todo: make a flexi-choose version of this

    narrow-to-region
    widen

    toggle-read-only
    ("read only" . read-only)
    writable
    undo
    ;; if we just call this "one window" it clashes with "one", in continuous-commands mode:
    
    ("other place" . exchange-point-and-mark)

    bury-buffer
    ("just one window" . delete-other-windows)
    ("other buffer" . switch-to-other-buffer)
    find-file-at-point			; from emacs/filenames-in-buffers.el
    ("find this file" . find-file-at-point)
    ("in other window find this file" . find-file-at-point-other-window)
    top-level

    make-directory
    copy-file
    delete-file
    ("delete this file" . delete-file-at-point-no-inspection)

    toggle-stack-trace

    ("of" . ignore-noise-word-as-command)
    ("the" . ignore-noise-word-as-command)
    ("with" . ignore-noise-word-as-command)
    )
  "Some general emacs voice commands.")

(defun switch-to-other-buffer ()
   "Switch to the most recently used buffer other than this one."
   (interactive)
   (switch-to-buffer (other-buffer)))

(defun enclose-with-parentheses ()
  "Enclose following ARG sexps in parentheses.  Leave point after open-paren."
  (interactive)
  (insert-parentheses 1))

(defun read-only ()
  "Make the current buffer read-only."
  (interactive)
  (toggle-read-only 1))

(defun writable ()
  "Make the current buffer writable."
  (interactive)
  (toggle-read-only -1))

(defun ignore-noise-word-as-command ()
  "If run as a command from the voice system, do nothing.
If run within embedded-commands, insert the word.
This covers the words which the speech system mistakenly generates
during pauses."
  (interactive)
  (if embedded-command-running
      (insert "%" vr-reading-string "%")
    (message "Ignored probable spurious noise")))

;;; end of general-voice.el
