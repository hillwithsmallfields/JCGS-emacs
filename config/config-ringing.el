;;;; Config for emacs-ringing
;; Time-stamp: <2018-04-27 16:38:02 jcgs>
;; I'm resuming using this after a long gap, and have probably lost my old configuration for it

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/lisp"))
(setq method-library:directory (substitute-in-file-name "$GATHERED/ringing/method-libraries"))

(require 'ringing)


