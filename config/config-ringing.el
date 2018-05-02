;;;; Config for emacs-ringing
;; Time-stamp: <2018-04-30 14:12:52 jcgs>
;; I'm resuming using this after a long gap, and have probably lost my old configuration for it

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/lisp"))
(add-to-list 'safe-local-variable-values (cons 'ringing-suggested-layout "portrait"))
(setq method-library:directory (substitute-in-file-name "$GATHERED/ringing/method-libraries")
      ;; ringing:postscript-styles-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/postscript/ps-styles")
      ;; ringing:postscript-title-styles-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/postscript/ps-title-styles")
      ;; ringing:postscript-grid-styles-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/postscript/ps-grid-styles")
      )

(require 'ringing)


