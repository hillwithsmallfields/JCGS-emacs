;;;; Config for emacs-ringing
;; Time-stamp: <2018-09-10 09:00:55 jcgs>

;; I'm resuming using this after a long gap, and have probably lost my old configuration for it

(let ((ringing-lisp-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/lisp")))
  (when (file-directory-p ringing-lisp-dir)
    (add-to-list 'load-path ringing-lisp-dir)
    (add-to-list 'safe-local-variable-values (cons 'ringing-suggested-layout "portrait"))
    (setq method-library:directory (substitute-in-file-name "$GATHERED/ringing/method-libraries")
          ;; ringing:postscript-styles-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/postscript/ps-styles")
          ;; ringing:postscript-title-styles-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/postscript/ps-title-styles")
          ;; ringing:postscript-grid-styles-dir (substitute-in-file-name "$OPEN_PROJECTS/emacs-ringing/postscript/ps-grid-styles")
          )
    (require 'ringing)))




