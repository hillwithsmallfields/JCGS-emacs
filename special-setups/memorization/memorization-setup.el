;;;; Setup for memorization
;;; Time-stamp: <>

(setq user-emacs-directory (substitute-in-file-name "$HOME/Dropbox/emacs/"))

(load-file (expand-file-name "config/use-flashcard.el" user-emacs-directory))
