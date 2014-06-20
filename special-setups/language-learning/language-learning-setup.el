;;;; JCGS' setup for language learning
;;; Time-stamp: <2014-01-05 13:40:15 jcgs>

(setq user-emacs-directory (substitute-in-file-name "$HOME/Dropbox/emacs/"))

(add-to-list 'load-path (substitute-in-file-name "$HOME/Dropbox/emacs/basics"))
(load-library "jcgs-common-setup")

(load-file (expand-file-name "config/use-flashcard.el" user-emacs-directory))
;;; TODO: include the fix for array index limiting

(find-file (substitute-in-file-name "~/Dropbox/languages/languages.deck"))

