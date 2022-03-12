;;; Time-stamp: <2021-11-14 18:34:18 jcgs>

(setq stack-trace-on-error t
      message-log-max t)

(message "COMMON=%S GATHERED=%S" (getenv "SYNCED") (getenv "GATHERED"))

(unless (getenv "SYNCED")
  (setenv "COMMON" "i:/common"))
(unless (getenv "GATHERED")
  (setenv "GATHERED" "j:/users/jcgs/library"))

(message "COMMON=%S GATHERED=%S" (getenv "SYNCED") (getenv "GATHERED"))

(add-to-list 'load-path (expand-file-name "appearance" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "basics" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "natural-language" user-emacs-directory))
(load-library "add-lispdir")
(load-library "modes")
(load-library "clo-gaelach")

(add-to-list 'load-path (expand-file-name "mode-setups" user-emacs-directory))
(load-library "setup-xml")

(global-font-lock-mode 1)

(find-file (expand-file-name "special-setups/just-xml-gaelach/just-xml-gaelach-setup.el" user-emacs-directory))
(find-file (substitute-in-file-name "$GATHERED/emacs/psgml-1.3.2/psgml-parse.el"))
(find-file (expand-file-name "mode-setups/setup-xml.el" user-emacs-directory))
(find-file (substitute-in-file-name "$SYNCED/projects/focloir/sample-files/ab.xmlex"))
