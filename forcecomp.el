(setq load-path `(;; "emacs/my-extensions-to-packages/bbdb"
		  "emacs/my-extensions-to-packages/type-break"
		  "emacs/my-extensions-to-packages/vm"
		  "emacs/my-extensions-to-packages/voice"
		  "/usr/local/share/emacs/20.7/site-lisp"
		  "/usr/local/share/emacs/20.7/site-lisp/egg"
		  "/usr/local/share/emacs/20.7/site-lisp/emu"
		  "/usr/local/share/emacs/20.7/site-lisp/egg/egg"
		  "/usr/local/share/emacs/20.7/site-lisp/egg/its"
		  "/usr/local/share/emacs/site-lisp"
		  "/usr/local/share/emacs/site-lisp/apel"
		  "/usr/local/share/emacs/site-lisp/color-mate"
		  "/usr/local/share/emacs/site-lisp/elib"
		  "/usr/local/share/emacs/site-lisp/flim"
		  "/usr/local/share/emacs/site-lisp/gnus"
		  "/usr/local/share/emacs/site-lisp/pcl-cvs"
		  "/usr/local/share/emacs/site-lisp/psgml"
		  "/usr/local/share/emacs/site-lisp/semi"
		  "/usr/local/share/emacs/site-lisp/sml-mode"
		  "/usr/local/share/emacs/site-lisp/yatex"
		  "/usr/local/share/emacs/site-lisp/color-mate/contrib"
		  "/usr/local/share/emacs/site-lisp/color-mate/kanakan-cursor"
		  "/usr/local/share/emacs/site-lisp/color-mate/theme"
		  "/usr/local/share/emacs/20.7/leim"
		  "/usr/local/share/emacs/20.7/lisp"
		  "/usr/local/share/emacs/20.7/lisp/textmodes"
		  "/usr/local/share/emacs/20.7/lisp/progmodes"
		  "/usr/local/share/emacs/20.7/lisp/play"
		  "/usr/local/share/emacs/20.7/lisp/mail"
		  "/usr/local/share/emacs/20.7/lisp/language"
		  "/usr/local/share/emacs/20.7/lisp/international"
		  "/usr/local/share/emacs/20.7/lisp/gnus"
		  "/usr/local/share/emacs/20.7/lisp/emulation"
		  "/usr/local/share/emacs/20.7/lisp/emacs-lisp"
		  "/usr/local/share/emacs/20.7/lisp/calendar"
		  ,(expand-file-name "handsfree" user-emacs-directory)
		  ,(expand-file-name "autoload" user-emacs-directory)
		  ,(expand-file-name "basics" user-emacs-directory)
		  ,(expand-file-name "editing" user-emacs-directory)
		  ,(expand-file-name "email" user-emacs-directory)
		  ,(expand-file-name "misc" user-emacs-directory)
		  ,(expand-file-name "mode-setups" user-emacs-directory)
		  ,(expand-file-name "persistence" user-emacs-directory)
		  ,(expand-file-name "playpen" user-emacs-directory)
		  ,(expand-file-name "startup" user-emacs-directory)
		  ,(expand-file-name "voice" user-emacs-directory)
		  ,(expand-file-name "webstuff" user-emacs-directory)
		  "$GATHERED/emacs/bbdb/bbdb-2.00/lisp"
		  "$GATHERED/emacs/html-helper-mode"
		  ;; "$GATHERED/emacs/vm/vm-6.79/"
		  "$GATHERED/emacs/voice/vrmode"
		  "$GATHERED/emacs/w3/w3-4.0pre.46/lisp"))
(byte-recompile-directory "$COMMON/emacs" 0 )
(switch-to-buffer "*Compile-Log*")
(goto-char (point-max))
