;;; host-setup.el --- host-specific setups
;; Time-stamp: <2018-11-15 19:23:59 jcgs>
;; Author: John Sturdy <jcgs@cb1.com>

;; (require 'screen-setups)
;; actually that gets loaded later

(defun system-short-name ()
  "Return the first part of the name of the machine you are running on, as a string."
  (let* ((full-name (system-name))
         (dot (string-match "\\." full-name)))
    (if dot
	(substring full-name 0 dot)
      full-name)))

(defun system-domain-name ()
  "Return the domain of the machine you are running on, as a string."
  (let* ((full (system-name))
	 (short (system-short-name))
	 (short-len (length short)))
    (if (string= full short)
	"unknown"
      (if (string= short (substring full 0 short-len))
	  (substring full (1+ short-len))
	full))))

(defvar suggested-frame-position nil
  "A host setup function can set this if it wants to position the main
Emacs frame explicitly.")

(defun host-setup-micah ()
  "Setup specific to Micah, my home server."
  (setq jcgs-use-feedmail t)
  (cond
   ((eq window-system 'x)
    (setq screen-setups
	  '(("normal" "6x10" 102 39)
	    ("compact" "-*-lucidatypewriter-medium-r-*-*-8-*-*-*-*-*-*-*" 124 39))
	  default-screen-setup "normal"))
   (t nil)))

(defun scroll-lock-dummy ()
  "Ignore the characters sent when the KVM switch used on joel lets them through"
  (interactive)
  (read-event)
  (read-event)
  )

(defun host-is-reachable (host)
  "Test for whether HOST is reachable."
  (save-excursion
    (let ((ping-buffer (ping host)))
      (sleep-for 2)
      (set-buffer ping-buffer)
      (goto-char (point-min))
      (re-search-forward "bytes from" (point-max) t))))

(defun host-setup-joel ()
  "Setup specific to Joel, my desktop linux at work."
  (setq vr-host "mayo.csis.ul.ie"
	vr-port 50505
	;; (string-to-number (read-from-minibuffer "Port for vr: " "0"))
	;; vr-command ""
	vr-win-class "Emacs"
	)
  (global-set-key [key-20] 'scroll-lock-dummy)
  (setq browse-url-browser-function 'browse-url-gnome-moz)
  (let ((display (getenv "DISPLAY")))
    (cond
     ((and (eq window-system 'x)
	   (stringp display)
	   (string= display ":0.0"))
      (setq screen-setups
	    '(("normal" "-Adobe-Courier-Medium-R-Normal--17-120-100-100-M-100-ISO8859-1"
	       134 64			; 98 46
	       )
	      ("quite compact" "6x10"
	       225 96			; 160 69
	       )
	      ("compact" "-*-lucidatypewriter-medium-r-*-*-8-*-*-*-*-*-*-*"
	       269 96			;  194 69
	       ))
	    default-screen-setup "normal")
      (set-face-font 'mode-line "6x10")
      (set-face-background 'mode-line "brown")
      (set-face-foreground 'mode-line "white"))
     (t nil))))

(defun host-setup-glg ()
  (host-setup-joel))

(defun host-setup-knoppix ()
  "Setup for running Oralux/Knoppix."
  (if (featurep emacspeak)
      (message "I appear to be running on Oralux")
    (message "I appear to be running on Knoppix")))

;; use x-list-fonts to explore the possible fonts

(defun host-setup-hosea ()
  "Setup specific to Hosea, my home voice-recognition front-end/workstation."
  ;; if my usb key has some elisp to run, run it
  (require 'removable-media)

  (defun mount-usb-keys ()
    "Mount my usb keys."
    (interactive)
    (mapcar 'mount-removable-top-level-directory
	    '("watch" "personal" "usb3" "usb4")))

  (mount-usb-keys)

  (setq old-voice t)

  (when (and (eq window-system 'w32)
	     (= emacs-major-version 23)
	     (= emacs-minor-version 0))
    ;; this one is a bit of a hack!
    ;; I'm trying to stop the redisplay problems

    (if t
	(add-hook 'post-command-hook 'recenter)
      (add-hook 'post-command-hook 'redisplay)))

  (cond
   ((memq window-system '(w32 win32))
    (setq suggested-frame-position '(0 100)
	  screen-on-compact-setting t
	  screen-setups
	  (if screen-on-compact-setting
	      '(("normal" "6x10" 177 55)
		;; ("big mono" "12x16" 80 24)
		("Kelt" "-outline-Kelt UNICODE-bold-r-normal-normal-*-*-96-96-p-*-iso10646-1" 207 54)
		("Gaeilge" "-outline-Gaeilge UNICODE-normal-r-normal-normal-*-*-96-96-p-*-iso10646-1": 111 40)
		("Rudhraigheact" "-outline-Rudhraigheacht UNICODE-normal-r-normal-normal-*-*-96-96-p-*-iso10646-1" 177 44)
		("Everson 36-pt" "-outline-Everson Mono Unicode-normal-r-normal-normal-48-360-96-96-c-*-iso10646-1" 45 10)
		("Everson 24-pt" "-outline-Everson Mono Unicode-normal-r-normal-normal-35-262-96-96-c-*-iso10646-1" 69 20)
		("Everson 18-pt" "-outline-Everson Mono Unicode-normal-r-normal-normal-24-180-96-96-c-*-iso10646-1" 88 25)
		("Everson 12-pt"  "-outline-Everson Mono Unicode-normal-r-normal-normal-16-120-96-96-c-*-iso10646-1" 138 40)
		("Everson 10-pt"  "-outline-Everson Mono Unicode-normal-r-normal-normal-13-97-96-96-c-*-iso10646-1" 177 52)
		("Everson 8-pt"  "-outline-Everson Mono Unicode-normal-r-normal-normal-11-82-96-96-c-*-iso10646-1" 207 62)
		("compact" "-*-Terminal-normal-r-*-*-8-60-*-*-c-*-*-oem-" 206 107)
		("more compact" "-raster-Small Fonts-normal-r-normal-normal-9-67-96-96-p-50-iso10646-1" 248 80)
		("tiny" "-raster-Small Fonts-normal-r-normal-normal-8-60-96-96-p-40-iso10646-1" 310 88)
		("just legible" "-raster-Small Fonts-normal-r-normal-normal-7-52-96-96-p-40-iso10646-1"  311  111)
		("too small to read" "-raster-Small Fonts-normal-r-normal-normal-5-37-96-96-p-30-iso10646-1" 414 148)
		("chunky" "-outline-Bitstream Vera Sans Mono-bold-r-normal-normal-*-*-96-96-c-*-iso10646-1" 123 45)
		("pretty big proportional" "-raster-MS Sans Serif-normal-r-normal-normal-32-240-96-96-p-160-iso10646-1"  78 24)
		;; ("rather large" "-raster-System-bold-r-normal-normal-13-97-96-96-p-70-iso8859-1"  78 22)
		("ugly old terminal" "-raster-Terminal-normal-r-normal-normal-12-90-96-96-c-160-ms-oem")

		)
	    '(("normal" "6x10" 102 38)
	      ("compact" "-*-Terminal-normal-r-*-*-8-60-*-*-c-*-*-oem-" 131 70)
	      ("more compact" "-raster-Small Fonts-normal-r-normal-normal-9-67-96-96-p-50-iso10646-1" 151 51)
	      ("tiny" "-raster-Small Fonts-normal-r-normal-normal-8-60-96-96-p-40-iso10646-1" 190 57)
	      ("just legible" "-raster-Small Fonts-normal-r-normal-normal-7-52-96-96-p-40-iso10646-1" 190 71)
	      ("too small to read" "-raster-Small Fonts-normal-r-normal-normal-5-37-96-96-p-30-iso10646-1" 253  94)

	      ))
	  default-screen-setup "chunky"
	  sidebrain-frame-width-fudge-factor 23)
    (set-face-font 'mode-line "-raster-Terminal-normal-r-normal-normal-12-90-96-96-c-50-ms-oemlatin")
    (set-face-background 'mode-line "brown")
    (set-face-foreground 'mode-line "white")
    )
   (t nil))
  (setq jcgs-use-feedmail t
	;; this machine doesn't have the fonts for everything
	;; mulvoc-dictionaries-pattern "\\(general\\|basic\\).csv$"
	shell-file-name "c:/fsf/bin/bash.exe"
	explicit-shell-file-name "c:/fsf/bin/bash.exe")
  (add-hook 'after-init-hook
	    (lambda ()
	      ;; these seem to be getting overwritten elsewhere
	      (setq shell-file-name "c:/fsf/bin/bash.exe"
		    explicit-shell-file-name "c:/fsf/bin/bash.exe")
	      ;; need to do this after loading the usual setup
	      (setq mulvo-displayed-languages
		    '(GLI
		      NRR SWD
		      FIN
		      DUT GER
		      pinyin romaji)) ; this machine doesn't have all the fonts

	      (require 'type-break-extras)
	      ))
  ;; (add-hook 'type-break-start-break-hook 'type-break-light-fire)
  )

(defun host-setup-mayo ()
  "Setup specific to Mayo, my work voice-recognition front-end."
  ;; based on hosea for now
  (setq mayo-original-font (cdr (assoc 'font (frame-parameters))))
  (cond
   ((eq window-system 'w32)
    (setq screen-setups
	  '(("dreadful" "6x10" 100 35)
	    ("chunky" "-outline-Bitstream Vera Sans Mono-bold-r-normal-normal-*-*-96-96-c-*-iso10646-1" 96 34)
	    ("normal" "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1" 124 41)
	    ("compact" "-*-Terminal-normal-r-*-*-8-60-*-*-c-*-*-oem-" 131 70)
	    ("more compact" "-raster-Small Fonts-normal-r-normal-normal-9-67-96-96-p-50-iso10646-1" 151 51)
	    ("tiny" "-raster-Small Fonts-normal-r-normal-normal-8-60-96-96-p-40-iso10646-1" 190 57)
	    ("just legible" "-raster-Small Fonts-normal-r-normal-normal-7-52-96-96-p-40-iso10646-1" 190 71)
	    ("too small to read" "-raster-Small Fonts-normal-r-normal-normal-5-37-96-96-p-30-iso10646-1" 253  94)

	    )
	  default-screen-setup "normal"))
   (t nil)))

(defun host-setup ()
  "Setup specific to each host."
  (interactive)				; for testing really
  (let* ((hostname (downcase (system-short-name)))
	 (hostsym (intern (concat "host-setup-" hostname))))
    (if (fboundp hostsym)
	(progn
	  (message "Running host setup %S" hostsym)
	  (funcall hostsym)
	  (if suggested-frame-position
	      (apply 'set-frame-position (selected-frame) suggested-frame-position))))))

(host-setup)

;;; host-setup.el ends here
