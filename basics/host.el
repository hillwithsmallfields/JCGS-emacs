;;; host.el --- host-specific setups
;; Time-stamp: <2018-07-05 13:51:10 jcgs>
;; Author: John Sturdy <jcgs@cb1.com>

;; todo: move these to ../host-setup.el

(defconst my-voice-systems
  '(("hosea" . windows-nt)
    "joel.csis.ul.ie"
    "glg.csisdmz.ul.ie"
    "mayo")
  "List of system names on which emacs should turn VR on on startup.")

(defun on-voice-system ()
  "Return whether we are on a voice system."
  (let ((name (downcase (system-name)))
	(pair nil)
	)
  (or (not (boundp 'my-voice-systems))
      (null my-voice-systems)
      (member name my-voice-systems)
      (and (setq pair (assoc name my-voice-systems))
	   (eq system-type (cdr pair))))))

(defalias 'voice-input-p 'on-voice-system)

(defconst my-pedals-systems
  '("micah"
    "hosea"
    "hosea.cnocnagortini"
    "hosea.cb1.com"
    ;; "joel.csis.ul.ie"
    ;; "glg.csisdmz.ul.ie"
    ;; "ezekiel.csis.ul.ie"
    ;; "blasket.cam.xci-test.com"
    ;; "mayo"
    "e104804-lin.cambridge.arm.com"
    "isaiah.cam.xci-test.com"
    "duralium"
    )
  "List of system names on which Emacs should load the pedal software on startup.")

(defun pedals-p ()
  "Return whether we have pedals fitted."
  (member (downcase (system-name)) my-pedals-systems))

(setq vm-host (member (system-name) '("micah"
				      "joel.csis.ul.ie"
				      "glg.csisdmz.ul.ie"
				      ))
      w3-host (member (system-name) '("micah"
				      ;; "joel.csis.ul.ie"
				      ))
      mulvo-host (not (string= (system-name) "MAYO"))
      )

(defun w3-host-p ()
  "Return whether this emacs is on a machine that I normally use w3 on the net from."
  (member (system-name) '("micah"
			  "joel.csis.ul.ie"
			  "glg.csisdmz.ul.ie"
			  )))

(defvar employer-domain "grapeshot"
  "The domain of my employer.
Used for determining whether this Emacs is running at work.")

(defvar employer-laptop "duralium"
  "The name of my work laptop.
This lets my configuration recognize I'm at work even when the
machine is on some other network connection.")

(defun at-work-p ()
  "Return whether I'm on a work machine."
  (or (string-match employer-domain
		    (system-name))
      (string-match employer-laptop
		    (system-name))))

(defun at-home-p ()
  "Return whether I'm on a home system.
Currently looks for whether one of my Raspberry Pis is reachable
through a local address."
  (string-match "1 packets transmitted, 1 received"
                (shell-command-to-string "ping -W 1 -c 1 fragaria")))

(defun on-small-machine ()
  "Whether I'm on some small machine, such as a laptop."
  (catch 'found
    (dolist (pattern '("ezra" "elijah"))
      (when (string-match pattern (system-name))
	(throw 'found t)))
    nil))

;;; host.el ends here
