;;; host.el --- host-specific setups
;; Time-stamp: <2014-10-20 15:27:21 johstu01>
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

(defun at-work ()
  "Return whether I'm on a work machine."
  (string-match "arm.com"
		(system-name)))

(defun on-small-machine ()
  "Whether I'm on some small machine, such as a laptop."
  (string-match "ezra" (system-name)))

;;; host.el ends here
