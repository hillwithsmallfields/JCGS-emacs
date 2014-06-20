;;; gm862.el --- stuff for doing python on the Telit GM862

;; Copyright (C) 2012, 2013, 2014  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: hardware, terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The GM862 requires code downloading to be done at 115200 baud, with
;; hardware flow control.  This code tries to manage the switching
;; between that and normal "terminal" interactions.

;;; Code:

(defvar gm862-tty-name "/dev/ttyUSB0"
  "The name of the device that connects through to the GM862.")

(defvar gm862-initial-baud-rate 
  115200 
  ;; 19200
  "The initial baud rate for talking to the GM862.")

(defvar gm862-download-chunk-interval 0.25
  "How many seconds to wait between chunks")

(defvar gm862-process nil
  "The serial process for communicating with the GM862.")

;; (require 'term)

;; (define-key term-raw-map [ s-x ] 'ctl-x-map)

(defvar gm862-download-prompt-received nil
  "Whether we have yet seen the prompt.")

(defvar gm862-download-error-received nil
  "Whether we have yet seen an error.")

(defvar gm862-filter-time nil
  "When the filter last ran.")

(defvar gm862-cmux-active nil
  ;; will work only in SELINT 2
  "Whether we are using CMUX protocol.")

(defun gm862-process-filter (proc str)
  "Filter for text coming back from the GM862.
Argument PROC is the process.
Argument STR is the string it produced."
  (if gm862-cmux-active
      (gm862-cmux-process-filter proc str)
      (gm862-plain-process-filter proc str)))

(defvar gm862-catch-output nil
  "Whether to catch the GM862 output.")

(defvar gm862-caught-output nil
  "Lines sent by the GM862 while gm862-catch-output is non-nil.")

(defun gm862-plain-process-filter (proc str)
  "Filter for text coming back from the GM862.
Argument PROC is the process.
Argument STR is the string it produced."
  (setq gm862-filter-time (float-time))
  (when gm862-catch-output
    (push str gm862-caught-output))
  (when (string-match ">>>" str)
    (message "Got file download prompt")
    (setq gm862-download-prompt-received t))
  (when (string-match "ERROR" str)
      (setq gm862-download-error-received t))
  (term-emulate-terminal proc str))

(defvar gm862-cmux-log-replies nil
  "Whether to log the CMUX stream from the GM862.")

(defun gm862-cmux-log-reply (str)
  "Log STR."
  (save-excursion
    (set-buffer (get-buffer-create "*cmux*"))
    (goto-char (point-max))
    (insert "ASCII: "
	    (mapconcat (function
			(lambda (byte)
			  (if (and (>= byte 32)
				   (< byte 127))
			      (format "%c " byte)
			    "  ")))
		       str
		       " ")
	    "\nHex:   "
	    (mapconcat (function
			(lambda (byte)
			  (format "%02x" byte)))
		       str
		       " ")
	    "\n")))

(defun gm862-cmux-process-filter (proc str)
  "Filter for CMUX packets coming back from the GM862.
Argument PROC is the process.
Argument STR is the string it produced."
  ;; packet format;
  ;;  flag address control length [ information ] fcs flag
  ;;  flag is 0xf9
  ;;  address is 1,c/r,port where port is 1-4, 0 being the control port
  (when gm862-cmux-log-replies
    (gm862-cmux-log-reply str))
  (let* ((initial-flag (aref str 0))
	 (address-byte (aref str 1))
	 (address (logand address-byte 63))
	 (command-response (logand address-byte 64))
	 (control-byte (aref str 2))
	 (p/f (logand control-byte 8))
	 (other-control (logand control-byte 247))
	 (length-byte-1 (aref str 3))
	 (length-byte-2 (aref str 4))
	 (fred (message "flag %x, command-response %d, address %d, p/f %x, other-control %x, length1 %d, length2 %d"
			initial-flag command-response address p/f other-control length-byte-1 length-byte-2))
	 (contents (substring str 4 -2))
	 )
    ;; channels:
    ;;   0: control channel
    ;;   1: virtual port #1
    ;;   2: virtual port #2
    ;;   3: virtual port #3
    ;;   4: reserved for Python debug
    (message "flag %x, command-response %d, address %d, p/f %x, other-control %x, length1 %d, length2 %d, contents \"%s\""
	     initial-flag command-response address p/f other-control length-byte-1 length-byte-2 contents)))

(defun gm862-send-bytes (&rest bytes)
  "Send BYTES to the GM862."
  (process-send-string gm862-process
		       (apply 'string bytes)))

(defconst gm862-crc-table
  [#x00 #x91 #xE3 #x72 #x07 #x96 #xE4 #x75 #x0E #x9F #xED #x7C #x09 #x98 #xEA #x7B
   #x1C #x8D #xFF #x6E #x1B #x8A #xF8 #x69 #x12 #x83 #xF1 #x60 #x15 #x84 #xF6 #x67
   #x38 #xA9 #xDB #x4A #x3F #xAE #xDC #x4D #x36 #xA7 #xD5 #x44 #x31 #xA0 #xD2 #x43
   #x24 #xB5 #xC7 #x56 #x23 #xB2 #xC0 #x51 #x2A #xBB #xC9 #x58 #x2D #xBC #xCE #x5F
   #x70 #xE1 #x93 #x02 #x77 #xE6 #x94 #x05 #x7E #xEF #x9D #x0C #x79 #xE8 #x9A #x0B
   #x6C #xFD #x8F #x1E #x6B #xFA #x88 #x19 #x62 #xF3 #x81 #x10 #x65 #xF4 #x86 #x17
   #x48 #xD9 #xAB #x3A #x4F #xDE #xAC #x3D #x46 #xD7 #xA5 #x34 #x41 #xD0 #xA2 #x33
   #x54 #xC5 #xB7 #x26 #x53 #xC2 #xB0 #x21 #x5A #xCB #xB9 #x28 #x5D #xCC #xBE #x2F
   #xE0 #x71 #x03 #x92 #xE7 #x76 #x04 #x95 #xEE #x7F #x0D #x9C #xE9 #x78 #x0A #x9B
   #xFC #x6D #x1F #x8E #xFB #x6A #x18 #x89 #xF2 #x63 #x11 #x80 #xF5 #x64 #x16 #x87
   #xD8 #x49 #x3B #xAA #xDF #x4E #x3C #xAD #xD6 #x47 #x35 #xA4 #xD1 #x40 #x32 #xA3
   #xC4 #x55 #x27 #xB6 #xC3 #x52 #x20 #xB1 #xCA #x5B #x29 #xB8 #xCD #x5C #x2E #xBF
   #x90 #x01 #x73 #xE2 #x97 #x06 #x74 #xE5 #x9E #x0F #x7D #xEC #x99 #x08 #x7A #xEB
   #x8C #x1D #x6F #xFE #x8B #x1A #x68 #xF9 #x82 #x13 #x61 #xF0 #x85 #x14 #x66 #xF7
   #xA8 #x39 #x4B #xDA #xAF #x3E #x4C #xDD #xA6 #x37 #x45 #xD4 #xA1 #x30 #x42 #xD3
   #xB4 #x25 #x57 #xC6 #xB3 #x22 #x50 #xC1 #xBA #x2B #x59 #xC8 #xBD #x2C #x5E #xCF
   ]
  "The CRC table from the document describing CMUX.")

(defun gm862-calculate-fcs (string)
  "Calculate the fcs of STRING."
  (let ((len (length string))
	(fcs #xff)
	(i 0))
    (while (< i len)
      (setq fcs (aref gm862-crc-table
		      (logand #xff
			      (logxor fcs
				      (aref string i))))
	    i (1+ i)))
    (- #xff fcs)))

(defconst gm862-SABM #x2f
  "Set Asynchronous Balanced Mode.")

(defconst gm862-UA #x63
  "Unnumbered Acknowledgement.")

(defconst gm862-DM #x0f
  "Disconnected Mode.")

(defconst gm862-DISC #x43
  "Disconnect.")

(defconst gm862-UIH #xef
  "Unnumbered Information with Header check.")

(defun gm862-send-packet (address control bytes)
  "Send a packet to ADDRESS with CONTROL on the GM862."
  (let* ((preamble (string #xf9
			   (logor #x03 (lsh address -2))
			   (logor control #x10)
			   (length bytes)))
	 (postamble (gm862-calculate-fcs string) #xf9))
    (process-send-string gm862-process
			 (concat preamble bytes postamble))))

(defvar gm862-cmux-process-old-details nil
  "Port control information to restore when switching out of CMUX.")
		    
(defun gm862-cmux-begin ()
  "Switch into CMUX mode."
  (interactive)
  (when gm862-cmux-active
    (error "CMUX already active"))
  (setq gm862-cmux-process-old-details (process-contact gm862-process t))
  (gm862-command "AT#SELINT=2")
  (gm862-command "ATE0V1&K3&D2")
  (gm862-command "AT+CMUX=0")
  (serial-process-configure :port gm862-tty-name
			    :flowcontrol 'hw)
  ;; (gm862-send-packet 0 gm862-SABM )
  (gm862-send-bytes #xf9		; flag
		    #x03		; address = 0
		    #x3f		; control = SABM
		    #x01		; length
		    #x1c		; CRC
		    #xf9)		; flag
  (gm862-send-bytes #xf9		; flag
		    #x07		; address = 0
		    #x3f		; control = SABM
		    #x01		; length
		    #xde		; CRC
		    #xf9)		; flag
  (gm862-send-bytes #xf9		; flag
		    #x03		; address = 0
		    #xef		; control = UIH
		    #x09		; length
		    #xe1		; 
		    #x05		; 
		    #x07		; type = format
		    #x0c		; FC=0 RTS=1 DTR=1
		    #xfb		; CRC
		    #xf9)		; flag
  (setq gm862-cmux-active t))

(defun gm862-cmux-control-command ()
  "Send a command on the cmux control channel."
  (gm862-send-bytes #xf9 ))

(defun gm862-cmux-end ()
  "Switch out of CMUX mode."
  (interactive)
  (unless gm862-cmux-active
    (error "CMUX not active"))
  (gm862-send-bytes #xf9 )
  (serial-process-configure :port gm862-tty-name
			    :flowcontrol (plist-get gm862-cmux-process-old-details :flowcontrol))
  (setq gm862-cmux-active nil))

;; +CMUX - Multiplexing Mode
;; SELINT 2
;; AT+CMUX=
;; Set command is used to enable/disable the GSM 07.10 multiplexing protocol
;; <mode>
;; control channel.
;; [,<subset>]
;; Parameters:
;; <mode> multiplexer transparency mechanism
;; 0 - basic option; it is currently the only supported value.
;; <subset>
;; 0 - UIH frames used only; it is currently the only supported value.
;; Reproduction forbidden without Telit Communications S.p.A. written authorization - All Rights Reserved
;; page 84 of 566
;; AT Commands Reference Guide
;; 80000ST10025a Rev. 7 – 2010-05-07
;; +CMUX - Multiplexing Mode
;; SELINT 2
;; Note: after entering the Multiplexed Mode an inactive timer of five seconds
;; starts. If no CMUX control channel is established before this inactivity timer
;; expires the engine returns to AT Command Mode
;; Note: all the CMUX protocol parameter are fixed as defined in GSM07.10
;; and cannot be changed.
;; AT+CMUX?
;; AT+CMUX=?
;; Reference
;; 3.5.4.1.8.
;; Note: the maximum frame size is fixed: N1=128
;; Read command returns the current value of <mode> and <subset>
;; parameters, in the format:
;; +CMUX: <mode>,<subset>
;; Test command returns the range of supported values for parameters
;; <mode> and <subset>.

(defun gm862-cmux-send-string (process channel string)
  "To PROCESS's CHANNEL, send STRING."
  (process-send-string process
		       (format "%c%c" 
			       flag
			       (logor 128
				   64
				   channel) 
			       control	; todo: construct this
			       (length string) ; todo: what format?
			       string)))

(defvar gm862-stopwatch-times nil
  "The times taken to start running programs.")

(defun gm862-stopwatch ()
  "Time how long it is before some output appears."
  (interactive)
  (setq gm862-filter-time nil)
  (let ((started (float-time))
	(ended nil)
	(stopwatch-format (if gm862-stopwatch-times
			      (format "waiting for output; %%d seconds (%d average)"
				  (/ (apply '+ gm862-stopwatch-times)
				     (length gm862-stopwatch-times)))
			    "waiting for output; %d seconds")))
    (while (not (setq ended gm862-filter-time))
      (message stopwatch-format
	       (- (float-time) started))
      (sit-for 1))
    (let ((time (- ended started)))
      (push time gm862-stopwatch-times)
      (message "Took %d seconds to produce output"
	       time)
      time)))

(defun gm862-term ()
  "Set up a terminal to the GM862."
  (interactive)
  (setq gm862-process
	(get-buffer-process
	 (serial-term gm862-tty-name gm862-initial-baud-rate)))
  (set-process-filter gm862-process 'gm862-process-filter))

(defun ensure-gm862-term ()
  "Ensure there is a connection to the GM862."
  (unless (and (processp gm862-process)
	       (process-live-p gm862-process))
    (gm862-term)))

(defun gm862-show-output ()
  "Show the latest output from the GM862."
  (let ((original-window (selected-window)))
    (set-buffer
     (pop-to-buffer 
      (process-buffer
       (get-process gm862-tty-name))))
    (goto-char (point-max))
    ;; (recenter -1)
    (select-window original-window)))

(defun gm862-erase-output ()
  "Erase the output from the GM862."
  (interactive)
  (set-buffer
   (process-buffer
    (get-process gm862-tty-name)))
  (erase-buffer))

(defvar gm862-scripts-alist nil
  "Alist of names under which we've sent files to the GM862
mapped to their filenames on the host system.")

(defvar gm862-latest-mentioned-script nil
  "The script most recently chosen in interactive input.")

(defun gm862-prompt-for-script-name (prompt)
  "Prompt for a script name, using PROMPT."
  (setq gm862-latest-mentioned-script
	(completing-read prompt
			 gm862-scripts-alist
			 nil
			 'confirm
			 nil
			 nil
			 gm862-latest-mentioned-script)))

(defun gm862-send-file (file gm862-filename)
  "Send FILE to the GM862.
Argument GM862-FILENAME is the name to use on the GM862."
  (interactive
   (let* ((full-filename (read-file-name "File to send to GM862: "))
	  (filename-prompt "File name to use on GM862: ")
	  (gm862-filename nil))
     (while (or (not (stringp gm862-filename))
		(> (length gm862-filename) 16))
       (setq gm862-filename (read-string filename-prompt
					 (file-name-nondirectory full-filename))
	     filename-prompt "File name to use on GM862 (up to 16 characters): "))
     (list full-filename gm862-filename)))
  (ensure-gm862-term)
  (setq gm862-download-prompt-received nil
	gm862-download-error-received nil)
  (save-window-excursion
    (find-file file)
    ;; Rather than fully trusting the flow control, we send the file a
    ;; line at a time, with slight pauses.
    (let* ((file-as-lines (split-string
			   (buffer-substring-no-properties (point-min)
							   (point-max))
			   "[\n\r]" t))
	   (chunks (length file-as-lines))
	   (data-size (apply '+ (* 2	; CR, LF
				   chunks)
			     (mapcar 'length file-as-lines)))
	   (old-state (process-contact gm862-process t))
	   (hidden nil)
	   (progress-format (format "%%d/%d" chunks)))
      ;; (message "Old state is %S" old-state)
      ;; (message "data size is %d, as %d lines" data-size chunks)
      (process-send-string gm862-process
			   (format "AT#WSCRIPT=\"%s\",%d,%d\r\n"
				   gm862-filename
				   data-size
				   (if hidden 1 0)))
      ;; The spec says that the download always happens with these
      ;; serial line parameters, regardless of the settings you're
      ;; using at the time you send the WSCRIPT command:
      (serial-process-configure :port gm862-tty-name
				:speed 115200
				:bytesize 8
				:parity nil
				:stopbits 2
				:flowcontrol 'hw)
      ;; We should wait for a prompt ">>>", according to P84 of
      ;; "EasyScript in Python"
      (let ((countdown 12))
	(while (and (not gm862-download-prompt-received)
		    (> countdown 0))
	(sit-for 1)
	(message "Waiting for download prompt (%d)" countdown)
	(setq countdown (1- countdown))))
      (let ((line-count (length file-as-lines))
	    (i 0))
	(dolist (line file-as-lines)
	  (message "%d/%d: %s" i line-count line)
	  (setq i (1+ i))
	  (process-send-string gm862-process line)
	  (process-send-string gm862-process "\r\n")
	  (sit-for gm862-download-chunk-interval t)
	  (when gm862-download-error-received
	    (message "Detected error!"))
	  (gm862-show-output))
      ;; todo: can probably take this one out now
)      (when t
	(dotimes (i 4)
	  (process-send-string gm862-process "\r\n")
	  (sit-for gm862-download-chunk-interval t)))
      (serial-process-configure :port gm862-tty-name
				:speed (plist-get old-state :speed)
				:bytesize (plist-get old-state :bytesize)
				:parity (plist-get old-state :parity)
				:stopbits (plist-get old-state :stopbits)
				:flowcontrol (plist-get old-state :flowcontrol))))
  (unless (assoc gm862-filename gm862-scripts-alist)
    (push (cons gm862-filename file)
	  gm862-scripts-alist))
  (message "%s downloaded as %s" file gm862-filename)
  (gm862-show-output))

(defun gm862-send-this-file ()
  "Send the file in the current buffer to the GM862."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
	(gm862-send-file file (file-name-nondirectory file))
      (error "Buffer %s is not visiting a file" (buffer-name)))))

(defun gm862-command (command &rest args)
  "Send COMMAND to the gm862.
Optional ARGS are formatted into the command."
  (interactive "sCommand: ")
  (ensure-gm862-term)
  (if gm862-cmux-active
      (gm862-cmux-send-string gm862-process 1
			      (concat "at"
				      (apply 'format
					     command
					     args)
				      "\r\n"))
    (process-send-string gm862-process
			 (concat "at"
				 (apply 'format
					command
					args)
				 "\r\n")))
  (gm862-show-output))

(defun gm862-list-scripts ()
  "Tell the GM862 to list its installed scripts."
  (interactive)
  (setq gm862-caught-output nil
	gm862-catch-output t)
  (gm862-command "#lscript")
  ;; TODO: use the output for the completion alist
  (message "caught output: %S" gm862-caught-output)
  (let ((lines nil))
    (dolist (bunch gm862-caught-output)
      (dolist (line (split-string bunch "\n"))
	(push line lines)))
    (message "Got lines %S" lines))
  (setq gm862-caught-output nil
	gm862-catch-output nil))

(defun gm862-read-script (script)
  "Tell the GM862 to read back SCRIPT."
  (interactive (list (gm862-prompt-for-script-name "Script to read: ")))
  (gm862-command "#rscript=\"%s\"" script))

(defun gm862-delete-script (script)
  "Delete SCRIPT."
  (interactive (list (gm862-prompt-for-script-name "Script to delete: ")))
  (gm862-command "#dscript=\"%s\"" script))

(defun gm862-activate-script (script)
  "Make SCRIPT active."
  (interactive (list (gm862-prompt-for-script-name "Script to make active: ")))
  (gm862-command "#escript=\"%s\"" script))

(defun gm862-show-active-script ()
  "Show which script is active."
  (interactive)
  (gm862-command "#escript?"))

(defun gm862-run-active-script ()
  "Run the active script."
  (interactive)
  (gm862-command "#execscr")
  (sit-for 1)
  (gm862-stopwatch))

(defun gm862-show-battery-charger-status ()
  "Tell the GM862 to show its battery/charger status."
  (interactive)
  (gm862-command "#cbc"))

(defvar gm862-selected-interface-style 0
  "The interface style most recently selected.")

(defun gm862-select-interface-style (style)
  "Set the interface to STYLE."
  (interactive
   (list
    (completing-read "Interface style: "
		     '("0" "1" "2")
		     nil
		     t
		     nil
		     nil
		     "2")))
  (gm862-command "#selint=%s" style)
  (setq gm862-selected-interface-style (string-to-number style)))

(provide 'gm862)
;;; gm862.el ends here
