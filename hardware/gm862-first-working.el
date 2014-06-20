;;; gm862.el --- stuff for doing python on the Telit GM862

;; Copyright (C) 2012  John Sturdy

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

(defvar gm862-download-chunk-interval 1
  "How many seconds to wait between chunks")

(defvar gm862-process nil
  "The serial process for communicating with the GM862.")

;; (require 'term)

;; (define-key term-raw-map [ s-x ] 'ctl-x-map)

(defun gm862-process-filter (proc str)
  "Filter for text coming back from the GM862."
  (when (string-match ">>>" str)
    (message "Got file download prompt"))
  (term-emulate-terminal proc str))

(defun gm862-term ()
  "Set up a terminal to the GM862."
  (interactive)
  (setq gm862-process
	(get-buffer-process
	 (serial-term gm862-tty-name gm862-initial-baud-rate)))
  (set-process-filter gm862-process 'gm862-process-filter))

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
      (message "Old state is %S" old-state)
      (message "data size is %d, as %d lines" data-size chunks)
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
      ;; "EasyScript in Python"; for now, rather than doing stuff with
      ;; the process filter, we just allow a reasonable time for the
      ;; response to come back
      (sit-for 1)
      (dolist (line file-as-lines)
	(message "Sending %S" line)
	(process-send-string gm862-process line)
	(process-send-string gm862-process "\r\n")
	(sit-for gm862-download-chunk-interval t)
	(gm862-show-output))
      ;; todo: can probably take this one out now
      (when t
	(dotimes (i 4)
	  (process-send-string gm862-process "\r\n")
	  (sit-for gm862-download-chunk-interval t)))
      (serial-process-configure :port gm862-tty-name
				:speed (plist-get old-state :speed)
				:bytesize (plist-get old-state :bytesize)
				:parity (plist-get old-state :parity)
				:stopbits (plist-get old-state :stopbits)
				:flowcontrol (plist-get old-state :flowcontrol))))
  (gm862-show-output))

(defun gm862-command (command)
  "Send COMMAND to the gm862."
  (interactive "sCommand: ")
  (process-send-string gm862-process command)
  (gm862-show-output))

(defun gm862-list-scripts ()
  "Tell the GM862 to list its installed scripts."
  (interactive)
  (gm862-command "AT#LSCRIPT\r\n"))

(defun gm862-read-script (script)
  "Tell the GM862 to read back SCRIPT."
  (interactive "sScript to read: ")
  (gm862-command (format "AT#RSCRIPT=\"%s\"\r\n" script)))

(defun gm862-delete-script (script)
  "Delete SCRIPT."
  (interactive "sScript to delete: ")
  (gm862-command (format "AT#DSCRIPT=\"%s\"\r\n" script)))

(defun gm862-activate-script (script)
  "Make SCRIPT active."
  (interactive "sScript to make active: ")
  (gm862-command (format "AT#ESCRIPT=\"%s\"\r\n" script)))

(defun gm862-show-active-script ()
  "Show which script is active."
  (interactive)
  (gm862-command (format "at#escript?\r\n")))

(defun gm862-run-active-script ()
  "Run the active script."
  (interactive)
  (gm862-command "at#execscr\r\n"))

(provide 'gm862)
;;; gm862.el ends here
