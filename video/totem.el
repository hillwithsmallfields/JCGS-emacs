;;; totem.el --- control of a totem video player process

;; Copyright (C) 2009  John C G Sturdy

;; Author: John C G Sturdy <john.sturdy@ul.ie>
;; Keywords: processes, multimedia

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

;; This starts, and interacts with, the movie player `totem'.  I
;; originally wrote it for use with joystick.el, so I could sit back
;; and use a gamepad to control totem, without even having to leave
;; Emacs.

;;; Code:

(defvar totem-process nil
  "The current totem process, or nil if there is none.
Use the function of the same name to access it, to have one
created for you automatically.")

(defvar totem-commands
  '(("toggle play-pause" . "--play-pause")
    ("play" . "--play")
    ("pause" . "--pause")
    ("next" . "--next")
    ("previous" . "--previous")
    ("forwardnseek" . "--seek-fwd")
    ("backward seek" . "--seek-bwd")
    ("increase volume" . "--volume-up")
    ("reduce volume" . "--volume-down")
    ("fullscreen" . "--fullscreen")
    ("quit" . "--quit"))
  "Totem command names, and the associated strings.")

(defgroup totem nil
  "Control of the movie player totem."
  :prefix "totem-")

(defcustom totem-program "totem"
  "The program to run to get totem."
  :group 'totem
  :type 'string)

(defcustom totem-args nil
  "The arguments to give to totem when it starts up."
  :group 'totem
  :type '(repeat string))

(defun totem-process ()
  "Get the current totem process, creating one if necessary."
  (or totem-process
      (setq totem-process (apply 'start-process "*totem movie player*"
				 "*totem output*"
				 totem-program
				 totem-args)))
  totem-process)

(defun totem-prompt-for-command (prompt)
  "Read a totem command, prompting with PROMPT."
  (completing-read prompt
		   totem-commands
		   nil
		   t))

(defun totem-command (command)
  "Send COMMAND to toem."
  (interactive (list (totem-prompt-for-command "Command: ")))
  (shell-command (format "%s %s"
			 totem-program
			 (cdr (assoc command
				     totem-commands)))))

(defun totem-toggle-play-pause ()
  "Tell totem to toggle play-pause."
  (interactive)
  (totem-command "toggle play-pause"))

(defun totem-play (&optional new-args)
  "Tell totem to play.
With optional NEW-ARGS, start a new process using the given args."
  (interactive
   (list (if (or current-prefix-arg
		 (null totem-process))
	     (split-string (read-from-minibuffer "Totem args: ") " ")
	   nil)))
  (if new-args
      (let ((totem-args (append new-args totem-args)))
	(when totem-process
	  (totem-quit)
	  (kill-process totem-process))
	(totem-command "play"))
    (totem-command "play")))

(defun totem-pause ()
  "Tell totem to pause."
  (interactive)
  (totem-command "pause"))

(defun totem-next ()
  "Tell totem to next."
  (interactive)
  (totem-command "next"))

(defun totem-previous ()
  "Tell totem to previous."
  (interactive)
  (totem-command "previous"))

(defun totem-seek-forward ()
  "Tell totem to seek forward."
  (interactive)
  (totem-command "forward seek"))

(defun totem-seek-backward ()
  "Tell totem to seek backward."
  (interactive)
  (totem-command "backward seek"))

(defun totem-increase volume ()
  "Tell totem to increase the volume."
  (interactive)
  (totem-command "increase volume"))

(defun totem-reduce volume ()
  "Tell totem to reduce the volume."
  (interactive)
  (totem-command "reduce volume"))

(defun totem-fullscreen ()
  "Tell totem to use fullscreen."
  (interactive)
  (totem-command "fullscreen"))

(defun totem-quit ()
  "Tell totem to quit."
  (interactive)
  (totem-command "quit"))

(provide 'totem)
;;; totem.el ends here
