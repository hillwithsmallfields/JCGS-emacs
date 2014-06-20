;;; ratpoison.el --- ratpoison support for Emacs
;;
;; Copyright (C) 2003 Gergely Nagy, Shawn Betts, Jay Belanger
;; Copyright (C) 2008, 2012 John Sturdy
;;
;; Authors: Gergely Nagy <algernon@debian.org>,
;;          Shawn Betts <sabetts@users.sourceforge.net>,
;;          Jay Belanger <belanger@truman.edu>,
;;          John Sturdy <john@cb1.com>
;; Maintainer: Gergely Nagy <algernon@debian.org>
;; Version: 0.3
;; Keywords: faces, ratpoison, X
;; CVS Id: $Id: ratpoison.el,v 1.5 2006/03/16 00:33:34 sabetts Exp $
;; Last updated: <2001/10/05 17:58:38 algernon>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This file provides a major mode for editing .ratpoisonrc files, and
;; functions to access ratpoison from within Emacs.

;;; History:
;; Version 0.3:
;;  - Added commands analogous to Emacs' window commands (John)
;; Version 0.2:
;;  - Added command-interpreter (from Shawn and Jay)
;;  - Added info-lookup functions (from Jay)
;;  - renamed to ratpoison.el
;;  - far better font-locking
;; Version 0.1:
;;  - initial version

;;; Todo:
;; - auto-completion of commands
;; - probably a bunch of other things

(require 'font-lock)
(require 'generic-x)

(defvar ratpoison-commands-0
  (list
   "abort"
   "banish"
   "clock"
   "curframe"
   "delete"
   "focus"
   "focusup"
   "focusdown"
   "focusleft"
   "focusright"
   "meta"
   "help"
   "info"
   "kill"
   "lastmsg"
   "redisplay"
   "restart"
   "next"
   "only"
   "other"
   "prev"
   "quit"
   "remove"
   "split"
   "hsplit"
   "version"
   "vsplit"
   ))

(defvar ratpoison-commands-rest
  (list
   "bind"
   "chdir"
   "colon"
   "defbarloc"
   "msgwait"
   "defborder"
   "deffont"
   "definputwidth"
   "defmaxsizepos"
   "defpadding"
   "deftranspos"
   "defwaitcursor"
   "defwinfmt"
   "defwinname"
   "defwinpos"
   "deffgcolor"
   "defbgcolor"
   "escape"
   "echo"
   "exec"
   "newwm"
   "number"
   "pos"
   "rudeness"
   "select"
   "setenv"
   "source"
   "startup_message"
   "title"
   "unbind"
   "unsetenv"
   "windows"
   ))

;; ratpoisonrc-mode
(define-generic-mode 'ratpoisonrc-mode
  ;; comments
  (list ?#)
  ;; keywords
  nil
  ;; font-lock stuff
  (list
    ;; commands without arguments
    (generic-make-keywords-list
     ratpoison-commands-0 font-lock-builtin-face "^[ \t]*")
    ;; commands with arguments
    (generic-make-keywords-list
     ratpoison-commands-rest font-lock-builtin-face "^[ \t]*" "[ \t]+")
    ;; exec <arg>
    (list "^[ \t]*\\(exec\\)[ \t]+\\(.*\\)"
          '(1 'font-lock-builtin-face)
          '(2 'font-lock-string-face))
    ;; arguments, the first is a keyword, the rest is tring
    (list (concat
                (car (generic-make-keywords-list
                      ratpoison-commands-rest font-lock-builtin-face "^[ \t]*" "[ \t]+"))
                "\\([0-9a-zA-Z\\/\\.\\-]+\\)[ \t]*\\(.*\\)")
          '(2 'font-lock-keyword-face)
          '(3 'font-lock-string-face)))
  ;; auto-mode alist
  (list "\\.ratpoisonrc\\'")
  ;; additional setup functions
  (list 'ratpoisonrc-mode-setup)
  "Generic mode for ratpoison configuration files.")

(defun ratpoisonrc-mode-setup()
  (defvar ratpoisonrc-mode-keymap (make-sparse-keymap)
    "Keymap for ratpoisonrc-mode")
  (define-key ratpoisonrc-mode-keymap "\C-c\C-e" 'ratpoison-line)
  (use-local-map ratpoisonrc-mode-keymap))

(provide 'ratpoisonrc-mode)

;; Ratpoison access
; Groups & Variables
(defgroup ratpoison nil "Ratpoison access"
  :group 'languages
  :prefix "ratpoison-")

(defcustom ratpoison-program "ratpoison"
  "The command to call the window manager."
  :group 'ratpoison
  :type 'string)

; Command stuff
(defun ratpoison-command (command)
  (interactive "sRP Command: ")
  (call-process ratpoison-program nil nil nil "-c" command))

(defun ratpoison-command-on-region (start end)
  (interactive "r")
  (mapcar 'ratpoison-command
          (split-string (buffer-substring start end)
                        "\n")))

(defun ratpoison-line ()
  "Send the current line to ratpoison."
  (interactive)
  (ratpoison-command
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

;; Documentation
(defun ratpoison-info ()
  "Call up the ratpoison info page."
  (interactive)
  (info "ratpoison"))

(defun ratpoison-command-info ()
  "Call up the info page listing the ratpoison commands."
  (interactive)
  (info "(ratpoison) Commands"))

;; Getting information from ratpoison into Emacs
(defun ratpoison-version ()
  "Return the version of ratpoison, as a string."
  (let ((ratpoison-buffer (get-buffer-create " *ratpoison version*")))
    (set-buffer ratpoison-buffer)
    (erase-buffer)
    (call-process ratpoison-program
		  nil
		  ratpoison-buffer
		  nil
		  "-c" "version")
    (split-string (buffer-string) "\n" t)))

(defun ratpoison-window-list ()
  "Return the list of ratpoison windows."
  (let ((ratpoison-buffer (get-buffer-create " *ratpoison window list*")))
    (set-buffer ratpoison-buffer)
    (erase-buffer)
    (call-process ratpoison-program
		  nil
		  ratpoison-buffer
		  nil
		  "-c" "windows %t")
    (split-string (buffer-string) "\n" t)))

(defun current-ratpoison-frame-configuration ()
  "Return the list of ratpoison frame.
Tries to be like Emacs' `current-frame-configuration'."
  (let ((ratpoison-buffer (get-buffer-create " *ratpoison frame list*")))
    (set-buffer ratpoison-buffer)
    (erase-buffer)
    (call-process ratpoison-program
		  nil
		  ratpoison-buffer
		  nil
		  "-c" "fdump")
    (cons 'ratpoison-frame-configuration
	  (mapcar 'read
	    (split-string (buffer-string) ", *\n*" t)))))

(defun set-ratpoison-frame-configuration (configuration)
  "Restore the ratpoison frames to  the state described by CONFIGURATION."
  (unless (eq (car configuration) 'ratpoison-frame-configuration)
    (error
     "set-ratpoison-frame-configuration needs a ratpoison-frame-configuration"))
  (ratpoison-command
   (concat "frestore "
	   (mapconcat 'prin1-to-string
		      (cdr configuration)
		      ","))))

(defmacro save-ratpoison-frame-excursion (&rest body)
  "Like save-window-excursion, but for ratpoison frames."
  `(let ((frame-config (current-ratpoison-frame-configuration)))
     (unwind-protect
	 (progn
	   ,@body)
       (set-ratpoison-frame-configuration frame-config))))

;; ;; Commands analogous to Emacs' window commands
;; (defun ratpoison-remove-frame ()
;;   "Remove the current frame from the screen."
;;   (interactive)
;;   (ratpoison-command "remove"))

;; (defun ratpoison-delete-other-windows ()
;;   "Make the current frame fill the screen."
;;   (interactive)
;;   (ratpoison-command "only"))

;; (defun ratpoison-split-window-vertically ()
;;   "Split current ratpoison window into two windows, one above the other."
;;   (interactive)
;;   (ratpoison-command "split"))

;; (defun ratpoison-split-window-horizontally ()
;;   "Split current ratpoison window into two windows, one beside the other."
;;   (interactive)
;;   (ratpoison-command "hsplit"))

;; (defun ratpoison-shrink-frame (n)
;;   "Shrink the current frame by N pixels vertically."
;;   (interactive "NShrink frame by: ")
;;   (ratpoison-command (format "resize 0 -%d" n)))

;; (defun ratpoison-enlarge-frame (n)
;;   "Enlarge the current frame by N pixels vertically.."
;;   (interactive "NEnlarge frame by: ")
;;   (ratpoison-command (format "resize 0 %d" n)))

;; (defun ratpoison-shrink-frame-horizontally (n)
;;   "Shrink the current frame by N pixels horizontally.."
;;   (interactive "NShrink frame by: ")
;;   (ratpoison-command (format "resize -%d 0" n)))

;; (defun ratpoison-enlarge-frame-horizontally (n)
;;   "Enlarge the current frame by N pixels horizontally."
;;   (interactive "NEnlarge frame by: ")
;;   (ratpoison-command (format "resize %d 0" n)))

;; (defun ratpoison-other-window ()
;;   (interactive)
;;   (ratpoison-command "focus"))

;; (defun ratpoison-delete-window
;;   (interactive)
;;   (ratpoison-command "delete"))

;; (defun ratpoison-kill-window
;;   (interactive)
;;   (ratpoison-command "kill"))

;; (defun ratpoison-undo ()
;;   "Undo the most recent ratpoison frame layout change."
;;   (interactive)
;;   (ratpoison-command "undo"))

;; (defun ratpoison-select (window)
;;   "Tell ratpoison to select WINDOW."
;;   (interactive
;;    (list (completing-read "Select window: "
;; 			  (mapcar 'list (ratpoison-window-list)))))
;;   (ratpoison-command (concat "select " window)))

;; (defun ratpoison-select-other-pane (window)
;;   "Tell ratpoison to select WINDOW in another pane."
;;   (interactive
;;    (list (completing-read "Select window: "
;; 			  (mapcar 'list (ratpoison-window-list)))))
;;   (ratpoison-command "focus")
;;   (ratpoison-command (concat "select " window)))

(defvar ratpoison-command-map
  (let ((map (make-sparse-keymap "Ratpoison")))
    (define-key map "0" 'ratpoison-remove-frame)
    (define-key map "1" 'ratpoison-delete-other-windows)
    (define-key map "2" 'ratpoison-split-window-vertically)
    (define-key map "3" 'ratpoison-split-window-horizontally)
    (define-key map "\C-_" 'ratpoison-undo)
    (define-key map "}" 'ratpoison-enlarge-frame-horizontally)
    (define-key map "{" 'ratpoison-shrink-frame-horizontally)
    (define-key map "^" 'ratpoison-enlarge-frame-horizontally)
    (define-key map "v" 'ratpoison-shrink-frame-horizontally)
    (define-key map "o" 'ratpoison-other-window)
    (define-key map "k" 'ratpoison-delete-window)
    (define-key map "K" 'ratpoison-kill-window)
    (define-key map "b" 'ratpoison-select)
    map))

(fset 'ratpoison-command-map ratpoison-command-map)

(global-set-key "\C-xw" ratpoison-command-map)

(define-minor-mode ratpoison
  "Minor mode to control ratpoison from Emacs key sequences."
  nil
  nil
  'ratpoison-command-map
  :global t
  )

(provide 'ratpoison)
