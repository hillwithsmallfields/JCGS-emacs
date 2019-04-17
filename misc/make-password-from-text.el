;;; make-password-from-text.el --- Make a password derived from a piece of text, so I have a mnemonic for it.

;; Copyright (C) 2019  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I feel safe publishing how I generate passwords that I have some
;; chance of memorizing, because the raw material from which I can
;; draw the mnemonic phrases is so vast.  Hiding the algorithm would
;; be "security by obscurity" and not hinder anyone trying to guess my
;; passwords.

;;; Code:

(defun make-password-rle (fragments)
  "Return FRAGMENTS but with repeats marked by numbers."
  (let ((accum nil)
        (count 1))
    (while fragments
      (let ((this (car fragments)))
        (if (equal this (cadr fragments))
            (setq count (1+ count))
          (push (if (> count 1)
                    (format "%d%s" count this)
                  this)
                accum)
          (setq count 1)))
      (setq fragments (cdr fragments)))
    (nreverse accum)))

(defun make-password-ensure-charclass (base regexp pool)
  "If BASE contains no match for REGEXP, replace a char with one from POOL."
  (unless (string-match regexp base)
    (aset base (random (length base))
          (aref pool
                (random (length pool)))))
  base)

(defun make-password-from-string (phrase)
  "Make a password from some letters of PHRASE.
Note that if you use this interactively, and save
your Emacs command history in a file, you will have
left a good clue as to what you changed a password on 
one of your systems to."
  (interactive "sPhrase to base password on: ")
  (let ((password (mapconcat (lambda (word)
                               (if (> (length word) 1)
                                   (substring word 0 2)
                                 word))
                             (make-password-rle (split-string phrase))
                             "")))
    (when (< (length password) 12)
      (error "Not enough characters to pass typical new password checkers"))
    (when (> (length password) 16)
      (setq password (substring password 0 (+ 12 (random 5)))))
    (setq password
          (make-password-ensure-charclass
           (make-password-ensure-charclass
            password "[,.&!@#$%^&*(()]" ",.;")
           "[0-9]" "0123456789"))
    (when (called-interactively-p)
      (message "Password \"%s\" (you should probably mangle this further)"
               password))
    password))

(defun make-password-from-region (begin end)
  "Make a password from some letters of the text between BEGIN and END."
  (interactive "r")
  (let ((password (make-password-from-string
                   (buffer-substring-no-properties begin end))))
    (when (called-interactively-p)
      (message "Password \"%s\" (you should probably mangle this further)"
               password))
    password))

(defun random-emacs-command ()
  "Return the symbol naming a random Emacs command."
  (let ((symbols nil))
    (mapatoms (function
               (lambda (atom)
                 (when (commandp atom)
                   (push atom symbols)))))
    (nth (random (length symbols))
         symbols)))

(defun docstring-first-line (function-name)
  "Return the short help string for a random Emacs command."
  (car (split-string (documentation function-name) "\n")))

(defun make-password-from-emacs-help (&optional copy)
  "Make a password with a mnemonic phrase.
The phrase is the first line of the help string for a random Emacs command.
With non-nil COPY (prefix interactively), put the password into the kill ring."
  (interactive "P")
  (let* ((fname (random-emacs-command))
         (mnemonic (docstring-first-line fname))
         (password (make-password-from-string mnemonic)))
    (when copy
      (kill-new password))
    (message "Password \"%s\" with mnemonic from %s \"%s\""
             password fname mnemonic)))

(provide 'make-password-from-text)
;;; make-password-from-text.el ends here
