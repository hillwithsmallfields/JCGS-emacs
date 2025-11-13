;;; config-shells.el --- configure subshells         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: processes

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

;; 

;;; Code:

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/codeberg.org/akib/emacs-eat") t)

(autoload 'eat-term-make "eat"
  "Make a Eat terminal at POSITION in BUFFER.")

(autoload 'eat "eat"
    "Start a new Eat terminal emulator in a buffer.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eat]),
switch to the session with that number, or create it if it doesn't
already exist.

With double prefix argument ARG, ask for the program to run and run it
in a newly created session.

PROGRAM can be a shell command." t)

(autoload 'eat-other-window "eat"
  "Start a new Eat terminal emulator in a buffer in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG switch to the session with that number, or
create it if it doesn't already exist.

With double prefix argument ARG, ask for the program to run and run it
in a newly created session.

PROGRAM can be a shell command." t)

(autoload 'eat-eshell-mode "eat"
  "Toggle Eat terminal emulation in Eshell." t)

(autoload 'eat-eshell-visual-command-mode "eat"
  "Toggle running Eshell visual commands with Eat." t)

(autoload 'eat-project "eat"
  "Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
  t)

(autoload 'eat-project-other-window "eat"
  "Start Eat in the current project root directory in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
  t)

(provide 'config-shells)
;;; config-shells.el ends here
