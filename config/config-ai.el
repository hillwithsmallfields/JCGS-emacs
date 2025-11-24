;;; config-ai.el --- configure interfaces to AI systems  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
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

;; configure interfaces to claude etc

;;; Code:

(add-to-list 'load-path (substitute-in-file-name "$OPEN_PROJECTS/github.com/cpoile/claudemacs"))

(setq claudemacs-prefer-projectile-root nil)


;;; Generated autoloads from ../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs.el

(autoload 'claudemacs-run "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Start Claude Code or switch to existing session.
With prefix ARG, prompt for the project directory.

(fn &optional ARG)" t)
(autoload 'claudemacs-resume "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Start Claude Code with resume or switch to existing session.
With prefix ARG, prompt for the project directory.

(fn &optional ARG)" t)

(autoload 'claudemacs-fix-error-at-point "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Send a request to Claude to fix the error at point using flycheck." t)
(autoload 'claudemacs-execute-request "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Execute a Claude request with file context.
If a region is selected, use it as context with line range.
Otherwise, use current line as context." t)
(autoload 'claudemacs-ask-without-context "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Ask Claude a question without file or line context.
Prompts for a question and sends it directly to Claude without any 
file location or context information." t)

(autoload 'claudemacs-implement-comment "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Send comment at point or region to Claude for implementation.
If region is active, uses the exact region.
If no region, finds the comment block at point.
Extracts comment text and sends it to Claude with implementation instructions." t)

(defvar claudemacs-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c C-e") #'claudemacs-transient-menu) map) "\
Keymap for `claudemacs-mode'.")
(autoload 'claudemacs-mode "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" "\
Minor mode for Claude Code AI pair programming.

\\{claudemacs-mode-map}

This is a minor mode.  If called interactively, toggle the `Claudemacs
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `claudemacs-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs" '("claudemacs-"))


;;; Generated autoloads from ../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs-comment.el

(register-definition-prefixes "../home/jcgs/open-projects/github.com/cpoile/claudemacs/claudemacs-comment" '("claudemacs--"))

;;; End of scraped data



(provide 'config-ai)
;;; config-ai.el ends here
