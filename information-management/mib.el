;;; mib.el --- parse SNMP MIBs

;; Copyright (C) 2013  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: data, convenience

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

;; Parse MIB data.

;;; Code:

(defconst mib-definition-pattern "^\\([a-z0-9]+\\)\\s-+OBJECT IDENTIFIER ::=\\s-+{\\s-+\\([a-z0-9]+\\)\\s-+\\([0-9]+\\)\\s-+}"
  "Pattern for a MIB definition line.")

(defvar mib-mib-buffers nil
  "Alist of buffers from which MIB information has been parsed.
Maps names to buffers.")

(defvar mib-symbol-table nil
  "Symbol table for a MIB buffer.
Becomes buffer-local when set.")

(make-variable-buffer-local 'mib-symbol-table)

(defvar mib-root-symbol nil
  "The root symbol for this MIB buffer.
Becomes buffer-local when set.")

(make-variable-buffer-local 'mib-root-symbol)

(defun mib-parse ()
  "Parse a MIB file."
  (interactive)
  (add-to-list 'mib-mib-buffers (cons (buffer-name) (current-buffer)))
  (save-excursion
    (setq mib-symbol-table (make-vector 65535 nil)
	  mib-root-symbol nil)
    (goto-char (point-min))
    (while (re-search-forward mib-definition-pattern (point-max) t)
      (let* ((name (intern (match-string-no-properties 1) mib-symbol-table))
	     (parent-string (match-string-no-properties 2))
	     (parent-known (intern-soft parent-string mib-symbol-table))
	     (parent (intern parent-string mib-symbol-table))
	     (number (string-to-number (match-string-no-properties 3))))
	(set name nil)
	(message "defining parent=%S (%s) [%d] to name=%S" parent (if parent-known "already defined" "not yet defined") number name)
	(set parent (cons (cons name number)
			  (if parent-known
			      (symbol-value parent)
			    nil)))
	(message "%s now defined as %S" parent (symbol-value parent))
	(when (and (not parent-known)
		   (null mib-root-symbol))
	  (message "setting mib-root-symbol to %s" parent)
	  (setq mib-root-symbol parent)))))
  (let ((root-value (symbol-value mib-root-symbol)))
    (message "root value is %S" root-value)
    (if (= (length root-value) 1)
	(setq mib-root-symbol (car (car root-value)))
      (error "Multiple sub-roots"))
    (message "root symbol is now %S" mib-root-symbol)
    )
  )

(defun mib-numeric-to-symbolic (numeric-string mib-buffer)
  "Convert NUMERIC-STRING to its symbols, using MIB-BUFFER for the symbols.
NUMERIC-STRING is assumed to be a complete string, i.e. its first
number represents the root node."
  (interactive
   (list
    (read-from-minibuffer "Convert numeric string: ")
    (completing-read "Use MIB buffer: " mib-mib-buffers nil t)))
  (save-excursion
    (set-buffer mib-buffer)
    (let ((parent mib-root-symbol)
	  (result nil))
      (message "set parent to %s" parent)
      (dolist (number (split-string numeric-string "\\." t))
	(message "Looking up %s in %s=%S" number parent (symbol-value parent))
	(let ((symbol (car (rassq (string-to-number number) (symbol-value parent)))))
	  (setq result (cons symbol result)
		parent symbol)))
      (setq result (nreverse result))
      (when t (interactive-p)
	(message (mapconcat 'symbol-name result " ")))
      result)))

(provide 'mib)

;;; mib.el ends here
