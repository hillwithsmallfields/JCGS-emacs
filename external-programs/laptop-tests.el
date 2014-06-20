;;; laptop-tests.el --- xenclient laptop testing

;; Copyright (C) 2012  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extra stuff for tracked-compile that is specific to XC's laptop setup

;;; Code:

(require 'tracked-compile)

(defvar prompt-for-ipaddr-map
  (let* ((map (make-keymap))
	 (c ?\0)
	 (s " "))
    (while (< c
	      127)
      (aset s 0 c)
      (define-key map s 'undefined)
      (setq c (1+ c)))
    (define-key map "0" 'self-insert-command)
    (define-key map "1" 'self-insert-command)
    (define-key map "2" 'self-insert-command)
    (define-key map "3" 'self-insert-command)
    (define-key map "4" 'self-insert-command)
    (define-key map "5" 'self-insert-command)
    (define-key map "6" 'self-insert-command)
    (define-key map "7" 'self-insert-command)
    (define-key map "8" 'self-insert-command)
    (define-key map "9" 'self-insert-command)
    (define-key map "." 'self-insert-command)
    (define-key map "\r" 'exit-minibuffer)
    (define-key map "\n" 'exit-minibuffer)
    (define-key map "\d" 'backward-delete-char)
    map)
  "Keymap for reading IP addresses.")

(defvar prompt-for-ipaddr-history nil
  "History for prompting for IP addresses.")

(defun prompt-for-ipaddr (&optional prompt)
  "Prompt for an IP address.  Optional PROMPT may be given."
  (read-from-minibuffer (or prompt "IP address: ")
			(car prompt-for-ipaddr-history)
			prompt-for-ipaddr-map
			nil
			'prompt-for-ipaddr-history))

(defun pxe-versions-list (&optional laptop)
  "List the pxe versions available for LAPTOP."
  (directory-files (xc-pxe-subdirectory)
		   nil (concat (or laptop
				 current-laptop)
			     "-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}")
		   ))

(defvar pxe-root "/home/xc_tftpboot/pxe"
  "The PXE root directory.")

(defvar pxe-handle nil
  "Your handle within the PXE directory.")

(defvar prompt-for-laptop-history nil
  "History for prompting for laptops.")

(defvar prompt-for-version-history nil
  "History for prompting for versions.")

(defvar current-laptop nil
  "The current laptop to use.")

(defvar current-guest nil
  "The current guest to use.")

(defun xc-build-directory ()
  "Return my current XC build directory."
  (let ((build-dirs (directory-files (expand-file-name "~/xcbuild")
				     t
				     "[0-9]\\{8\\}")))
    (car (last build-dirs))))

(defun xc-image-directory (module)
  "Return my current XC image directory for MODULE."
  (car (directory-files
	(expand-file-name
	 "build-scripts/build/oe/build/work/i686core2-angstrom-linux"
	 (xc-build-directory))
	t
	(format "%s.+[0-9]+\\.[0-9]+\\.[0-9]+$" module))))

(defun xc-pxe-subdirectory ()
  "Return my pxe subdirectory."
  (expand-file-name pxe-handle
		    pxe-root))

(defun xc-latest-loaded-pxedir (laptop)
  "Find the most recently loaded PXE subdirectory for LAPTOP."
  (car (last (directory-files (xc-pxe-subdirectory)
			      t
			      (format "%s-[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
				      laptop)))))

(defun prompt-for-version (&optional prompt laptop)
  "Prompt for a version, optionally with PROMPT and for LAPTOP."
  (completing-read (or prompt "Version: ")
		   (pxe-versions-list laptop)
		   nil
		   t
		   (car prompt-for-version-history)
		   'prompt-for-version-history
		   ))

(defun prompt-for-laptop (&optional prompt)
  "Prompt for the name of a laptop."
  (read-from-minibuffer (or prompt "Laptop name: ")
			      (car prompt-for-laptop-history)
			      nil
			      nil
			      'prompt-for-laptop-history))

(defun xc-get-config (pxedir)
  "Get the configuration from PXEDIR."
  (save-excursion
    (let* ((file (expand-file-name "xenclient.conf" pxedir))
	   (visiting (find-buffer-visiting file))
	   (result nil))
      (find-file file)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\) = \\(.+\\)$" (point-max) t)
	(push (cons (match-string-no-properties 1)
		    (match-string-no-properties 2))
	      result))
      (unless visiting
	(kill-buffer))
      result)))

(defun xc-config-attr (config attr)
  "For CONFIG, get ATTR."
  (cdr (assoc attr config)))

(define-tracked-command "load-laptop" "$COMMON/projects/xen/scripts/desk/"
  (lambda (command) (list (setq current-laptop (prompt-for-laptop "Load laptop: "))))
  (lambda (laptop) (let* ((pxedir (xc-latest-loaded-pxedir laptop))
			  (xc-config (xc-get-config pxedir))
			  (fred (message "Got config %S" xc-config))
			  (build (xc-config-attr xc-config "build"))
			  (build-date (xc-config-attr xc-config "build_date"))
			  (build-branch (xc-config-attr xc-config "build_branch"))
			  (version (xc-config-attr xc-config "version"))
			  (release (xc-config-attr xc-config "release"))
			  (tools (xc-config-attr xc-config "tools")))
		     (laptop-state-put-alist
		      laptop
		      (list (cons "xc-build" (format "build-%s" build))
			    (cons "xen-build-date" build-date)
			    (cons "branch" build-branch)
			    (cons "version" version)
			    (cons "release" release)
			    (cons "tools" tools)
			    (cons "qemu" "default")))
		     (laptop-state-record laptop))))

(define-tracked-command "load-guest" "$COMMON/projects/xen/scripts/desk/"
  (lambda (command) (list (setq current-guest (prompt-for-ipaddr "Load guest at IP address: ")))))

(define-tracked-command "pxe-setup-laptop" "$COMMON/projects/xen/scripts/desk/"
  (lambda (command) (list (setq current-laptop (prompt-for-laptop "PXE setup for laptop: "))))
  (lambda (laptop) (laptop-state-put-alist
		    laptop
		    '(("xen" . "default-pxe")))))

(define-tracked-command "pxe-from-saved" "$COMMON/projects/xen/scripts/desk/"
  (lambda (command) (list (yes-or-no-p "Use originals? ")
		   (prompt-for-version "Use saved version: "
				       current-laptop)))
  (lambda (original version)
    (laptop-state-put current-laptop "xen" (concat (if original "original-" "changed-")
						   version))))

(define-tracked-command "qemu-to-laptop" "$COMMON/projects/xen/scripts/desk/"
  (lambda (command)
    (list (yes-or-no-p "Use original? ")
	  (setq current-laptop (prompt-for-laptop "Load qemu onto laptop: "))))
  (lambda (original laptop)
    (laptop-state-put current-laptop
		      "qemu"
		      (if original
			  "original"
			(let* ((image-dir (xc-image-directory "ioemu-get"))
			       (qemu-file (expand-file-name "usr/lib/xen/bin/qemu-dm"
							    image-dir))
			       (qemu-modtime (nth 5 (file-attributes qemu-file)))
			       )
			  (format-time-string "changed-%Y_%m_%d_%H_%M_%S"
					      qemu-modtime))))))

(provide 'laptop-tests)

;;; laptop-tests.el ends here
