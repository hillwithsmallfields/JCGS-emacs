;;; annotation.el --- Manage annotation of source files in projects

;; Copyright (C) 2011, 2021  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: docs

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

;; This helps you manage your own per-defun commentary on source code,
;; without modifying the original source files.

;; Annotations are kept in an org-mode file, which can be named
;; locally per-buffer, or globally.

;; A special syntax:
;;  # <<projectname:filename:functionname>>
;; is used in the annotation file(s), to locate where to display the
;; annotations.  The user can provide hook functions to recognize
;; projects and to make filenames relative to projects.

;; Annotations are displayed using overlays.  A function, suitable for
;; use on a mode-hook, is provided for decorating a file with its
;; annotation overlays.

;;; Code:

(defvar annotation-file "~/notes.org"
  "The annotation org file for this buffer.
May be a central one for everything, or you can use separate files for
different projects.")

(defvar annotation-directory-description-functions nil
  "List of functions from directory to description of the directory.")

(defvar annotation-relative-file-functions nil
  "List of functions from directory, project and filename to relative filename.")

(defun find-linux-kernel-source ()
  "Find the linux kernel source."
  (if (file-exists-p "/usr/src/linux")
      "/usr/src/linux"
    (expand-file-name
     "$GATHERED/source/linux/kernel/linux-source-2.6.32")))

(add-hook 'annotation-directory-description-functions
	  (lambda (dir)
	    (if (string-match "linux-kernel" dir)
		(find-linux-kernel-source)
	      nil)))

(add-hook 'annotation-directory-description-functions
	  (lambda (dir)
	    (if (string-match "i686core2-angstrom-linux/\\([a-z0-9]+\\)-" dir)
		(match-string 1 dir)
		nil)))

(add-hook 'annotation-directory-description-functions
	  (lambda (dir)
	    (if (string-match "open-projects/\\([^/]+\\)/" dir)
		(match-string 1 dir)
		nil)))

(add-hook 'annotation-directory-description-functions
	  (lambda (dir)
	    (if (string-match (substitute-in-file-name
                               "$SYNCED/emacs/\\([^/]+\\)/") dir)
		(concat "emacs/" (match-string 1 dir))
		nil)))

(add-hook 'annotation-relative-file-functions
	  (lambda (dir project file)
	    (file-name-nondirectory file)))

(add-hook 'annotation-relative-file-functions
	  (lambda (dir project file)
	    (if (string= project "linux-kernel")
		(if (string-match "kernel/\\(.+\\)" file)
		    (match-string-no-properties 1 file)
		  nil)
	      nil)))

(add-hook 'annotation-relative-file-functions
	  (lambda (dir project file)
	    (if (string-match "/git/\\(.+\\)$" dir)
		(match-string-no-properties 1 dir)
		nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Short names for things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun annotation-project-description (&optional directory)
  "Return a description of optional DIRECTORY (or current), in terms of projects."
  (run-hook-with-args-until-success 'annotation-directory-description-functions
				    (or directory default-directory)))

(defun annotation-current-file (project directory filename)
  "Return the name within PROJECT (in DIRECTORY) for FILENAME."
  (run-hook-with-args-until-success 'annotation-relative-file-functions
				    project directory filename))

;;;;;;;;;;;;;;;;;
;; Annotations ;;
;;;;;;;;;;;;;;;;;

(defun annotation-find (project file defun)
  "Find an annotation for PROJECT FILE DEFUN.
Returns cons of its annotation file and starting position therein,
or nil if no annotation is found."
  (interactive "sProject:
sFile:
sDefun: ")
  (save-excursion
    (find-file annotation-file)
    (let ((old (point)))
      (goto-char (point-min))
      (if (search-forward (concat "<<" project ":" file ":" defun ">>") (point-max) t)
	  (progn
	    (beginning-of-line 2)
	    (cons (current-buffer) (point)))
	(goto-char old)
	nil))))

(defun annotation-text (project file defun)
  "Return the annotation text for PROJECT FILE DEFUN.
Returns nil if no annotation is found."
  (interactive "sProject:
sFile:
sDefun: ")
  (save-excursion
    (let* ((where (annotation-find project file defun)))
      (if (null where)
	  nil
	(set-buffer (car where))
	(goto-char (cdr where))
	(if (re-search-forward "^\\*\\{1,4\\} " (point-max) t)
	    (buffer-substring-no-properties (cdr where) (1- (match-beginning 0)))
	  (goto-char (point-max))
	  (when (bolp)
	    (end-of-line 0))
	  (buffer-substring-no-properties (cdr where) (point)))))))

(defun annotation-open (project file defun)
  "Open an annotation for PROJECT FILE DEFUN."
  (interactive "sProject:
sFile:
sDefun: ")
  (find-file annotation-file)
  ;; we must be in something based on org-mode for some org-mode
  ;; functions we use to work; we mustn't call the mode setup
  ;; function each time, because it kills all local variables
  (if (fboundp 'org-annotation-mode)
      (unless (eq major-mode 'org-annotation-mode)
	(org-annotation-mode))	     ; todo: write org-annotation-mode
    (unless (eq major-mode 'org-mode)
      (org-mode)))
  (goto-char (point-max))
  ;; find or create project heading
  (if (re-search-backward (format "^\\*\\* %s$" project) (point-min) t)
      (end-of-line)
    (goto-char (point-max))
    (insert "** " project "\n")
    (end-of-line 0))
  ;; now at end of the right project heading
  (let* ((project-end
	  (save-excursion
	    (if (re-search-forward "^\\*\\* " (point-max) t)
		(progn
		  (beginning-of-line 1)
		  (point))
	      (point-max)))))
    (if (re-search-forward (format "^\\*\\*\\* %s$" file) project-end t)
	(end-of-line 1)
      (goto-char project-end)
      (insert "*** " file "\n")
      (end-of-line 0)))
  ;; now at end of the right file heading
  (let* ((file-end
	  (save-excursion
	    (if (re-search-forward "^\\*\\*\\* " (point-max) t)
		(progn
		  (beginning-of-line 0)
		  (point))
	      (point-max)))))
    (if (re-search-forward (format "^\\*\\*\\*\\* %s$" defun) file-end t)
	(beginning-of-line 3)
      (goto-char file-end)
      (insert "**** " defun "\n     # <<" project ":" file ":" defun ">>\n"))))

(defun annotation-update (where)
  "Update the display of the annotation around WHERE (point, interactively)."
  (interactive "d")
  
  )

(defun annotation-defun-name ()
  "Return the name of the defun around point."
  (save-excursion
    (cond
     ((eq major-mode 'c-mode)
      (c-defun-name))
     ((memq major-mode '(lisp-mode emacs-lisp-mode scheme-mode))
      (beginning-of-defun 1)
      (down-list 1)
      (forward-sexp 2)
      (let ((end (point)))
	(backward-sexp)
	(buffer-substring-no-properties (point) end))))))

(defun annotate-defun ()
  "Open an annotation of the defun around point."
  (interactive)
  (let* ((project (annotation-project-description))
	 (file (annotation-current-file project default-directory buffer-file-name))
	 (defun-name (annotation-defun-name)))
    (open-annotation project file defun-name)))

(defun annotation-previous-defun ()
  "Move to the beginning of the previous defun.
Puts in various safeguards so it won't get stuck and won't throw an error."
  (catch 'abandon
    (cond
     ((eq major-mode 'c-mode)
      (condition-case evar
	  (c-beginning-of-defun 1)
	(error
	 (let ((started (point))
	       attempt
	       (unfound t)
	       )
	   (message "Problem %S in going back a defun in C at %d in %s" evar started (buffer-file-name))
	   (while (and unfound (setq attempt (re-search-backward "^$" (point-min) t)))
	     (forward-line -1)
	     (when (< (point) started)
	       (setq unfound nil)))
	   (unless attempt (throw 'abandon nil)))
	 nil)))
     (t (beginning-of-defun 1))))
  )

(defvar annotation-overlays nil
  "Annotation overlays for this buffer.")

(make-variable-buffer-local 'annotation-overlays)

(defun annotation-decorate-file ()
  "Find the annotations for the current file, and display them."
  (interactive)
  (mapcar 'delete-overlay annotation-overlays)
  (save-window-excursion
    (save-excursion
      (let* ((project (annotation-project-description))
	     (file (annotation-current-file project
					    default-directory
					    buffer-file-name)))
	(goto-char (point-max))
	(let ((prev (point)))
	  (annotation-previous-defun)
	  (while (/= prev (point))
	    (let* ((defun-name (annotation-defun-name))
		   (annotation-text (annotation-text project file defun-name)))
	      ;; (message "annotation %S" annotation-text)
	      (when (stringp annotation-text)
		(setq annotation-text (concat "\n" comment-start
					      "Annotation: " annotation-text
					      comment-end "\n"))
		(let ((overlay (make-overlay (point) (point))))
		  (add-to-list 'annotation-overlays overlay)
		  (overlay-put overlay 'before-string annotation-text)))
	      (setq prev (point))
	      (annotation-previous-defun))))))))

;; (defun annotation-test ()
;;   "Test the annotation system."
;;   (interactive)
;;   (open-annotation "alpha" "main.c" "main")
;;   (open-annotation "alpha" "main.c" "init")
;;   (open-annotation "alpha" "file.c" "init")
;;   (open-annotation "alpha" "file.c" "open")
;;   (open-annotation "alpha" "file.c" "close")
;;   (open-annotation "beta" "thing.c" "fred")
;;   (open-annotation "beta" "thing.c" "jim")
;;   (open-annotation "beta" "stuff.c" "sheila")
;;   (open-annotation "alpha" "file.c" "status")
;;   (open-annotation "alpha" "file.c" "delete")
;;   (open-annotation "alpha" "screen.c" "draw"))

(provide 'annotation)
;;; annotation.el ends here
