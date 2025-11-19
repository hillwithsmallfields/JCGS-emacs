;;; Time-stamp: <2025-11-19 14:44:14 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'load-directory)

(defvar load-directory-loaded nil
  "The files loaded by load-directory.")

(defvar load-directory-bytes 0
  "The number of bytes loaded by load-directory.")

(defvar load-directory-pattern
      "\\.\\(elc?\\)\\|\\(ELC?\\)$"
  "Pattern for which files to load when loading all elisp in a directory.
Unfortunately, directory-files insists on treating its pattern
case-sensitively, case-fold-search notwithstanding.")

(setq load-directory-pattern "\\.elc?$")

(defvar load-directory-file-conses nil
  ;; message "while loading %s, there were %d new conses, %d new symbols, %d more string chars"
  "How much storage was allocated by each file loaded.")

(defun load-directory (dir
                       &optional
                       lisp-only
                       before-load-test
                       after-load-function)
  "Load all the el or elc files in DIR.

If the optional second argument is not given, or is nil:
if there are both an elc and an el file for the same base name, load only
the elc file.

If the optional second argument LISP-ONLY is non-nil, load only .el files.

If the optional third argument BEFORE-LOAD-TEST is non-nil, call it on
the filename before loading each file, and only load the file if the
result is non-nil.

If the optional fourth argument AFTER-LOAD-FUNCTION is non-nil, call it
on the filename after loading that file, unless it is 't', in which case
prompt the user to ask whether to load each file."
  (interactive "DDirectory to load emacs files from: 
P")
  (let ((files (directory-files (expand-file-name (substitute-in-file-name dir)) t
				load-directory-pattern))
	(load-compiled (not lisp-only)))
    (message "load-directory: files are %s" files)
    (let ((stack-trace-on-error t))
      (dolist (filename files)
	(message "considering loading %s" filename)
	(when (and (or (and load-compiled ; we can load .elc files
			    (string-match "\\.elc$" filename))
		       ;; there is no <name>.elc corresponding to this
		       ;; <name>.el, or we are not loading .elc files:
		       (not (file-exists-p (concat filename "c"))))
		   (or (null before-load-test)
                       (eq before-load-test t)
                       (funcall before-load-test filename))
                   (or (not (eq before-load-test t))
                       (y-or-n-p (format "Load file %s? " filename))))
	  (condition-case error-var
	      (progn
		(message "Loading %s..." filename)
		(load-file filename)
		(message "Loading %s... done" filename))
	    (error
	     (progn
	       ;; unfortunately, handling it here means we don't get a backtrace!
	       (if (get-buffer "*Backtrace*")
		   (progn
		     (set-buffer  "*Backtrace*")
		     (rename-buffer (format  "*Backtrace-%s*" filename) t)))
	       (if (eq (car error-var) 'file-error)
		   (message "load-path is %S" load-path))
	       (message "Problem in loading %s: %s" filename error-var)
	       (sit-for 2)))
	    )
	  (setq load-directory-loaded (cons filename load-directory-loaded)
		load-directory-bytes (+ load-directory-bytes
					(nth 7 (file-attributes filename)))))
        (when after-load-function
          (funcall after-load-function filename))))))

;;; useful auxiliary function for the above
(defun find-subdirectory-from-path (subdir)
  "Return a full pathname for SUBDIR as a subdirectory of something on load-path"
  (interactive "sFind subdir from path: ")
  (catch 'found
    (let ((lp load-path))
      (while lp
	(let* ((fulldir (expand-file-name (car lp)))
	       (fullsubdir (expand-file-name subdir fulldir)))
	  (if (file-directory-p fullsubdir)
	      (throw 'found fullsubdir)))
	(setq lp (cdr lp))))
    nil))

;;; end of load-directory.el
