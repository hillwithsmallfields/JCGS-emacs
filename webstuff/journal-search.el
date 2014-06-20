;;;; journal-search.el -- search my journal
;;; Time-stamp: <2006-02-10 18:57:39 jcgs>

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

(provide 'journal-search)

(defvar journal-search-day-header "^\\[[0-9][0-9][0-9][0-9]-[a-z]+-[0-9][0-9]\\]$"
  "Regexp for the start of a day.")

(defun journal-make-searchable-file (journal)
  "Prepare a searchable file for JOURNAL."
  (interactive (list (journal-choose-journal "Make searchable version of journal: ")))
  (let* ((journal-dir (eval (cdr (assoc journal journal-dates-directories))))
	 (journal-subdirs (directory-files journal-dir t "^[0-9]+$")))
    (find-file (expand-file-name (concat journal "-all.txt") journal-dir))
    (setq buffer-read-only nil)
    (erase-buffer)
    (while journal-subdirs
      (let ((subdir-files (directory-files (car journal-subdirs) t "[0-9][0-9]-[0-9][0-9]\\.html$")))
	(message "Gathering files from %s" (car journal-subdirs))
	(while subdir-files
	  (message "Adding %s" (car subdir-files))
	  (insert-file (car subdir-files))
	  (goto-char (point-max))
	  (setq subdir-files (cdr subdir-files))))
      (setq journal-subdirs (cdr journal-subdirs)))
    (goto-char (point-min))
    (message "Converting from HTML")
    (while (re-search-forward "<[^>]*>" (point-max) t)
      (replace-match ""))
    (message "Tidying paragraphs")
    (fill-individual-paragraphs (point-min) (point-max))
    (goto-char (point-min))
    (message "Removing junk")
    (flush-lines "\\(^ *$\\)\\|\\(^[A-Z][a-z]+ [0-9][0-9][0-9][0-9]$\\)\\|\\(\\[\\(Last month\\)\\|\\(Next month\\)\\|\\(This year\\)\\|\\(Dates index\\)\\]\\)\\|\\(Last modified\\)\\|\\(^Contact me$\\)\\|\\(^John Sturdy$\\)")
    (message "Tidying into day blocks")
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward journal-search-day-header (point-max) t)
      (replace-match "\n\\&"))
    (message "Done")
    (goto-char (point-min))
    (basic-save-buffer)
    (journal-index-mode)))

(defun journal-search-find-original (where)
  "Find the place in the original file that corresponds to WHERE (point, interactively)."
  (interactive "d")
  (let* ((day-tag (save-excursion
		    (goto-char where)
		    (if (re-search-backward "^\\[\\([0-9][0-9][0-9][0-9]\\)-\\([a-z][a-z][a-z]\\)-\\([a-z]*\\)\\([0-9]+\\)\\]$"
					    (point-min) t)
			(list (match-string-no-properties 1)
			      (match-string-no-properties 2)
			      (match-string-no-properties 4))
		      nil)))
	 (year (first day-tag))
	 (month (second day-tag))
	 (day (third day-tag)))
    (journal-find-start-of-day year month day)))

(defun journal-search-next-day ()
  "Move forward a day."
  (interactive)
  (re-search-forward journal-search-day-header (point-max) 'just-stop)
  (beginning-of-line 2))

(defun journal-search-previous-day ()
  "Move backward a day."
  (interactive)
  (beginning-of-line 0)
  (re-search-backward journal-search-day-header (point-min) 'just-stop)
  (beginning-of-line 2))

(defvar journal-index-mode-map (make-keymap "Journal index")
  "Keymap for journal index mode.")

(suppress-keymap journal-index-mode-map)

(define-key journal-index-mode-map "\C-c\C-c" 'journal-search-find-original)
(define-key journal-index-mode-map "n" 'journal-search-next-day)
(define-key journal-index-mode-map "p" 'journal-search-previous-day)

(defun journal-index-mode ()
  "Mode for journal search index files."
  (kill-all-local-variables)
  (use-local-map journal-index-mode-map)
  (setq buffer-read-only t))

;;; end of journal-search.el
