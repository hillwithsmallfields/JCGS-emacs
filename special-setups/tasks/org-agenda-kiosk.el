;;;; Kiosk-style operation of my agenda
;;; Time-stamp: <2016-01-23 21:08:12 jcgs>

;;; This lets you operate an agenda with very few buttons.

;;; The idea is to have a Raspberry Pi or similar in a case with some
;;; buttons on it, centrally in my house, that I can use to find
;;; things to do and to tick them off as I do them.

(load-file "$EMACS/basics/tasks-emacs-setup.el")

(defun org-agenda-kiosk-next ()
  "Move to the next entry."
  (interactive)
  (outline-next-visible-heading 1))

(defun org-agenda-kiosk-previous ()
  "Move to the previous entry."
  (interactive)
  (outline-previous-visible-heading 1))

(defun org-agenda-kiosk-up-level ()
  "Move up a level."
  (interactive)
  (if (<= (org-outline-level) 1)
      (org-agenda-kiosk-files-list (buffer-file-name))
    (outline-up-heading 1)))

(defun org-agenda-kiosk-down-or-cycle-level ()
  "Move down a level."
  (interactive)
  (let (child-level)
    (if (save-excursion
	  (setq child-level (org-goto-first-child)))
	(progn
	  (show-children 1)
	  (goto child-level))
      ;; todo: cycle the keyword
      )))

(defvar org-agenda-kiosk-files-buffer nil
  "Buffer containing the agenda files list.")

(defun org-agenda-kiosk-files-list (initial-file)
  "Display the agenda files list, with FILE as current."
  (interactive (list nil))
  (unless (and (bufferp org-agenda-kiosk-files-buffer)
	       (buffer-live-p org-agenda-kiosk-files-buffer))
    (setq org-agenda-kiosk-files-buffer (get-buffer-create "*Agenda files*"))
    (set-buffer org-agenda-kiosk-files-buffer)
    (erase-buffer)
    (dolist (file org-agenda-files)
      (let ((title (save-excursion
		     (find-file file)
		     (save-excursion
		       (goto-char (point-min))
		       (if (re-search-forward "^#+TITLE: \\(.+\\)" (point-max) t)
			   (match-string-no-properties 1)
			 (file-name-nondirectory file)))
		     )))
	(insert (format "%s %s\n" file title)))))
  (switch-to-buffer org-agenda-kiosk-files-buffer)
  (goto-char (point-min))
  (when initial-file
    ;; todo: find initial file
    ))

