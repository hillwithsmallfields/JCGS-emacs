;;;; Kiosk-style operation of my agenda
;;; Time-stamp: <2016-01-24 17:32:18 jcgs>

;;; This lets you operate an agenda with very few buttons.

;;; The idea is to have a Raspberry Pi or similar in a case with some
;;; buttons on it, centrally in my house, that I can use to find
;;; things to do and to tick them off as I do them.

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
    (outline-up-heading 1)
    (hide-subtree)))

(defun org-agenda-kiosk-down-or-cycle-level ()
  "Move down a level."
  (interactive)
  (let (child-level)
    (if (save-excursion
	  (setq child-level (org-goto-first-child)))
	(progn
	  (show-children 1)
	  (goto-char child-level))
      (org-todo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode for use over org-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-agenda-kiosk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [ kp-up ] 'org-agenda-kiosk-previous)
    (define-key map [ kp-down ] 'org-agenda-kiosk-next)
    (define-key map [ kp-left ] 'org-agenda-kiosk-up-level)
    (define-key map [ kp-right ] 'org-agenda-kiosk-down-or-cycle-level)
    (define-key map [ kp-begin ] 'org-agenda-kiosk-files-list)
    (define-key map [ kp-prior ] 'org-metaup)
    (define-key map [ kp-next ] 'org-metadown)
    (define-key map [ kp-home ] 'org-priority-up)
    (define-key map [ kp-end ] 'org-priority-down)
    ;; (define-key map [ kp-divide ] ')
    ;; (define-key map [ kp-multiply ] ')
    ;; (define-key map [ kp-subtract ] ')
    ;; (define-key map [ kp-kp-add ] ')
    ;; (define-key map [ kp-enter ] ')
    map)
  "Keymap for the agenda kiosk.")

(define-minor-mode org-agenda-kiosk-mode
  "Minor mode to put kiosk keys onto org-mode."
  nil
  " agenda kiosk"
  'org-agenda-kiosk-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File selection buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-agenda-kiosk-files-buffer nil
  "Buffer containing the agenda files list.")

(defun org-agenda-kiosk-files-list (&optional initial-file)
  "Display the agenda files list, with FILE as current."
  (interactive)
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
	(put-text-property 0 (length title) 'file file title)
	(insert (format "%s\n" title))))
    (org-agenda-kiosk-files-mode))
  (switch-to-buffer org-agenda-kiosk-files-buffer)
  (goto-char (point-min))
  (when initial-file
    (unless (catch 'found 
	      (while (not (eobp))
		(if (equal (get-text-property (point) 'file)
			   initial-file)
		    (throw 'found (point))
		  (beginning-of-line 2))))
      (goto-char (point-min)))))

(defun org-agenda-kiosk-files-select ()
  "Select the file currently pointed to."
  (interactive)
  (let ((file (get-text-property (point) 'file)))
    (if (stringp file)
	(if (file-exists-p file)
	    (find-file file)
	  (error "File %s is missing" file))
      (error "No file specified here"))))

(defvar org-agenda-kiosk-files-map
  (let ((map (make-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map " " 'org-agenda-kiosk-files-select)
    (define-key map [ kp-down ] 'next-line)
    (define-key map [ kp-up ] 'previous-line)
    (define-key map [ kp-right] 'org-agenda-kiosk-files-select)
    (define-key map [ kp-begin ] 'beginning-of-buffer)
    map))

(defun org-agenda-kiosk-files-mode ()
  "A mode for the agenda file selector buffer."
  (interactive)
  (setq goal-column 0
	major-mode 'org-agenda-kiosk-files-mode
	mode-name "Agenda file selector")
  (use-local-map org-agenda-kiosk-files-map))

(defun org-agenda-kiosk-on ()
  "Turn kiosk mode on in this buffer."
  (org-agenda-kiosk-mode 1))

(defun org-agenda-kiosk ()
  "Start running the agenda kiosk."
  (interactive)
  (keypad-setup 'none)
  (add-hook 'org-mode-hook 'org-agenda-kiosk-on)
  (load-file "$EMACS/special-setups/tasks/tasks-emacs-setup.el")
  (org-agenda-kiosk-files-list))
