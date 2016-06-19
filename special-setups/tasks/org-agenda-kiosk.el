;;;; Kiosk-style operation of my agenda
;;; Time-stamp: <2016-06-18 20:57:05 jcgs>

;;; This lets you operate an agenda with very few buttons.

;;; This is for use on a small, always-on, home server (Raspberry Pi
;;; or similar) in a case with some buttons on it, centrally in my
;;; house, that I can use to find things to do and to tick them off as
;;; I do them.  Some related software will look for incoming changes
;;; to the agenda files, read the new versions, and produce a new
;;; agenda.

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
      ;; (org-todo) ; put this on a separate key
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode for use over org-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-agenda-portrait-keypad t
  "Whether the keypad has a portrait orientation.")

(defvar org-agenda-kiosk-mode-map
  (let ((map (make-sparse-keymap)))
    (if org-agenda-portrait-keypad
	(progn
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
	  )
      (define-key map [ kp-left ] 'org-agenda-kiosk-previous)
      (define-key map [ kp-right ] 'org-agenda-kiosk-next)
      (define-key map [ kp-up ] 'org-agenda-kiosk-up-level)
      (define-key map [ kp-down ] 'org-agenda-kiosk-down-or-cycle-level)
      (define-key map [ kp-begin ] 'org-agenda-kiosk-files-list)
      (define-key map [ kp-next ] 'org-metaup)
      (define-key map [ kp-end ] 'org-metadown)
      (define-key map [ kp-prior ] 'org-priority-up)
      (define-key map [ kp-home ] 'org-priority-down)

      ;; (define-key map [ kp-divide ] ')
      ;; (define-key map [ kp-multiply ] ')
      ;; (define-key map [ kp-subtract ] ')
      ;; (define-key map [ kp-kp-add ] ')
      ;; (define-key map [ kp-enter ] ')
      )
    (define-key map [ up ] 'org-agenda-kiosk-previous)
    (define-key map [ down ] 'org-agenda-kiosk-next)
    (define-key map [ left ] 'org-agenda-kiosk-up-level)
    (define-key map [ right ] 'org-agenda-kiosk-down-or-cycle-level)
    (define-key map [ prior ] 'org-metaup)
    (define-key map [ next ] 'org-metadown)
    (define-key map [ home ] 'org-priority-up)
    (define-key map [ end ] 'org-priority-down)
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

(defun org-agenda-kiosk-insert-file-index (files)
  "Insert entries for some FILES."
  (dolist (file files)
    (let ((title (save-excursion
		   (find-file file)
		   (save-excursion
		     (goto-char (point-min))
		     (if (re-search-forward "^#+TITLE: \\(.+\\)" (point-max) t)
			 (match-string-no-properties 1)
		       (file-name-nondirectory file))))))
      (let ((title-string (format "** %s\n" title)))
	(put-text-property 0 (length title-string) 'file file title-string)
	(insert title-string)))))

(defvar org-reading-files nil
  "Files to put on my reading and reference list.")

(defun org-agenda-kiosk-files-list (&optional initial-file)
  "Display the agenda files list, with FILE as current."
  (interactive)
  (unless (and (bufferp org-agenda-kiosk-files-buffer)
	       (buffer-live-p org-agenda-kiosk-files-buffer))
    (setq org-agenda-kiosk-files-buffer (get-buffer-create "*Agenda files*"))
    (set-buffer org-agenda-kiosk-files-buffer)
    (erase-buffer)
    (insert "* Files\n")
    (org-agenda-kiosk-insert-file-index org-agenda-files)
    (insert "* Agendas\n")
    (dolist (agenda-command org-agenda-custom-commands)
      (let ((title-string (format "** %s\n" (second agenda-command))))
	(put-text-property 0 (length title-string) 'command-key (first agenda-command) title-string)
	(insert title-string)))
    (insert "* Reading\n")
    (org-agenda-kiosk-insert-file-index org-reading-files)
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
      (let ((command-key (get-text-property (point) 'command-key)))
	(if (stringp command-key)
	    (org-agenda nil command-key) ; todo: put it in a new mode that will define the keys, including one to get back to here, like using org-agenda-kiosk-on
	  (error "Neither file nor agenda specified here"))))))

(defvar org-agenda-kiosk-files-map
  (let ((map (make-keymap)))
    (if org-agenda-portrait-keypad
	(progn
	  (define-key map "n" 'next-line)
	  (define-key map "p" 'previous-line)
	  (define-key map " " 'org-agenda-kiosk-files-select)
	  (define-key map [ kp-down ] 'next-line)
	  (define-key map [ kp-up ] 'previous-line)
	  (define-key map [ kp-right] 'org-agenda-kiosk-files-select)
	  (define-key map [ kp-begin ] 'beginning-of-buffer)
	  (define-key map [ down ] 'next-line)
	  (define-key map [ up ] 'previous-line)
	  (define-key map [ right] 'org-agenda-kiosk-files-select)
	  )
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
      (define-key map " " 'org-agenda-kiosk-files-select)
      (define-key map [ kp-down ] 'next-line)
      (define-key map [ kp-up ] 'previous-line)
      (define-key map [ kp-right] 'org-agenda-kiosk-files-select)
      (define-key map [ kp-begin ] 'beginning-of-buffer)
      (define-key map [ down ] 'next-line)
      (define-key map [ up ] 'previous-line)
      (define-key map [ right] 'org-agenda-kiosk-files-select)
      )
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
  (setq debug-on-error t)
  (keypad-setup 'none)
  (add-hook 'org-mode-hook 'org-agenda-kiosk-on)
  (let ((no-versor t))
    (load-file "$EMACS/special-setups/tasks/tasks-emacs-setup.el"))
  (jcgs/org-agenda-monitor-start)
  (org-agenda-kiosk-files-list))
