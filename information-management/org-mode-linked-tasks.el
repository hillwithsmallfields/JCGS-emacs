;;;; linked tasks in org-mode
;;; Time-stamp: <2015-04-27 21:55:20 jcgs>

;; Copyright (C) 2015 John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, tools

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

;; Propagates state changes up trees of tasks (by marking parents as
;; DONE when the last child task is DONE, all the way to the top
;; level), and along lists of tasks (by moving a :next: marker, and
;; propagating markers such as :urgent: as it does so).

;;; Code:

(defun jcgs/org-clock-in-prepare-function ()
  "My customization of task clock-in."
  (save-excursion
    (while (> (funcall outline-level) 1)
      (outline-up-heading 1)
      (when (looking-at org-complex-heading-regexp)
	(let ((state (match-string-no-properties 2)))
	  (when (equal state "TODO")
	    (org-todo "OPEN")))))))

(add-hook 'org-clock-in-prepare-hook 'jcgs/org-clock-in-prepare-function)

(defun jcgs/org-after-todo-state-change-propagate-upwards ()
  "When the last of a set of sibling tasks is marked as DONE,
mark the ancestral tasks as DONE."
  (while (> (funcall outline-level) 1)
    (outline-up-heading 1)
    (let ((not-all-done nil)
	  (on-first t))
      (org-map-entries
       (lambda ()
	 (when (and (not on-first)
		    (org-entry-is-todo-p))
	   (setq not-all-done t))
	 (setq on-first nil))
       nil
       'tree)
      (unless not-all-done
	(org-todo "DONE")))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-propagate-upwards t)

(defvar jcgs/org-after-todo-state-follower-tags
  '("urgent" "soon")
  "Tags which `jcgs/org-after-todo-state-change-move-next-marker'
should move as the \"next\" tag moves.")

(defun jcgs/org-after-todo-state-change-move-next-marker ()
  "If this task is being marked as done, and has a :next: tag, move the tag.
Propagate :urgent: and :soon: tags as needed."
  (let ((original-tags (org-get-tags)))
    (when (and (org-entry-is-done-p)
	       (member "next" original-tags))
      (let ((tags-to-move nil))
	(dolist (maybe jcgs/org-after-todo-state-follower-tags)
	  (if (member maybe original-tags)
	      (push maybe tags-to-move)))
	(org-toggle-tag "next" 'off)
	(beginning-of-line 1)
	(let ((started-at (point)))
	  (org-forward-heading-same-level 1)
	  (if (/= (point) started-at)
	      (progn
		(org-toggle-tag "next" 'on))
	    (when (y-or-n-p "Move :next: marker to next subtree? ")
	      (outline-next-heading)
	      (org-toggle-tag "next" 'on))))
	(dolist (moving-tag tags-to-move)
	  (org-toggle-tag moving-tag 'on))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-move-next-marker)

;;;;;;;;;;;;;;;;;;;;;;
;; chaining entries ;;
;;;;;;;;;;;;;;;;;;;;;;

;; TODO: blocking on multiple tasks?

(defun jcgs/org-setup-chain-task (uuid tag)
  "Set up a chained task.
When the current task is done, onto the task with UUID add the TAG."
  (interactive
   (let ((pair (save-window-excursion
		 (save-excursion
		   (message 
		    (substitute-command-keys
		     "Move to task to chain, press \\[exit-recursive-edit]"))
		   (recursive-edit)
		   (cons (org-id-get nil t)
			 (org-get-buffer-tags))))))
     (list (car pair)
	   (completing-read "Tag: " (cdr pair)))))
  (org-entry-put nil "CHAIN_UUID" uuid)
  (org-entry-put nil "CHAIN_TAG" tag))

(defun jcgs/org-block-task ()
  "Mark the current task as blocked, and link the blocking task to unblock it."
  (interactive)
  (let ((old-tag-state (org-get-todo-state))
	(blocked-uuid (org-id-get nil t)))
    (save-window-excursion
      (save-excursion
	(message
	 (substitute-command-keys
	  "Move to task blocking this one, press \\[exit-recursive-edit]"))
	(recursive-edit)
	(org-entry-put nil "CHAIN_UUID" blocked-uuid)
	(org-entry-put nil "CHAIN_STATE" old-tag-state))))
  (org-todo "BLOCKED"))

(defun jcgs/org-maybe-chain-task ()
  "Activate the next stage of a chain."
  (when (org-entry-is-done-p)
    (let ((chained-task-id (org-entry-get nil "CHAIN_UUID"))
	  (chained-task-tag (org-entry-get nil "CHAIN_TAG"))
	  (chained-task-state (org-entry-get nil "CHAIN_STATE")))
      (when (and chained-task-id (or chained-task-tag chained-task-state))
	(save-window-excursion
	  (save-excursion
	    (org-id-goto chained-task-id)
	    (when chained-task-tag (org-toggle-tag chained-task-tag 'on))
	    (when chained-task-state (org-todo chained-task-state))))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-maybe-chain-task)

(provide 'org-mode-linked-tasks)
