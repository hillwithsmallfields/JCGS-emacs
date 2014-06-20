;;; use-flashcard.el --- load flashcards
;; Time-stamp: <2014-01-05 13:33:43 jcgs>

(defvar flashcard-questions-answered 0
  "How many questions the user has answered in this session.")

(defvar flashcard-questions-answered-string "no"
  "How many questions the user has answered in this session, as a string.")

(defvar flashcard-questions-answered-correctly 0
  "How many questions the user has answered in this session.")

(defvar flashcard-questions-answered-incorrectly 0
  "How many questions the user has answered in this session.")

(defvar flashcard-using-recursive-edit nil
  "This is bound to non-nil when doing a recursive edit specially for flashcard practice.")

(defvar flashcard-recursive-edit-target 0
  "The total number of questions at which to stop.")

(defvar flashcard-latest-language-code nil
  "The most recently used flashcard code.")

(defvar flashcard-languages-with-answers nil
  "List of languages with at least one question answered.")

(defvar flashcard-n-languages-with-answers-string "no"
"Number of languages with at least one question answered, as a string.")

(defvar flashcard-languages-with-correct-answers nil
  "List of languages with at least one question answered correctly.")

(defvar flashcard-languages-with-incorrect-answers nil
  "List of languages with at least one question answered correctly.")

(defun flashcard-ask-N-questions (file n)
  "From FILE, ask the user N questions."
  (interactive "fFlashcard file: 
nAsk how many questions: ")
  (setq flashcard-recursive-edit-target (+ flashcard-questions-answered n))
  (save-window-excursion
    (save-excursion
      (find-file file)
      (recursive-edit))))

(defun flashcard-count-questions (result)
  "Count questions.  Success was RESULT."
  (setq flashcard-questions-answered (1+ flashcard-questions-answered)
	flashcard-questions-answered-string (int-to-string flashcard-questions-answered))
  (add-to-list 'flashcard-languages-with-answers flashcard-latest-language-code)
  (setq flashcard-n-languages-with-answers-string (int-to-string (length flashcard-languages-with-answers)))
  (if (if (numberp result)
	  (>= result 4)
	result)
      (progn
	(setq flashcard-questions-answered-correctly (1+ flashcard-questions-answered-correctly))
	(add-to-list 'flashcard-languages-with-correct-answers flashcard-latest-language-code))
    (setq flashcard-questions-answered-incorrectly (1+ flashcard-questions-answered-incorrectly))
    (add-to-list 'flashcard-languages-with-incorrect-answers flashcard-latest-language-code))
  (message "%d questions answered (%d right, %d wrong) in %d languages (%d, %d)"
	   flashcard-questions-answered
	   flashcard-questions-answered-correctly
	   flashcard-questions-answered-incorrectly
	   (length flashcard-languages-with-answers)
	   (length flashcard-languages-with-correct-answers)
	   (length flashcard-languages-with-incorrect-answers))
  (when (and flashcard-using-recursive-edit
	     (>= flashcard-questions-answered
		 flashcard-recursive-edit-target))
    (bury-buffer nil)
    (exit-recursive-edit)))

(add-hook 'flashcard-positive-feedback-functions
	  'flashcard-count-questions
	  t)

(defun flashcard-mark-seen (result)
  "Mark the current card as having been seen.
Ignore RESULT."
  (let ((old (flashcard-card-get-note flashcard-card 'js-seen)))
    (flashcard-card-set-note flashcard-card 'js-seen
			     (if (numberp old)
				 (1+ old)
			       1))))

(add-hook 'flashcard-positive-feedback-functions
	  'flashcard-mark-seen)

(defun flashcard-add-mode-line-question-count ()
  "Add the question count to the modeline."
  (interactive)
  (unless (assq 'flashcard-questions-answered-string mode-line-format)
    (let* ((the-format (copy-sequence mode-line-format))
	   (l (memq 'mode-line-modes the-format)))
      (rplacd l (cons 'flashcard-questions-answered-string
		      (cons " answered in "
			    (cons 'flashcard-n-languages-with-answers-string
				  (cons " languages"
					(cdr l))))))
      (setq mode-line-format the-format))))

(defvar flashcard-relearn-list nil
  "List of cards the user should be shown so they can re-learn them.")

(defun flashcard-add-to-relearn-list (result)
  "If RESULT is nil (user got it wrong) add the latest question to a list.
This list is displayed in various circumstances, such as typing breaks."
  (when (not result)
    (push flashcard-card
	  flashcard-relearn-list)))

(add-hook 'flashcard-positive-feedback-functions
	  'flashcard-add-to-relearn-list
	  t)

(defun flashcard-show-relearn-list ()
  "Show the questions and answers that the user has got wrong."
  (interactive)
  (with-output-to-temp-buffer "*For relearning*"
    (let ((item-format (format "%% %ds | %%s\n"
			       (apply 'max
				      (mapcar 'length
					      (mapcar 'flashcard-card-question
						      flashcard-relearn-list))))))
      (dolist (item flashcard-relearn-list)
	(princ (format item-format
		       (flashcard-card-question item)
		       (flashcard-card-answer item)))))))

(defvar flashcard-question-prefix ""
  "*String to put at the start of each question as they are read.")

(defun flashcard-set-question-prefix (new-prefix)
  "Set the question prefix to NEW-PREFIX."
  (interactive "sQuestion prefix: ")
  (setq flashcard-question-prefix new-prefix))

(defun flashcard-add-language-card (deck card)
  "To DECK add CARD."
  (interactive (list flashcard-deck
                     (flashcard-make-card
                      (read-from-minibuffer "Question: " flashcard-question-prefix)
                      (read-from-minibuffer "Answer: "))))
  (flashcard-method-initialize-card card)
  (flashcard-deck-set-cards deck
                            (cons card
                                  (flashcard-deck-cards deck)))
  (set-buffer-modified-p t)
  (when (and (interactive-p)
             (not flashcard-card))
    (flashcard-ask)))

(defvar flashcard-sm5-learnt-threshold 4
  "How many times a card should be counted as done successfully, to count as learnt.")

(defun flashcard-output-stats (cards by-number &optional html)
  "Output stats for CARDS sorted BY-NUMBER else alphabetically.
Optional argument HTML sets output format to HTML."
  (princ (if html
	     "<table><tr><th align=\"right\">Language</th><th  align=\"right\">total</th><th  align=\"right\">learnt</th><th  align=\"right\">learning</th><th  align=\"right\">waiting</th></tr>\n"
	   "In this deck:\nCounts: total learnt learning waiting\n"))
  (let ((first-word-counts nil))
    (dolist (card cards)
      (let* ((question (flashcard-card-question card))
	     (first-word (substring question 0 (and
						(string-match "^[A-Z]+" question)
						(match-end 0))))
	     (fw-pair (assoc first-word first-word-counts)))
	(if fw-pair
	    (let* ((total-cell (cdr fw-pair))
		   (subtotals (cadr total-cell)))
	      (rplaca total-cell (1+ (car total-cell)))
	      (if (flashcard-card-get-note card 'js-seen)
		  (if (> (flashcard-card-get-note card 'sm-count) 0)
		      (aset subtotals 0 (1+ (aref subtotals 0)))
		    (aset subtotals 1 (1+ (aref subtotals 1))))
		(aset subtotals 2 (1+ (aref subtotals 2)))))
	  (setq fw-pair (list first-word 1 (vector
					    ;; todo: one of these should be 1
					    (if (and (flashcard-card-get-note card 'js-seen)
						     (> (flashcard-card-get-note card 'sm-count) flashcard-sm5-learnt-threshold)) 1 0)
					    (if (and (flashcard-card-get-note card 'js-seen)
						     (<= (flashcard-card-get-note card 'sm-count) flashcard-sm5-learnt-threshold)) 1 0)
					    (if (flashcard-card-get-note card 'js-seen)
						0 1)))
		first-word-counts (cons fw-pair first-word-counts)))))
    (setq first-word-counts (sort first-word-counts
				  (if by-number
				      (if t
					  (lambda (a b) (> (aref (caddr a) 0) (aref (caddr b) 0)))
					(lambda (a b) (> (cadr a) (cadr b))))
				    (lambda (a b) (string< (car a) (car b))))))
    (dolist (count first-word-counts)
      (let ((subs (caddr count)))
	(princ (format (if html
			   "  <tr><th align=\"right\">%s</th><td align=\"right\">%d</td><td align=\"right\"><b>%d</b></td><td align=\"right\">%d</td><td align=\"right\">%d</td></tr>\n"
			 "%s:    %5d  %5d    %5d   %5d\n")
		       (car count) (cadr count) (aref subs 0) (aref subs 1) (aref subs 2)))))
    (princ (format (if html
		       "</table><p>Total: %d questions in %d languages, average %g questions per language.</p>\n"
		     "Total: %d questions in %d languages, average %g questions per language\n")
		   (length cards)
		   (length first-word-counts)
		   (/ (length cards)
		      (length first-word-counts))))

    (cond
     ((eq flashcard-method 'leitner)
      (princ "\nIn this session:\n")
      (princ (format "Languages with at least one right answer: %s\n"
		     flashcard-languages-with-correct-answers))
      (princ (format "Languages with at least one wrong answer: %s\n"
		     flashcard-languages-with-incorrect-answers))
      (princ (format "Languages with all correct answers: %s\n"
		     (set-difference flashcard-languages-with-correct-answers
				     flashcard-languages-with-incorrect-answers
				     :test 'equal)))
      (princ (format "Languages with all incorrect answers: %s\n"
		     (set-difference flashcard-languages-with-incorrect-answers
				     flashcard-languages-with-correct-answers
				     :test 'equal))))
     ((eq flashcard-method 'sm5)
      (let ((learnt 0)
	    (learning 0)
	    (waiting 0))
	(dolist (card cards)
	  (if (flashcard-card-get-note card 'js-seen)
	      (if (> (flashcard-card-get-note card 'sm-count) 0)
		  (setq learnt (1+ learnt))
		(setq learning (1+ learning)))
	    (setq waiting (1+ waiting))))
	(princ (format (if html
			   "<p>Learnt: %d<br>\nlearning: %d<br>\nwaiting: %d</p>\n"
			 "learnt: %d\nlearning: %d\nwaiting: %d")
		       learnt learning waiting)))))))

(defun flashcard-stats (&optional by-numbers)
  "Display the stats for the current deck.
Optional argument BY-NUMBERS says to sort by numbers of
questions, not alphabetically."
  (interactive "P")
  (with-output-to-temp-buffer "*Flashcard stats*"
    (flashcard-output-stats (flashcard-deck-cards flashcard-deck)
			    by-numbers)))

(defun flashcard-stats-html (&optional by-numbers)
  "Display the stats for the current deck as HTML.
Optional argument BY-NUMBERS says to sort by numbers of
questions, not alphabetically."
  (interactive "P")
  (with-output-to-temp-buffer "*Flashcard stats*"
    (flashcard-output-stats (flashcard-deck-cards flashcard-deck)
			    by-numbers
			    t)))

(defun flashcard-list-deck ()
  "List the current deck."
  (interactive)
  (let* ((getter (cond
		  ((eq flashcard-method 'sm5)
		   (lambda (card)
		     (let ((js-seen (flashcard-card-get-note card 'js-seen))
			   (sm-count (flashcard-card-get-note card 'sm-count)))
		       (propertize (flashcard-card-question card)
				   'face
				   (cons 'foreground-color
				   (if js-seen
				       (if (> sm-count 3)
					   "green"
					 "orange")
				     "red"))))))
		  (t 'flashcard-card-question)))
	 (questions (sort (mapcar getter
				  (flashcard-deck-cards flashcard-deck))
			  'string<)))
    (message "Q %S" questions)
    (let ((list-buffer (get-buffer-create "*Flashcards*")))
      (pop-to-buffer list-buffer)
      (set-buffer list-buffer)
      (toggle-read-only -1)
      (dolist (question questions)
	(insert question "\n"))
      ;; (flashcard-output-stats (flashcard-deck-cards flashcard-deck) nil)
      (toggle-read-only 1))))

(defvar jcgs-input-methods
  '(
    ("CMN" . chinese-tonepy)
    ("CZC" . czech-qwerty)
    ("FIN" . finnish-postfix)
    ("FRN" . french-prefix)
    ("GER" . german-postfix)
    ("GLI" . irish-prefix)
    ("HBR" . hebrew)
    ("ICE" . icelandic-keyboard)
    ("JPN" . japanese)
    ("KHK" . mongolian-cyrillic)
    ("MLS" . maltese-prefix)
    ("NRR" . norwegian-postfix)
    ("PQL" . polish-slash)
    ("PRT" . portuguese-prefix)
    ("RUM" . romanian-prefix)
    ("RUS" . russian-computer)
    ("SWD" . swedish-postfix)
    )
  "Which input method to use for each of my languages.")

(defun jcgs-flashcard-post-question-function ()
  "Function to run when a question has been added."
  (let* ((code (save-excursion
		 (if (re-search-backward "^\\([A-Z][A-Z][A-Z]\\) " (point-min) t)
		     (match-string-no-properties 1)
		   nil)))
	 (input-method (cdr (assoc code jcgs-input-methods))))
	 ;; (message "Setting up for %s: %s" code input-method)
    (setq flashcard-latest-language-code code)
    (activate-input-method input-method)))

(add-hook 'flashcard-post-question-hook 'jcgs-flashcard-post-question-function)

(defun flashcard-remove-duplicates ()
  "Remove duplicates in the deck.
Which of the duplicates is kept is effectively arbitrary."
  (interactive)
  (unless (eq major-mode 'flashcard-mode)
    (error "Not in a flashcard buffer!"))
  (let* ((cards (flashcard-edit-get-sorted-cards flashcard-deck))
	 (original-length (length cards))
	 (cs cards)
	 (c (car cs))
	 (cq (flashcard-card-question c))
	 (ca (flashcard-card-answer c)))
    (while (and cs (cdr cs))
      (let* ((d (cadr cs))
	     (dq (flashcard-card-question d))
	     (da (flashcard-card-answer d)))
	(if (and (string= cq dq)
		 (string= ca da))
	    (progn
	      (message "Removing %s : %s" cq ca)
	      (rplacd cs (cddr cs)))
	  (setq cs (cdr cs)
		c d
		cq dq
		ca da))))
    (message "Length was %d, is now %d" original-length (length cards))
    (flashcard-deck-set-cards flashcard-deck cards)))


(defvar flashcard-extra-map
  (let ((map (make-keymap "Flashcard extra")))
    (define-key map "a" 'flashcard-add-language-card)
    (define-key map "d" 'flashcard-remove-duplicates)
    (define-key map "e" 'flashcard-edit-current-deck)
    (define-key map "h" 'flashcard-stats-html)
    (define-key map "i" 'flashcard-import-from-colon-file)
    (define-key map "l" 'flashcard-list-deck)
    (define-key map "p" 'flashcard-set-question-prefix)
    (define-key map "r" 'flashcard-show-relearn-list)
    (define-key map "s" 'flashcard-stats)
    (define-key map "u" 'flashcard-shuffle-deck)
    map))

(fset 'flashcard-extra-map flashcard-extra-map)

(defvar flashcard-method-sm5-js-answer-code-meanings
  '((?0 . 0)
    (?1 . 1)
    (?2 . 2)
    (?3 . 3)
    (?4 . 4)
    (?5 . 5)
    (PinkieBtn-up . 0)
    (Trigger-up . 1)
    (ThumbBtn-up . 2)
    (ThumbBtn2-up . 3)
    (TopBtn-up . 4)
    (BaseBtn2-up . 5))
  "Alist of meanings of reply codes.")

(defun flashcard-method-sm5-js-check-answer (card answer)
  "Insert the answer, ask the user for a quality point.
This refers to CARD and the given ANSWER."
  (flashcard-insert "The correct answer is:\n"
                    (propertize (flashcard-card-answer card)
                                'face 'flashcard-answer-face
                                'rear-nonsticky t) "\n")
  (catch 'done
    (while t
      (let* ((code (read-event "Quality of your answer (0-5): "))
	     (meaning (cdr (assoc code flashcard-method-sm5-js-answer-code-meanings))))
	(when meaning
	  (message "Quality %d" meaning)
	  (throw 'done meaning))))))


(defun flashcard-method-sm5-js-correct-p (code)
  "Indicate whether CODE indicates a correct answer."
  (>= code 4))

(defun flashcard-method-sm5-js ()
  "A joystick-extended version of flashcard-method-sm5."
  (interactive)
  (flashcard-method-sm5)
  (setq flashcard-method-check-answer-function 'flashcard-method-sm5-js-check-answer
	flashcard-method-correct-p-function 'flashcard-method-sm5-js-correct-p)
  (define-key flashcard-mode-map [ PinkieBtn-up ] 'flashcard-input))

(require 'use-package)

(use-package flashcard
	     "$GATHERED/emacs/flashcard/"
	     "http://repose.cx/flashcard/"
	     ("$GATHERED/emacs/flashcard/"
	      ("\\.deck\\'" . flashcard-mode)
	      (flashcard-mode "flashcard" t "Mode for flashcards")
	      (flashcard-method-sm5 "flashcard-sm5" t "Use the SuperMemo 5 method in flashcard.")
	      )
	     (setq flashcard-coding-system 'utf-8)
	     (set-face-foreground 'flashcard-question-face "blue")
	     (set-face-attribute 'flashcard-question-face nil :height 400)
	     (set-face-attribute 'flashcard-answer-face nil :height 400)
	     (set-face-foreground 'flashcard-answer-face "dark green")
	     (add-hook 'flashcard-mode-hook
		       (lambda ()
			 (define-key flashcard-mode-map "\C-c" 'flashcard-extra-map)
			 (flashcard-method-sm5-js)))
	     (add-hook 'flashcard-mode-hook
		       'flashcard-add-scroll-to-bottom)
	     (add-hook 'flashcard-mode-hook
		       'flashcard-add-mode-line-question-count)
	     (add-hook 'flashcard-positive-feedback-functions
		       'flashcard-feedback-highlight-answer)
	     (add-hook 'flashcard-positive-feedback-functions
		       'flashcard-feedback-congratulate)
	     (when nil
	       (add-hook 'flashcard-positive-feedback-functions
			 'flashcard-method-leitner-positive-feedback))
	     )

;;; use-flashcard.el ends here
