;;;; misc-voice.el -- obscure voice commands
;;; Time-stamp: <2006-10-01 12:35:52 jcgs>

(provide 'misc-voice)

(defvar misc-voice-command-list nil
  "Assorted voice commands that don't get onto lists of their own.")

;; This is set up this way, so that if, by the time we load this file,
;; misc-voice-command-list already has a value, the old value is kept.
;; This lets us put values onto it in things run from mounted USB
;; drives, etc, so they get a chance to add to the voice commands.
(setq misc-voice-command-list
      (append misc-voice-command-list
	      '(
		("remark" . editing-behaviour-log-remark)
		("show to do list" . todo-show)
		("something to do" . todo-insert-item)
		("find paper" . flexi-choose-paper)
		("list papers" . papers-list)
		("skip to code" . skip-to-actual-code)
		show-citation
		)))

;;; end of misc-voice.el
