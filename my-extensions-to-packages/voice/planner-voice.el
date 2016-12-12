;;;; planner-voice.el -- voice commands for planner
;;; Time-stamp: <2006-12-03 23:11:42 jcgs>

(defvar vr-planner-commands
  '( ;; planner itself
    ("make plans" . plan)
    ("show plans" . planner-goto-today-and-current-task)
    ("planner task" . planner-create-task-from-buffer)
    ("done task" . planner-task-done)
    ("cancel task" . planner-task-cancelled)
    ("delete task" . planner-delete-task)
    ("raise task" . planner-raise-task)
    ("lower task" . planner-lower-task)
    ("prioritize task" . planner-raise-task-priority)
    ("deprioritize task" . planner-lower-task-priority)
    ("reschedule task" . planner-copy-or-move-task)
    ("move task" . planner-replan-task)
    ;; remember
    remember
    ;; timeclock
    ("start task" . planner-task-in-progress)
    ("resume task" . planner-task-in-progress)
    ;; ("clock in" . planner-timeclock-in)
    ("clock out" . timeclock-out))
  "Voice commands for planner and friends.")

(provide 'planner-voice)

;;; end of planner-voice.el
