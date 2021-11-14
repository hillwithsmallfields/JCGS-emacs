;;; Time-stamp: <2021-11-14 18:34:19 jcgs>

(setq stack-trace-on-error t)

(unless (getenv "SYNCED")
  (setenv "COMMON" "i:/common"))

(load-file (substitute-in-file-name "$SYNCED/projects/focloir/duinnin.el"))
