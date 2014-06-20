;;; Time-stamp: <2007-05-01 17:01:35 jcgs>

(setq stack-trace-on-error t)

(unless (getenv "COMMON")
  (setenv "COMMON" "i:/common"))

(load-file (substitute-in-file-name "$COMMON/projects/focloir/duinnin.el"))
