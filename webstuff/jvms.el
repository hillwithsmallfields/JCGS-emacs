;;;; jvms.el -- convert Dad's old word files
;;; Time-stamp: <03/03/01 18:29:41 jcgs>

(defun convert-dads-word-file (file)
  "Convert FILE."
  (interactive "fFile to convert: ")
  (let* ((basename (substring file 0 (string-match "\\.doc" file)))
	 (htmlname (concat basename ".html")))
    (if (file-exists-p htmlname)
	(message "already done %s" basename)
    (find-file htmlname)
    (erase-buffer)
    (insert-file-contents file)
    (delete-region 1 129)
    )))
