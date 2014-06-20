;;;; tags-with-hooks.el
;;; Time-stamp: <2011-05-18 17:02:53 johnstu>

(provide 'tags-with-hooks)
(require 'etags)

(defvar find-tag-before-hooks nil
  "Functions to try for finding a tag, before trying the normal find-tag.")

(defun find-tag-with-hooks (tagname &optional next-p regexp-p)
  "Find a tag, using find-tag-before-hooks or find-tag if none of those respond."
  (interactive (find-tag-interactive "Find tag: "))
  (unless (run-hook-with-args-until-success 'find-tag-before-hooks tagname next-p regexp-p)
    (find-tag tagname next-p regexp-p)))

(global-set-key "\M-." 'find-tag-with-hooks)

;;; end of tags-with-hooks.el
