
(let ((possible-slime-directory "/usr/share/emacs/site-lisp/slime/"))
  (message "Looking for %s" possible-slime-directory)
  (when (file-directory-p possible-slime-directory)
    (message "Configuring slime using %s" possible-slime-directory)
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (add-to-list 'load-path possible-slime-directory)
    (require 'slime-autoloads)
    (slime-setup)))

