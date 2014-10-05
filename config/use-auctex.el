;;;; find, load and configure auctex
;;; Time-stamp: <2014-10-05 22:44:59 jcgs>

(fset 'tex-mode nil)
(fset 'latex-mode nil)

(add-to-list 'load-path (substitute-in-file-name "$GATHERED/emacs/auctex/installed/"))
(load-file "$GATHERED/emacs/auctex/installed/tex-site.el")

(use-package tex-site
	     "$GATHERED/emacs/auctex/installed"
	     nil
	     (("\\.tex$" . tex-mode)
	      "$GATHERED/emacs/auctex/installed/auctex"
	      (expand-file-name "graphics" user-emacs-directory)
	      (view-dvi-file "dvi-view" "View DVI-FILE in an Emacs buffer." t))
	     (setq tex-dvi-view-command "/usr/bin/xdvi"
		   tex-dvi-print-command "dvips"
		   TeX-auto-save t
		   TeX-parse-self t
		   TeX-newline-function 'reindent-then-newline-and-indent
		   TeX-auto-untabify t
		   ;; TeX-auto-local "auto/"
		   TeX-lisp-directory (substitute-in-file-name "$GATHERED/emacs/auctex/installed/auctex")
		   find-function-regexp
		   ;; include TeX-defun
		   "^\\s-*(\\(TeX-\\)?\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|\
\[^cgv\W]\\w+\\*?\\)\\|define-minor-mode\
\\|easy-mmode-define-global-mode\\)\\(\\s-\\|\n\\)+'?\
%s\\(\\s-\\|$\\|\(\\|\)\\)")
	     (add-hook 'LaTeX-mode-hook
		       '(lambda ()
			  (TeX-fold-mode 1)
			  (set-fill-column 60)))
	     (load-library "tex.el")
	     (load-library "latex.el"))

(setq-default TeX-master nil)
(put 'TeX-master 'safe-local-variable (lambda (file) (or (eq file t)
							 (not (string-match "/" file)))))

;;; end of use-auctex.el
