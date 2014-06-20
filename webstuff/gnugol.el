;; gnugol.el - Web search using the gnugol command line utility
;; Copyright (C) 2010 Dave Täht
;; Some parts (gnugol-mode) Copyright (C) 2012, 2013 John Sturdy
;; License:    GNU Public License, version 3
;; Author:     Dave Taht, John Sturdy (gnugol-mode)
;; Maintainer: d + gnugol AT taht.net
;; Created:    Dec-2008
;; Version:    See git tree
;; Keywords:   extensions, web, search, google

;; This is an interface to the gnugol command line
;; web search utility, which can be obtained at:
;; http://gnugol.taht.net

;; I find gnugol useful enough to stick on a function key
;; in my keybindings.el file elsewhere.
;; (define-key global-map [f6] 'gnugol)

;; FIXME: Convert all to defcustom and add support for args

(defcustom gnugol-cmd "gnugol"
  "Shell command to invoke gnugol."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-opts nil
  "Additional default options for gnugol."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-engine nil
  "Default search engine backend for gnugol. Presently supported are:
  google: Full support (license key recommended)
  bing: (with a license key)
  dummy: (useful for testing)
  credits: various options like about, licenses etc."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-nresults 8
  "Default number of results for gnugol to return."
  :type 'integer
  :group 'gnugol)

(defcustom gnugol-default-safe-mode "1"
  "Default safe mode to search in: 0 = none, 1 = moderate, 2 = active"
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-header "0"
  "Default output header for gnugol. 0 = no header. "
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-footer "0"
  "Default output footer for gnugol. 0 = no footer"
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-output-format "org"
  "Default output format for gnugol."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-output-buffer "*gnugol*"
  "Output buffer. Set this to something like ~/org/gnugol_history.org if you want to keep your history."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-input-language nil
  "Set this to your preferred language 2 character code if you want to override the LANG variable in your environment."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-output-language nil
  "Set this to the preferred language 2 character code to **restrict** the results you get to this language."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-search-maxlen 200
  "Maximum string length of search term. This saves on sending accidentally large queries to the search engine."
  :type 'integer
  :group 'gnugol)

(defcustom gnugol-default-search-maxwords 4
  "Maximum number of words to search for."
  :type 'integer
  :group 'gnugol)

(defcustom gnugol-default-output-mode-sensitive nil
  "Be sensitive to the current buffer mode. Output results in that format."
  :type 'boolean
  :group 'gnugol)

(defcustom gnugol-default-timestamp-output nil
  "Timestamp the output."
  :type 'boolean
  :group 'gnugol)

;; FIXME Haven't decided if doing a ring buffer would be useful

(defcustom gnugol-ring-max 16
  "Maximum length of search ring before oldest elements are thrown away."
  :type 'integer
  :group 'gnugol)

;; FIXME figure out how to search the buffer-modes

;; (defun gnugol-get-output-mode
;;  "Get the gnugol output mode from the current buffer mode."
;;  (if gnugol-default-output-mode-sensitive 
;;      () 
;;    ("org")))


(defun gnugol-url-encode (str)
  "URL-encode STR."
  (interactive "sURL-encode: ")
  (message "%s" (url-hexify-string str)))

(defun gnugol-url-decode (str)
  "URL-decode STR."
  (interactive "sURL-decode: ")
  (message "%s" (decode-coding-string
		 (url-unhex-string str)
		 'utf-8)))

;; FIXME: gnugol should act more like "woman"
;; FIXME: gnugol should maybe output info and LISP format
;; FIXME: If there is a visible gnugol buffer change focus to that rather than the current
;; FIXME: Add hooks for further washing the data

;; FIXME: add next, prev, and refresh buttons 
;        [[gnugol: :next :pos 4 str][more]] [[gnugol: prev][prev]] [[refresh]]
;; FIXME: Document this shell shortcut into org or write org mode version
;; FIXME: search forward in the buffer for an existing
;;        set of keywords rather than call gnugol
;;        (if (search-forward (concat "[[Search: " str )) () (gnugol-cmd str)))?
;; FIXME: Sanitize the shell arguments to prevent abuse !! For example no CRs
;;        regexp? I'm *nearly* certain that url escaping and shell quoting the args
;;        is good enough.
;; FIXME: actually, going to the 4th char in on the title would work best
;; FIXME: make gnugol opts be local
;; FIXME: CNTR-U should set the position
;; FIXME: a query with the same keywords as the previous should fetch more 
;;        results (maybe)

(defun gnugol-other-window (str &optional engine noselect)
  "Search the web for STR via gnugol, putting results in another window.
Optional second argument ENGINE is the search engine to use.
Optional third argument NOSELECT means don't select the window."
  (save-window-excursion
    (gnugol str engine))
  (funcall (if noselect 'display-buffer 'pop-to-buffer)
	   gnugol-default-output-buffer))

(defun gnugol (str &optional engine)
  "Search the web for STR via gnugol, bring up results in org buffer.
Optional argument ENGINE is the search engine to use."
  (interactive
   (list
    (read-from-minibuffer "Search for: "
			  (thing-at-point 'symbol))))
  (if (< (length str) gnugol-search-maxlen)
      (let (newbuffer)
	(setq gnugol-opts
	      (concat (if gnugol-default-opts
			  (concat gnugol-default-opts)
			nil)
		      (if gnugol-default-nresults
			  (concat " -n "
				  (int-to-string gnugol-default-nresults))
			nil)
		      (if engine
			  (concat " -e " engine)
			(if gnugol-default-engine
			    (concat " -e " gnugol-default-engine)))
		      (if gnugol-default-output-format
			  (concat " -o " gnugol-default-output-format)
			nil)
		      (if gnugol-default-header
			  (concat " -H " gnugol-default-header)
			nil)
		      (if gnugol-default-footer
			  (concat " -F " gnugol-default-footer)
			nil)
		      ;; (if gnugol-default-safe-mode (concat " -S " gnugol-default-safe-mode) nil)
		      (if gnugol-default-input-language
			  (concat " -l " gnugol-default-input-language)
			nil)
		      (if gnugol-default-output-language
			  (concat " -L " gnugol-default-output-language)
			nil)
		      ))
	(setq gnugol-full-cmd  (concat gnugol-cmd " " gnugol-opts " -U -- "
				       (shell-quote-argument
					(gnugol-url-encode str))))
	;; FIXME: Open the file, or reload the file if not a *gnugol* buffer
	(setq newbuffer (get-buffer-create gnugol-default-output-buffer))
	(switch-to-buffer newbuffer)
	;; FIXME: Set mode of buffer based on the extension
	(gnugol-mode)
	(goto-char (point-min))
	;; FIXME what we want to do is something like this but I'm getting it wrong
	;; (if (search-forward (concat "[Search: " str "]")) ()
	;;	(message "%s" gnugol-full-cmd)
	(let ((buffer-read-only nil))
	  (save-excursion
	    (insert "* [[gnugol: " str "][Search: " str "]]\n"
		    (shell-command-to-string gnugol-full-cmd)))))
    (error "Search string too long")))

(defun gnugol-search-selection ()
  "Do a gnugol search based on a region"
  (interactive)
  (let (start end term url)
    (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
        (progn
          (setq start (region-beginning)
                end   (region-end))
          (if (> (- start end) gnugol-search-maxlen)
              (setq term (buffer-substring start (+ start gnugol-search-maxlen)))
            (setq term (buffer-substring start end)))
          (gnugol term))
      (beep)
      (message "Region not active"))))

(defun gnugol-search-dummy (str)
  "Search the dummy engine via gnugol. (Useful for debugging)"
  (interactive "sSearch: ")
  (gnugol str "dummy"))

(defun gnugol-search-credits (str)
  "Search the local credits engine via gnugol."
  (interactive "sSearch: ")
  (gnugol str "credits"))

(defun gnugol-search-bing (str)
  "Search bing via gnugol."
  (interactive "sSearch: ")
  (gnugol str "bing"))

(defun gnugol-search-stackapps (str)
  "Search stackapps via gnugol."
  (interactive "sSearch: ")
  (gnugol str "stackapps"))

(defun gnugol-search-google (str)
  "Search google via gnugol."
  (interactive "sSearch: ")
  (gnugol str "google"))

;; This are examples of using a site specific search

(defun gnugol-search-emacswiki(str)
  "Search emacswiki via gnugol."
  (interactive "sSearch: ")
  (gnugol-search-google (concat "site:www.emacswiki.org " str)))

(defun gnugol-search-gnugol(str)
  "Search gnugol site via gnugol."
  (interactive "sGnugol Search: ")
  (gnugol-search-google (concat "site:gnugol.taht.net " str)))

(defun gnugol-search-koders(str)
  "Search koders.com site via gnugol."
  (interactive "sSearch Koders: ")
  (gnugol-search-google (concat "site:www.koders.com " str)))

(defun gnugol-search-stackapps-google(str)
  "Search stackoverflow site via gnugol."
  (interactive "sSearch stackapps: ")
  (gnugol-search-google (concat "site:stackoverflow.com " str))
  )

;; It would be nice to do the above via command completion
;; (gnugol-search-site arg, arg), but this isn't right
;; I need to prepend args somehow
;; (defun gnugol-search-site(str)
;;   "Search any specific site via gnugol."
;;   (interactive "site: ")
;;   (gnugol-search-google (concat "site:" str))
;;   )

;; Make it a mode of its own, with a command to fetch more detail

(define-derived-mode gnugol-mode org-mode "Gnugol"
  "Major mode for displaying gnugol results.
Derived from `org-mode', with the following extra commands:
\\<gnugol-mode-map>
\\[gnugol-fetch-page-as-text]	Fetch this entry as full text
\\[hide-subtree]	Fold the current entry away
\\[gnugol-copy-page-url]	Copy the current entry's URL to kill ring
\\[gnugol]	Ask another question
\\[bury-buffer]	Hide this buffer
\\[scroll-up]	Move down the document
\\[scroll-down]	Move up the document
\\[hide-body]	Hide the text, showing just the headings
\\[outline-next-visible-heading]	Move to the next heading
\\[outline-previous-visible-heading]	Move to the previous heading
\\[outline-up-heading]	Go up a level of headings
\\[outline-backward-same-level]	Go back to the previous heading at this level
\\[outline-forward-same-level]	Go forward to the next heading at this level
"
  ;; (view-mode 1)
  )

(suppress-keymap gnugol-mode-map)

(define-key gnugol-mode-map "a" 'gnugol)
(define-key gnugol-mode-map "g" 'gnugol-fetch-page-as-text)
(define-key gnugol-mode-map "r" 'gnugol-copy-page-url)
(define-key gnugol-mode-map "q" 'bury-buffer)
(define-key gnugol-mode-map "c" 'hide-subtree)
(define-key gnugol-mode-map "h" 'hide-body)

(define-key gnugol-mode-map " " 'scroll-up)
(define-key gnugol-mode-map "\d" 'scroll-down)

(define-key gnugol-mode-map "p" 'outline-previous-visible-heading)
(define-key gnugol-mode-map "n" 'outline-next-visible-heading)

(define-key gnugol-mode-map "b" 'outline-backward-same-level)
(define-key gnugol-mode-map "f" 'outline-forward-same-level)
(define-key gnugol-mode-map "u" 'outline-up-heading)

(defun gnugol-current-header-url ()
  "Return the current gnugol header url."
  (cond
   ((org-in-regexp org-bracket-link-regexp)
    (progn
      (let (link)
	(setq link (org-extract-attributes
		    (org-link-unescape (org-match-string-no-properties 1))))
	(while (string-match " *\n *" link)
	  (setq link (replace-match " " t t link)))
	(setq link (org-link-expand-abbrev link)))))
   ((save-excursion
      (beginning-of-line 1)
      (looking-at outline-regexp))
    (re-search-forward org-bracket-link-regexp)
    (goto-char (1+ (match-beginning 0)))
    (gnugol-current-header-url))
   (t
    (re-search-backward org-bracket-link-regexp)
    (goto-char (1+ (match-beginning 0)))
    (gnugol-current-header-url))
   nil))

(defun gnugol-fetch-page-as-text (url)
  "Fetch URL as text, and put it in the gnugol buffer."
  (interactive (list (gnugol-current-header-url)))
  (save-excursion
    (beginning-of-line 2)
    (let ((start (point)))
      (call-process "w3m" nil (current-buffer) nil "-dump" url)
      (indent-rigidly start (point) 3))))

(defun gnugol-copy-page-url ()
  "Put the URL of this page onto the kill ring."
  (interactive)
  (let ((url (gnugol-current-header-url)))
    (kill-new url)
    (message "URL %s saved to kill ring" url)))

;; And this is just around so that I test output formats
;; It suffers because I would like it to toss output
;; into a buffer formatted in that format...

(defun gnugol-test(str)
  "Output gnugol's test data."
  (interactive "sMode: ")
  (let (gnugol-default-output-format)
    (setq gnugol-default-output-format str)
    (gnugol-search-dummy "all")
    )
)

;; FIXME: I'd really like a way to split the current window 
;;        at 80 characters and bring up the search on the 
;;        right. AND override my default url opener to be
;;        the internal emacs web browser for sites on a
;;        whitelist.
;; FIXME: NOTHING BELOW HERE ACTUALLY WORKS at all YET
;; (in contrast to the above which only sort of works)
;; FIXME: add hooks for additional modes - 
;; FIXME: For the into-pt stuff, be sensitive to the mode
;;        If I'm in markdown format, return markdown
;;        org, do org
;;        html, do html. Etc.
;;        C mode, put it in comments
;;        etc
;; FIXME: simplify navigation in org-mode buffer with minor mode
;;        add n and p to move to the links? CNTRL-arrows are good enough 
;;        
;; FIXME: Add robust interface
;; gnugol-thing-at-pt
;; gnugol-into-pt
;; gnugol-thing-at-pt-into-pt



(provide 'gnugol)

