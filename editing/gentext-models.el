;;;; gentext-models.el -- model functions for generic text
;;; Time-stamp: <2006-02-17 18:49:32 jcgs>

(provide 'gentext-models)

(defmodel generic-text-title (&optional surround)
  "Insert a title.
With non-nil (prefix) argument, surround the region with a title."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

;; (defmodel new-paragraph (&optional surround)
;;   "Insert a paragraph.
;; With non-nil (prefix) argument, surround the region with a paragraph."
;;   (interactive "P")
;;   (run-hook-with-args-until-success 'generic-text-before-hooks)
;;   (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-end-whatever ()
  "Capitalize the first word in the sentence, but do not add sentence-ending punctuation."
  (interactive)
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-end-sentence ()
  "Insert a period here, and capitalize the first word in the sentence."
  (interactive)
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-end-question ()
  "Insert a question mark here, and capitalize the first word in the sentence."
  (interactive)
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-end-exclamation  ()
  "Insert an exclamation mark here, and capitalize the first word in the sentence."
  (interactive)
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-paragraph (&optional surround)
  "Insert a paragraph.
With non-nil (prefix) argument, surround the region with a paragraph."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-insert-item (&optional surround)
  "Insert a insert-item.
With non-nil (prefix) argument, surround the region with a insert-item."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-ordered-list (&optional surround)
  "Insert a ordered-list.
With non-nil (prefix) argument, surround the region with a ordered-list."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-unordered-list (&optional surround)
  "Insert a unordered-list.
With non-nil (prefix) argument, surround the region with a unordered-list."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-definition-list (&optional surround)
  "Insert a definition-list.
With non-nil (prefix) argument, surround the region with a definition-list."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-table (&optional surround)
  "Insert a table.
With non-nil (prefix) argument, surround the region with a table."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-table-row (&optional surround)
  "Insert a table-row.
With non-nil (prefix) argument, surround the region with a table-row."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-table-data (&optional surround)
  "Insert a table-data.
With non-nil (prefix) argument, surround the region with a table-data."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-table-heading (&optional surround)
  "Insert a table-heading.
With non-nil (prefix) argument, surround the region with a table-heading."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-hyperlink (&optional surround)
  "Insert a hyperlink.
With non-nil (prefix) argument, surround the region with a hyperlink."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-header-1 (&optional surround)
  "Insert a header-1.
With non-nil (prefix) argument, surround the region with a header-1."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-header-2 (&optional surround)
  "Insert a header-2.
With non-nil (prefix) argument, surround the region with a header-2."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-header-3 (&optional surround)
  "Insert a header-3.
With non-nil (prefix) argument, surround the region with a header-3."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-header-4 (&optional surround)
  "Insert a header-4.
With non-nil (prefix) argument, surround the region with a header-4."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-horizontal-line (&optional surround)
  "Insert a horizontal-line.
With non-nil (prefix) argument, surround the region with a horizontal-line."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-image (&optional surround)
  "Insert a image.
With non-nil (prefix) argument, surround the region with a image."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-link-target (&optional surround)
  "Insert a link-target.
With non-nil (prefix) argument, surround the region with a link-target."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-italic (&optional surround)
  "Insert an \"italic\".
With non-nil (prefix) argument, surround the region with an \"italic\" style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-bold (&optional surround)
  "Insert a \"bold\".
With non-nil (prefix) argument, surround the region with a \"bold\" style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-strong (&optional surround)
  "Insert a \"strong\".
With non-nil (prefix) argument, surround the region with a \"strong\" style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-keyboard (&optional surround)
  "Insert a \"keyboard\" style.
With non-nil (prefix) argument, surround the region with a \"keyboard\" style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-variable (&optional surround)
  "Insert a \"variable\" style.
With non-nil (prefix) argument, surround the region with a \"variable\" style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-emphasized (&optional surround)
  "Insert a emphasized.
With non-nil (prefix) argument, surround the region with a emphasized."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-definition (&optional surround)
  "Insert a definition.
With non-nil (prefix) argument, surround the region with a definition."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-code (&optional surround)
  "Insert a code.
With non-nil (prefix) argument, surround the region with a code."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-preformatted (&optional surround)
  "Insert a preformatted.
With non-nil (prefix) argument, surround the region with a preformatted."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-blockquote (&optional surround)
  "Insert a blockquote.
With non-nil (prefix) argument, surround the region with a blockquote."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel generic-text-citation ()
  "Insert a citation."
  (interactive))

(defmodel sample-style (&optional surround)
   "Insert a sample style style.
With non-nil (prefix) argument, surround the region with a sample style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel variable-style (&optional surround)
   "Insert a variable style style.
With non-nil (prefix) argument, surround the region with a variable style."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel braces (&optional surround)
   "Insert braces.
With non-nil (prefix) argument, surround the region with braces."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

(defmodel environment (&optional surround)
   "Insert an environment.
With non-nil (prefix) argument, surround the region with an environment."
  (interactive "P")
  (run-hook-with-args-until-success 'generic-text-before-hooks)
  (run-hook-with-args-until-success 'generic-text-after-hooks))

;;; end of gentext-models.el
