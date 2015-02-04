;;; vellum-theme.el --- theme a bit like an old manuscript

;; Copyright (C) 2013, 2015  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I created the original well before there was a theme system (or
;; perhaps just before I knew of one).

;;; Code:

(defun color-theme-vellum ()
  "Vellum manuscript-style color theme."
  (interactive)
  (color-theme-standard)
  (color-theme-install
   '(color-theme-vellum
     ((background-color . "wheat")
      (foreground-color . "brown")))))


(deftheme vellum
  "Vellum theme."

(let ((class '((class color) (min-colors 89)))
      ;; Vellum palette colors.
      ;; (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      ;; (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      ;; (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      ;; (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      ;; (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      ;; (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      ;; (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      ;; (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      ;; (alum-4 "#888a85") (alum-5 "#5f615c") (alum-6 "#2e3436")
      ;; (cham-4 "#346604") (blue-0 "#8cc4ff") (orange-4 "#b35000")
      )

  (custom-theme-set-faces
   'vellum
   `(default ((,class (:foreground "wheat" :background "brown"))))
   ;; `(cursor ((,class (:background ,blue-3))))

   ;; ;; Highlighting faces
   ;; `(fringe ((,class (:background ,alum-2))))
   ;; `(highlight ((,class (:background ,alum-3))))
   ;; `(region ((,class (:background ,alum-3))))
   ;; `(secondary-selection ((,class (:background ,blue-0))))
   ;; `(isearch ((,class (:foreground "#ffffff" :background ,orange-3))))
   ;; `(lazy-highlight ((,class (:background ,choc-1))))
   `(trailing-whitespace ((,class (:background "red"))))

   ;; Mode line faces
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
			      :background "purple" :foreground "dark red"))))
   ;; `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
   ;; 				  :background ,alum-4 :foreground ,alum-6))))

   ;; Escape and prompt faces
   ;; `(minibuffer-prompt ((,class (:weight bold :foreground ,blue-3))))
   ;; `(escape-glyph ((,class (:foreground ,red-3))))
   ;; `(error ((,class (:foreground ,red-3))))
   ;; `(warning ((,class (:foreground ,orange-3))))
   ;; `(success ((,class (:foreground ,cham-3))))

   ;; Font lock faces
   ;; `(font-lock-builtin-face ((,class (:foreground ,plum-2))))
   `(font-lock-comment-face ((,class (:slant italic :foreground "purple"))))
   ;; `(font-lock-constant-face ((,class (:weight bold :foreground ,blue-3))))
   `(font-lock-function-name-face ((,class (:foreground "red")
					   (:background "gold")
					   (:height 1.25)
					   (:bold t)
					   (:box (:color "dark green"
							 :width 2)))))
   ;; `(font-lock-keyword-face ((,class (:foreground ,cham-4))))
   ;; `(font-lock-string-face ((,class (:foreground ,plum-3))))
   ;; `(font-lock-type-face ((,class (:foreground ,blue-3))))
   ;; `(font-lock-variable-name-face ((,class (:foreground ,orange-4))))

   ;; Button and link faces
   ;; `(link ((,class (:underline t :foreground ,blue-3))))
   ;; `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Gnus faces
   ;; `(gnus-group-news-1 ((,class (:weight bold :foreground ,plum-3))))
   ;; `(gnus-group-news-1-low ((,class (:foreground ,plum-3))))
   ;; `(gnus-group-news-2 ((,class (:weight bold :foreground ,blue-3))))
   ;; `(gnus-group-news-2-low ((,class (:foreground ,blue-3))))
   ;; `(gnus-group-news-3 ((,class (:weight bold :foreground ,red-3))))
   ;; `(gnus-group-news-3-low ((,class (:foreground ,red-3))))
   ;; `(gnus-group-news-4 ((,class (:weight bold :foreground ,"#7a4c02"))))
   ;; `(gnus-group-news-4-low ((,class (:foreground ,"#7a4c02"))))
   ;; `(gnus-group-news-5 ((,class (:weight bold :foreground ,orange-3))))
   ;; `(gnus-group-news-5-low ((,class (:foreground ,orange-3))))
   ;; `(gnus-group-news-low ((,class (:foreground ,alum-4))))
   ;; `(gnus-group-mail-1 ((,class (:weight bold :foreground ,plum-3))))
   ;; `(gnus-group-mail-1-low ((,class (:foreground ,plum-3))))
   ;; `(gnus-group-mail-2 ((,class (:weight bold :foreground ,blue-3))))
   ;; `(gnus-group-mail-2-low ((,class (:foreground ,blue-3))))
   ;; `(gnus-group-mail-3 ((,class (:weight bold :foreground ,cham-3))))
   ;; `(gnus-group-mail-3-low ((,class (:foreground ,cham-3))))
   ;; `(gnus-group-mail-low ((,class (:foreground ,alum-4))))
   ;; `(gnus-header-content ((,class (:foreground ,cham-3))))
   ;; `(gnus-header-from ((,class (:weight bold :foreground ,butter-3))))
   ;; `(gnus-header-subject ((,class (:foreground ,red-3))))
   ;; `(gnus-header-name ((,class (:foreground ,blue-3))))
   ;; `(gnus-header-newsgroups ((,class (:foreground ,alum-4))))

   ;; Message faces
   ;; `(message-header-name ((,class (:foreground ,blue-3))))
   ;; `(message-header-cc ((,class (:foreground ,butter-3))))
   ;; `(message-header-other ((,class (:foreground ,choc-2))))
   ;; `(message-header-subject ((,class (:foreground ,red-3))))
   ;; `(message-header-to ((,class (:weight bold :foreground ,butter-3))))
   ;; `(message-cited-text ((,class (:slant italic :foreground ,alum-5))))
   ;; `(message-separator ((,class (:weight bold :foreground ,cham-3))))
   ;; ;; SMerge
   ;; `(smerge-refined-change ((,class (:background ,plum-1))))

   ;; Ediff
   ;; `(ediff-current-diff-A ((,class (:background ,blue-1))))
   ;; `(ediff-fine-diff-A ((,class (:background ,plum-1))))
   ;; `(ediff-current-diff-B ((,class (:background ,butter-1))))
   ;; `(ediff-fine-diff-B ((,class (:background ,orange-1))))

   ;; Flyspell
   ;; `(flyspell-duplicate ((,class (:underline ,orange-1))))
   ;; `(flyspell-incorrect ((,class (:underline ,red-1))))

   ;; Semantic faces
   ;; `(semantic-decoration-on-includes ((,class (:underline  ,cham-4))))
   ;; `(semantic-decoration-on-private-members-face
   ;;   ((,class (:background ,alum-2))))
   ;; `(semantic-decoration-on-protected-members-face
   ;;   ((,class (:background ,alum-2))))
   ;; `(semantic-decoration-on-unknown-includes
   ;;   ((,class (:background ,choc-3))))
   ;; `(semantic-decoration-on-unparsed-includes
   ;;   ((,class (:underline  ,orange-3))))
   ;; `(semantic-tag-boundary-face ((,class (:overline   ,blue-1))))
   ;; `(semantic-unmatched-syntax-face ((,class (:underline  ,red-1))))
   )

  (custom-theme-set-variables
   'vellum
   `(ansi-color-names-vector [])))

(provide-theme 'vellum)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; vellum-theme.el ends here
