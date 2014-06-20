;;;; js-doctor.el -- put paragraphs through doctor.el to make facetious replies
;;; Time-stamp: <2004-12-04 13:16:49 jcgs>
;;; Old-time-stamp: <93/03/29 19:20:55 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; By John Sturdy <john@cb1.com>

(provide 'js-doctor)

(or (fboundp 'doctor-mode) (load-library "doctor"))

(defun js-doctor:doctor-paragraph ()
  "Doctor a paragraph.
Expects point to be within the paragraph."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (make-doctor-variables)
    ;; (setq js-doctor-paragraph-input-string (buffer-substring (region-beginning) (region-end)))
    (narrow-to-region (region-beginning) (region-end))
    (goto-char (point-min)) (forward-sentence 1)
    (setq js-doctor-readin-list (doctor-readin))
    (goto-char (point-max))
    (doctor-doc js-doctor-readin-list)
    (widen)
    ))


;;; end of js-doctor.el
