;;; set-ogg-comments.el --- set OGG file comments

;; Copyright (C) 2009  John C G Sturdy

;; Author: John C G Sturdy <john.sturdy@ul.ie>
;; Keywords: multimedia, convenience

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

;; 

;;; Code:

(defvar ogg-latest-album nil)
(defvar ogg-latest-genre nil)
(defvar ogg-latest-artist nil)

(defun set-ogg-comments (file album title genre artist)
  "Set the file comments to FILE ALBUM TITLE GENRE ARTIST."
  (interactive
   (list (read-file-name "Set OGG comments for file: ")
	 (setq ogg-latest-album (read-from-minibuffer "Album: " nil nil nil nil ogg-latest-album))
	 (read-from-minibuffer "Title: ")
	 (setq ogg-latest-genre (read-from-minibuffer "Genre: " nil nil nil nil ogg-latest-genre))
	 (setq ogg-latest-artist (read-from-minibuffer "Artist: " nil nil nil nil ogg-latest-artist))))
  (let ((commentfile (make-temp-file "/tmp/oggcomments")))
    (find-file commentfile)
    (erase-buffer)
    (insert "ALBUM=" album "
TITLE=" title "
GENRE=" genre "
ARTIST=" artist "\n")
    (basic-save-buffer)
    (kill-buffer)
    (shell-command (format "vorbiscomment -w -c %s %s"
			   commentfile
			   file))
    (delete-file commentfile)))

(provide 'set-ogg-comments)
;;; set-ogg-comments.el ends here
