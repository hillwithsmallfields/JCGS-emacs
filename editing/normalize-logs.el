;;; normalize-logs.el --- edit log files to change commonly uninteresting variable text to constant text

;; Copyright (C) 2012, 2013  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience

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

(require 'replace-regexp-list)

(defvar normalize-log-edits
  '(("^[0-9.]+s " . "")
    (" in \\([0-9]+\\)\\.[0-9]+ seconds" . "in \\1 seconds")
    (" in \\([0-9]+\\)\\.[0-9]+s" . "in \\1s")
    ("used \\([0-9]+\\)\\.[-0-9.e]+ " . "used \\1 ")
    ("^[JFMASOND][aepuoe][nbrylgptvc] [0-9]+ [0-9]+:[0-9]+:[0-9.]+ " . "")
    ("[JFMASOND][aepuoe][nbrylgptvc] [0-9]+ [0-9]+:[0-9]+:[0-9.]+ " . "<timestamp> ")
    ("tapdisk\\[[0-9]+\\]" . "tapdisk[]")
    ("audit([0-9.:]+)" . "audit()")
    ("kernel:\\[[0-9.]+\\]" . "kernel:[]")
    ("pipe:\\[[0-9]+\\]" . "pipe:[]")
    ("pid=[0-9]+" . "pid=*")
    ("ino=[0-9]+" . "ino=*")
    ("user [-0-9a-f]+" . "user fred")
    ("password [0-9a-f]+" . "password secret")
    ("uuid [-0-9a-f]+" . "uuid <uuid>")
    ("domid [0-9]+" . "domid 42")
    ("\\[[0-9][0-9]/[a-z][a-z][a-z]/[0-9][0-9][0-9][0-9]:[0-9][0-9]:[0-9][0-9]:[0-9][0-9] [-+][0-9]+\\]" . "[timestamp]")
    ("u'[-0-9a-f]+'" . "u'uuid'")
    ("ID [0-9a-f]+" . "ID <id>")
    ("\\(apache access.log [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) [-0-9a-f]+" . "\\1 <uuid>")
    ("[-/0-9a-f]+\\.tar" . "<path>.tar")
    ("repo/[-0-9a-f]+" . "repo/f00")
    ("vm/[-_0-9a-f]+" . "vm/f00")
    ("confirmed [-0-9a-f]+ exists" . "confirmed f00 exists")
    ("[-0-9a-f]+\\.vhd" . "f00.vhd")
    ("xp([-0-9a-f]+)" . "xp()")
    ("at 0x[-0-9a-f]+" . "at <address>")
    ("'uuid': '[-0-9a-f]+'" . "'uuid': '01230123-0123-0123-0123-012301230123'")
    ("10.80.251.[0-9]+" . "10.80.251.256")
    ("[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]" . "f0:f0:f0:f0:f0:f0")
    (" used [0-9]+ timeout " . " used 1 timeout ")
    (" age \\([0-9]+\\)\\.[0-9]+ days" . " age \1 days")
    (" free space=[0-9]+ bytes, \\([0-9]+\\)\\.[0-9]+GB" . " free space=\\1000000000 bytes, \\1GB")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    ;; ("[-0-9a-f]+" . "")
    )
  "Edits for normalizing a log file.")

(defun normalize-log-text ()
  "Normalize log text in the current buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (string-match "-orig\\'" filename)
      (error "Don't edit the original!"))
    (let ((orig-name (concat buffer-file-name "-orig")))
      (unless (file-exists-p orig-name)
	(copy-file filename orig-name))))
  (replace-regexp-alist normalize-log-edits
			nil
			nil
			nil
			nil
			t))

(provide 'normalize-logs)
;;; normalize-logs.el ends here
