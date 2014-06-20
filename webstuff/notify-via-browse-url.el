;;; notify-via-browse-url.el --- Display a message to the user via browse-url

;; Copyright (C) 2011, 2013  John Sturdy

;; Author: John Sturdy <john.sturdy@citrix.com>
;; Keywords: convenience, processes, hypermedia

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

;; When waiting for a long compile etc, I can easily get distracted
;; into looking at stuff on the web, then fail to notice when the
;; compilation has finished.  So I wrote this to interrupt my browsing
;; by putting the message onto my browser.

;;; Code:

(defvar notify-via-browse-url-directory
  (expand-file-name (format "notify-%s-pages" user-login-name) "/tmp")
  "The directory into which to put notification pages.")

(defvar notify-via-browse-url-keep-directory-clean t
  "Whether to keep cleaning old files out of the notification page directory.")

(defvar notify-via-browse-url-preamble-1
  "<html>\n<head>\n<title>"
  "Text to precede the notification title.")

(defvar notify-via-browse-url-preamble-2
  "</title>\n"
  "Text to follow the notification title.")

(defvar notify-via-browse-url-preamble-3
  "</head>\n<body>\n<h1>"
  "Text to follow the notification title.")

(defvar notify-via-browse-url-preamble-4
  "</h1>\n"
  "Text to precede the notification.")

(defvar notify-via-browse-url-postamble
  "\n</body></html>\n"
  "Text to follow the notification.")

(defun browse-url-in-side-frame (url)
  "Call `browse-url' on URL in the frame in which I keep my side oddments."
  (let* ((old-frame (selected-frame))
	 (side-window (get-buffer-window "#xc" t))
	 (side-frame (window-frame side-window)))
    (select-frame side-frame)
    (browse-url url)
    (select-frame old-frame)))

(defun notify-via-browse-url-new-page-name ()
  "Make a page name for notifying the user."
  (unless (file-directory-p notify-via-browse-url-directory)
    (make-directory notify-via-browse-url-directory))
  (when notify-via-browse-url-keep-directory-clean
    (mapcar 'delete-file (directory-files notify-via-browse-url-directory
					  t
					  "[0-9]+\\.html"
					  t)))
  (expand-file-name (format-time-string "%Y%m%d%H%M%S.html")
		    notify-via-browse-url-directory))

(defun notify-via-browse-url (style title &rest text)
  "Using STYLE, display TITLE and TEXT on the user's web browser.

TITLE may be a string to use as the title and heading,
or a cons of two strings to use at the title and heading,
or a cons of a string and nil to use as a title and no heading,
or nil, in which case the first of TEXT is used (and must be a string).

TEXT are any remaining arguments.  Each of the TEXT arguments may be:

  A string, which is inserted literally

  A buffer object, the contents of which are inserted literally

  A list of a string, a buffer object, and a string, in which case
    those are inserted in that order, allowing easy wrapping of
    buffer text in preformatting markers."
  (let ((file (notify-via-browse-url-new-page-name)))
    (find-file file)
    (erase-buffer)
    (insert notify-via-browse-url-preamble-1
	    (cond
	     ((stringp title) title)
	     ((consp title) (car title))
	     ((null title) (car text)))
	    notify-via-browse-url-preamble-2
	    (or style "")
	    notify-via-browse-url-preamble-3
	    (cond
	     ((stringp title) title)
	     ((consp title) (or (cdr title) ""))
	     ((null title) (car text)))
	    notify-via-browse-url-preamble-4)
    (mapcar (function
	     (lambda (text-or-buffer)
	       (cond
		((stringp text-or-buffer)
		 (insert text-or-buffer))
		((bufferp text-or-buffer)
		 (insert-buffer-substring text-or-buffer))
		((and (consp text-or-buffer)
		      (bufferp (cadr text-or-buffer)))
		 (insert (car text-or-buffer))
		 (insert-buffer-substring (cadr text-or-buffer))
		 (insert (caddr text-or-buffer))))))
	    text)
    (insert notify-via-browse-url-postamble)
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    (browse-url-in-side-frame (format "file://%s" file))))

;;;; look at a web page for a set time

(defun web-page-for-time-finish (notification)
  "Put up NOTIFICATION.
For use from web-page-for-time."
  (notify-via-browse-url notification))

(defun web-page-for-time (url time notification)
  "Make the user's browser visit URL, then at/after TIME put up a NOTIFICATION text.
TIME is as described for `run-at-time'."
  (browse-url-in-side-frame url)
  (run-at-time time nil 'web-page-for-time-finish notification))

;;;; wait until a host is up

(defun notify-host-up (host)
  "Notify the user when HOST comes up."
  (interactive "sNotify host boot: ")
  (let* ((name (concat "*ping " host "*"))
	 (buf (get-buffer-create name))
	 (args (list "-c" "1" "-W" "1")))
    (set-buffer buf)
    (erase-buffer)
    (set-process-sentinel
     (apply 'start-process name buf ping-program args)
     'notify-host-check-ping-output)))

(defvar notify-host-up-check-interval "30 sec"
  "Interval for checking for a host to come up.")

 (defun notify-host-check-ping-output (process change)
  "Sentinel function for use from `notify-host-up'."
  (set-buffer (process-buffer process))
  (goto-char (point-min))
  (let ((host (car (last (process-command process)))))
    (if (search-forward ", 0 received" (point-max) t)
      (run-at-time notify-host-up-check-interval
		   nil
		   'notify-host-up
		   host)
    (notify-via-browse-url "Host up" "The host \"" host "\" is now responding."))))

(provide 'notify-via-browse-url)

;;; notify-via-browse-url.el ends here
