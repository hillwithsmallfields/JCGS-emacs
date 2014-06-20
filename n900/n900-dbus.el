;;; Time-stamp: 

(defun dbus-test ()
  (list
   (dbus-call-method
    :system "org.freedesktop.Hal" "/org/freedesktop/Hal/devices/computer"
    "org.freedesktop.Hal.Device" "GetPropertyString"
    "system.kernel.machine")
   (dbus-call-method
    :system "org.freedesktop.Hal"
    ;; "com.nokia.profiled" 
    "/com/nokia/profiled"
    "com.nokia.profiled"
    "get_profile"
)))

(defun n900-read-diary ()
  "Return the n900's diary entries, as a list.
Each entry is a list the same as the arguments to n900-add-diary-entry."
)

(defun n900-add-diary-entry (start end interval warning text place)
)

(defun n900-map-contacts (fn)
  "Apply FN to each entry in the contacts database.
The result is a list of the results of FN."
)

(defun n900-add-contact ()
  ;; make it update entry if pre-existing
)

(defun n900-voice-call (destination)
  "Place a call to DESTINATION.
DESTINATION my be a phone number, contact name, or contact structure."
)

(defun n900-send-SMS (destination message)
  "To DESTINATION, send MESSAGE."
)

(defun n900-set-alarm (time text)
)

(defun n900-handle-usb-disconnect ()
  "Actions to do on a USB disconnect."
  (when (fboundp 'post-sync-catchup)
    (post-sync-catchup)))

(defun n900-init-handlers ()
)

;;; n900-dbus.el ends here
