;;; dbus-proxy-tests.el --- Unit tests for dbus-proxy.el
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: D-Bus, ipc, net
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains unit tests for the code in dbus-proxy.el.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'ert)


;;; Unit Tests
;;

(ert-deftest dbus-proxy-test-make-call-arg-smoke ()
  "Smoke test for `make-call-arg'."
  (should
   (equal (dbus-proxy-make-call-args
	   '((arg ((type . "d")))
	     (arg ((type . "s")))
	     (arg ((type . "s"))))
	   '(arg1 arg2 arg3))
	  '(:double arg1 :string arg2 :string arg3)))
  )

(ert-deftest dbus-proxy-test-transform-camel-case-smoke ()
  "Smoke test for `dbus-proxy-transform-camel-case'."
  (dolist (case '(("CamelCase"        "camel-case")
		  ("UPPERCASE"        "uppercase")
		  ("PARTIALUppercase" "partial-uppercase")
		  ("CrazyCamelCase"   "crazy-camel-case")
		  ("loadURIList"      "load-uri-list")))
    (destructuring-bind (input expected) case
      (should (string= (dbus-proxy-transform-camel-case input)
		       expected))))
  )

(ert-deftest dbus-proxy-test-make-remote-proxy ()
  "Test `dbus-proxy-make-remote-proxy'."

  ;; Invalid object path - it seems we cannot check this
  ;; (should-error (dbus-proxy-make-remote-proxy
  ;;		     :session
  ;;		     "org.freedesktop.DBus"
  ;;		     "/org/freedesktop/DBus-invalid"))
  ;; Invalid service - it seems we cannot check this
  ;; (should-error (dbus-proxy-make-remote-proxy
  ;;		  :session
  ;;		  "org.freedesktop.DBus-invalid"
  ;;		  "/org/freedesktop/DBus"))
  ;; Invalid bus name - it seems we cannot check this
  ;; (should-error (dbus-proxy-make-remote-proxy
  ;;		     :session-invalid
  ;;		     "org.freedesktop.DBus"
  ;;		     "/org/freedesktop/DBus"))

  ;; Existing object on session bus.
  (let ((session-bus (dbus-proxy-make-remote-proxy
		      :session
		      "org.freedesktop.DBus"
		      "/org/freedesktop/DBus")))
    (with-slots (bus service object) session-bus
      (should (eq      bus     :session))
      (should (string= service "org.freedesktop.DBus"))
      (should (string= object  "/org/freedesktop/DBus"))))

  ;; Existing object on system bus.
  (let ((session-bus (dbus-proxy-make-remote-proxy
		      :system
		      "org.freedesktop.DBus"
		      "/org/freedesktop/DBus")))
    (with-slots (bus service object) session-bus
      (should (eq      bus     :system))
      (should (string= service "org.freedesktop.DBus"))
      (should (string= object  "/org/freedesktop/DBus"))))
  )

(ert-deftest dbus-proxy-test-connect ()
  "Test the `dbus-proxy-connect' mechanism."
  (let ((hal (dbus-proxy-make-remote-proxy
	      :system
	      "org.freedesktop.Hal"
	      "/org/freedesktop/Hal/Manager")))

    ;; Connecting to non-existing signal should signal an error.
    (should-error
     (dbus-proxy-connect hal 'does-not-exist #'ignore)
     :type 'no-such-signal)

    ;; Do a successful connection and then disconnect again.
    (let ((connection (dbus-proxy-connect
		       hal 'device-added #'ignore)))
      (should connection)
      (dbus-proxy-disconnect hal connection))

    ;; Connect by manually specifying the precise D-Bus interface
    ;; and signal names and then disconnect again.
    (let ((connection (dbus-proxy-connect
		       hal (list "org.freedesktop.Hal.Manager"
				 "DeviceAdded")
		       #'ignore)))
      (should connection)
      (dbus-proxy-disconnect hal connection))

    ;; TODO test this properly; the problem is that we need a D-Bus
    ;; service that emits signals regularly and automatically
    )
  )

(ert-deftest dbus-proxy-test-dbus ()
  "Some tests with the D-Bus object."
  (let ((dbus (dbus-proxy-make-remote-proxy
	       :session
	       "org.freedesktop.DBus"
	       "/org/freedesktop/DBus")))
    ;; Call Introspect method of the bus object.
    (should (stringp (introspect dbus)))

    ;; Call the Hello method. This fails since the bus does not
    ;; want us to call the method multiple times.
    (should-error
     (hello dbus)
     :type 'dbus-error)
    )
  )

(ert-deftest dbus-proxy-test-mission-control ()
  "Some tests with the Mission Control D-Bus object."
  (let ((mission-control (dbus-proxy-make-remote-proxy
			  :session
			  "org.freedesktop.Telepathy.MissionControl5"
			  "/org/freedesktop/Telepathy/AccountManager")))

    ;; Call the find-accounts method of the mission control
    ;; object.
    (should (listp
	     (find-accounts mission-control
			    '(:array :signature "{sv}")))))
  )

(ert-deftest dbus-proxy-test-epiphany ()
  "Some tests with the Epiphany D-Bus object."
  (let ((epiphany (dbus-proxy-make-remote-proxy
		   :session
		   "org.gnome.Epiphany"
		   "/org/gnome/Epiphany")))

    ;; Open the bookmarks editor
    (open-bookmarks-editor epiphany 0)

    ;; Load a URL
    (load-uri-list epiphany '("www.heise.de") "" 1))
  )

(ert-deftest dbus-proxy-test-devicekit ()
  "Some tests with the Devicekit D-Bus object."
  (let ((device-kit (dbus-proxy-make-remote-proxy
		     :system
		     "org.freedesktop.DeviceKit"
		     "/org/freedesktop/DeviceKit")))

    ;; Retrieve the daemon-version property.
    (should (stringp (slot-value device-kit :daemon-version)))
    (should (stringp (oref device-kit :daemon-version)))

    ;; Connect to the device-event signal.
    (dbus-proxy-connect device-kit 'device-event
			(lambda (&rest args)
			  (message "Device event %s" args)))

    ;; Enumerate subsystems.
    (should (listp (enumerate-by-subsystem device-kit '("sound")))))
  )

(ert-deftest dbus-proxy-test-rhythmbox ()
  "Some tests with the Rhythmbox D-Bus object."
  (let ((rhythmbox (dbus-proxy-make-remote-proxy
		    :session
		    "org.gnome.Rhythmbox"
		    "/org/gnome/Rhythmbox/Player")))

    ;; Connect to the playing-uri-changed signal
    (dbus-proxy-connect
     rhythmbox 'playing-uri-changed
     (lambda (&rest args)
       (message "Playing URI changed event %s" args)))

    ;; Why can't we have `compose' :(
    (dolist (v (mapcar
		(apply-partially #'* 0.3)
		(mapcar
		 #'abs
		 (mapcar
		  #'sin
		  (mapcar
		   (apply-partially #'* 0.1)
		   (number-sequence 1 100))))))
      (set-volume rhythmbox v)
      (sit-for 0.02)))
  )

(ert-deftest dbus-proxy-test-gdm ()
  "Some tests with the GDM D-Bus object."
  (let ((gdm (dbus-proxy-make-remote-proxy
	      :system
	      "org.gnome.DisplayManager"
	      "/org/gnome/DisplayManager/Manager" t)))

    ;; List displays. This is interesting because the return value
    ;; of the call is a list of object paths.
    (get-displays gdm))
  )

(provide 'dbus-proxy-tests)
;;; dbus-proxy-tests.el ends here
