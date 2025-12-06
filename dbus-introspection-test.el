;;; dbus-introspection-test.el --- Tests for dbus-introspection.el
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: DBus, ipc, test
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
;; This file contains unit tests for the code in
;; dbus-introspection.el.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;


(require 'ert)


;;; Unit Tests:
;;

(ert-deftest dbus-introspection-test-parse-simple-type-smoke ()
  "Smoke test for the `dbus-introspection-parse-simple-type' function."
  (dolist (case '(("u"  (:uint32 1))
		  ("ab" error)
		  ("!"  error)))
    (destructuring-bind (input expected) case
      (if (eq expected 'error)
	  (should-error
	   (dbus-introspection-parse-simple-type input)
	   :type 'error)
	(should (equal
		 (dbus-introspection-parse-simple-type input)
		 expected)))))
  )

(ert-deftest dbus-introspection-test-parse-composite-type-smoke ()
  "Smoke test for the `dbus-introspection-parse-composite-type' function."
  (dolist (case '(;; Simple
		  ("i"       (:int32 1))
		  ("ii"      (:int32 1))
		  ("u"       (:uint32 1))
		  ("uu"      (:uint32 1))

		  ;; Struct
		  ("(u)"     ((:struct (:uint32)) 3))
		  ("(v)"     ((:struct (:variant)) 3))
		  ("(ii)"    ((:struct (:int32 :int32)) 4))
		  ("(i(ii))" ((:struct (:int32 (:struct (:int32 :int32)))) 7))
		  ("(ius)"   ((:struct (:int32 :uint32 :string)) 5))
		  ("(ii"     error)
		  ("ii)"     (:int32 1))

		  ;; Array
		  ("au"      ((:array :uint32) 2))
		  ("ai"      ((:array :int32) 2))
		  ("av"      ((:array :variant) 2))
		  ("a(ii)"   ((:array (:struct (:int32 :int32))) 5))
		  ("aai"     ((:array (:array :int32)) 3))
		  ("aa"      error)

		  ;; Dict entry
		  ("a{su}"   ((:array (:dict-entry (:string :uint32))) 5))
		  ("a{su}s"  ((:array (:dict-entry (:string :uint32))) 5))
		  ("a{suu}"  error)
		  ("a{su"    error)
		  ("a{s"     error)

		  ;; Random stuff
		  ("!"       error)))
    (destructuring-bind (input expected) case
      (if (eq expected 'error)
	  (should-error
	   (dbus-introspection-parse-composite-type input)
	   :type 'error)
	(should (equal
		 (dbus-introspection-parse-composite-type input)
		 expected)))))
  )

(ert-deftest dbus-introspection-test-parse-type-list-smoke ()
  "Smoke test for the `dbus-introspection-parse-type-list' function."
  (dolist (case '(;; Simple
		  ("i"       ((:int32) 1))
		  ("ii"      ((:int32 :int32) 2))

		  ("u"       ((:uint32) 1))
		  ("uu"      ((:uint32 :uint32) 2))

		  ;; Struct
		  ("(ii)"    (((:struct (:int32 :int32))) 4))
		  ("(i(ii))" (((:struct (:int32 (:struct (:int32 :int32))))) 7))
					;("(ii"     error)
					;("ii)"     error)

		  ("(u)"     (((:struct (:uint32))) 3))
		  ("(v)"     (((:struct (:variant))) 3))
		  ("(ius)"   (((:struct (:int32 :uint32 :string))) 5))

		  ;; Array
		  ("ai"      (((:array :int32)) 2))
		  ("a(ii)"   (((:array (:struct (:int32 :int32)))) 5))
		  ("aai"     (((:array (:array :int32))) 3))
		  ("aa"      error)

		  ("au"      (((:array :uint32)) 2))
		  ("av"      (((:array :variant)) 2))

		  ;; Dict entry
		  ("a{su}"   (((:array (:dict-entry (:string :uint32)))) 5))
		  ("a{su}s"  (((:array (:dict-entry (:string :uint32))) :string) 6))
		  ("a{suu}"  error)
		  ("a{su"    error)
		  ("a{s"     error)

		  ;; Random stuff
		  ("!"       error)))
    (destructuring-bind (input expected) case
      (if (eq expected 'error)
	  (should-error
	   (dbus-introspection-parse-composite-type input)
	   :type 'error)
	(should (equal
		 (dbus-introspection-parse-type-list input)
		 expected)))))
  )

(provide 'dbus-introspection-test)
;;; dbus-introspection-test.el ends here
