;;; dbus-introspection.el --- Helper functions for D-Bus introspection
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: dbus, ipc
;; Version: 0.1
;; X-RCS: $Id:$
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
;; Helper functions for the `dbus-proxy' library.


;;; History:
;;
;; 0.3 - Consistent dbus-introspection- prefix
;;
;; 0.2 - Parsing of complex signatures
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'dbus)


;;; Error Conditions
;;

;; malformed-signature

(put 'malformed-signature 'error-conditions
     '(error malformed-signature))

(put 'malformed-signature 'error-message
     "Malformed signature signature")


;;;
;;

(defconst dbus-introspection-simple-type-codes
  '((?b . :boolean)
    (?y . :byte)
    (?n . :int16)
    (?q . :uint16)
    (?i . :int32)
    (?u . :uint32)
    (?x . :int64)
    (?t . :uint64)
    (?d . :double)
    (?s . :string)
    (?o . :object-path)
    (?g . :signature)
    (?v . :variant))
  "Mapping type indication characters to type keywords")

(defun dbus-introspection-parse-simple-type (string)
  "Parse (a substring of) STRING as a simple type designator.
The returned value is of the form (TYPE CONSUMED) where the
number of consumed characters is always one. If STRING is not
recognized as starting with a simple type designator, a
`malformed-signature' error condition is signaled.
A list of recognized simple type designators is available in the
variable `dbus-introspection-simple-type-codes."
  (let ((type (cdr (assoc (aref string 0)
			  dbus-introspection-simple-type-codes))))
    (if type
	(list type 1)
      (signal 'malformed-signature
	      (list "Unknown simple type designator" string)))))

(defun dbus-introspection-parse-composite-type (string)
  "Parse (a substring of) STRING as composite type designator.
A composite type is a simple type or one of struct, array or
dict-entry. The returned value is of the form (TYPE CONSUMED)
where the number of consumed characters is at least one but
potentially less than the length of STRING. TYPE is a list
starting with a type keyword:
\(SIMPLE\) for SIMPLE one of the cdrs in
`dbus-introspection-simple-type-codes'
\(:struct (SUBSTYPE1 SUBTYPE2 ...)\)
\(:array SUBTYPE\)
\(:dict-entry (KEYTYPE VALUETYPE)\)"
  (destructuring-bind (outer inner consumed)
      (case (aref string 0)
	;; Closing delimiters are consumed without generating
	;; anything.
	((?\) ?})
	 (list nil nil 1))

	;; Struct type
	((?r ?\()
	 (destructuring-bind (types consumed)
	     (dbus-introspection-parse-type-list (substring string 1))
	   (unless (= (aref string consumed) ?\)) ;; TODO check end of string
	     (signal 'malformed-signature
		     (list "struct misses closing parenthesis")))
	   `(:struct ,types ,consumed)))

	;; Array type
	(?a
	 `(:array ,@(dbus-introspection-parse-composite-type
		     (substring string 1))))

	;; Dict entry type.
	;;
	;; From the D-Bus spec: The restrictions are: it occurs only as
	;; an array element type; it has exactly two single complete
	;; types inside the curly braces; the first single complete
	;; type (the "key") must be a basic type rather than a
	;; container type.
	((?e  ?{)
	 (destructuring-bind (types consumed)
	     (dbus-introspection-parse-type-list (substring string 1))
	   (unless (= (length types) 2)
	     (signal 'malformed-signature
		     (list "dict entry has to contain exactly two types")))
	   (unless (= (aref string consumed) ?}) ;; TODO check end of string
	     (signal 'malformed-signature
		     (list "dict entry misses closing curly brace")))
	   `(:dict-entry ,types ,consumed)))

	(t
	 `(nil ,@(dbus-introspection-parse-simple-type string))))

    ;; Return the parsed type as a list of the form (TYPE CONSUMED).
    (cond
     (outer
      (list (list outer inner) (1+ consumed)))
     (inner
      (list inner consumed))
     (t
      (list nil consumed)))))

(defun dbus-introspection-parse-type-list (string)
  "Parse STRING as a list of type designators.
The returned value is of the form (TYPES CONSUMED). Where
consumed is equal to the length of STRING. TYPES is a list of
types
\(TYPE1 TYPE2 ...\)
where each element is of the form produced by
`dbus-introspection-parse-type-list'."
  (let ((remaining string)
	(all-consumed 0)
	(all-types))
    (catch 'early
      (while (> (length remaining) 0)
	(destructuring-bind (type consumed)
	    (dbus-introspection-parse-composite-type remaining)
	  (setq remaining (substring remaining consumed))
	  (incf all-consumed consumed)
	  (if type
	      (push type all-types)
	    (throw 'early nil)))))
    (list (reverse all-types) all-consumed)))


;;; Predicates and accessors for dbus types.
;;

;; Argument introspection elements

(defsubst dbus-introspection-arg-p (element)
  (eq (car-safe element) 'arg))

(defsubst dbus-introspection-arg-name (arg)
  (cdr (assoc 'name (second arg))))

(defsubst dbus-introspection-arg-type (arg)
  (cdr (assoc 'type (second arg))))

(defsubst dbus-introspection-arg-in-p (arg)
  (string= (cdr (assoc 'direction (second arg))) "in"))

;; Property introspection elements

(defsubst dbus-introspection-property-p (element)
  (eq (car-safe element) 'property))

(defsubst dbus-introspection-property-name (property)
  (cdr (assoc 'name (second property))))

(defsubst dbus-introspection-property-type (property)
  (cdr (assoc 'type (second property))))

(defsubst dbus-introspection-property-access (property)
  (cdr (assoc 'access (second property))))

;; Method introspection elements

(defsubst dbus-introspection-method-p (element)
  (eq (car-safe element) 'method))

(defsubst dbus-introspection-method-name (method)
  (cdr (assoc 'name (second method))))

;; Signal introspection elements

(defsubst dbus-introspection-signal-p (element)
  (eq (car-safe element) 'signal))

(defsubst dbus-introspection-signal-name (signal)
  (cdr (assoc 'name (second signal))))

;; Interface introspection elements

(defsubst dbus-introspection-interface-name (interface)
  (cdr (assoc 'name (second interface))))

(defsubst dbus-introspection-interface-elements (interface)
  (cddr interface))

(defsubst dbus-introspection-interface-properties (interface)
  (remove-if-not #'dbus-introspection-property-p(cddr interface)))

(defsubst dbus-introspection-interface-methods (interface)
  (remove-if-not #'dbus-introspection-method-p (cddr interface)))

(defsubst dbus-introspection-interface-signals (interface)
  (remove-if-not #'dbus-introspection-signal-p (cddr interface)))

(provide 'dbus-introspection)
;;; dbus-introspection.el ends here
