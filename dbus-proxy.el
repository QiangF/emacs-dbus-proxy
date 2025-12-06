;;; dbus-proxy.el --- Automatic proxies for remote D-Bus objects
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: DBus, proxy
;; Version: 0.3
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
;; Here is a basic example of the intended use:
;;
;; (let ((epiphany (dbus-proxy-make-remote-proxy
;;		    :session
;;		    "org.gnome.Epiphany"
;;		    "/org/gnome/Epiphany")))
;;   (open-bookmarks-editor epiphany 0))
;;
;; When called for the first time on a particular D-Bus object,
;; `dbus-proxy-make-remote-proxy' looks up the set of interfaces
;; implemented by the object and dynamically creates Emacs Lisp code
;; that mimics the D-Bus interfaces. This code mainly consists of
;; functions which correspond to the D-Bus interface methods, but
;; information about D-Bus properties and signals is also stored.
;; When called, the generated functions augment their arguments with
;; D-Bus type information and call their corresponding D-Bus methods.
;; When these proxy components are created, names are transformed from
;; D-Bus-typical CamelCamel to something-more-lispy. The generated
;; functions and object slots can be used like ordinary Emacs Lisp
;; functions and object slots. Generated proxy classes can be
;; inspected using `cl--described-class'.
;;
;; To illustrate the details of the code generation process, let us
;; consider the following introspection data of the
;; /org/gnome/Epiphany object, that has been used in the above
;; example:
;;
;; <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
;; "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
;; <node>
;;   [...]
;;   <interface name="org.gnome.Epiphany">
;;     <method name="openBookmarksEditor">
;;       <arg name="Timestamp" type="u" direction="in"/>
;;     </method>
;;     [...]
;;   </interface>
;; </node>
;;
;; The method description is translated into the following Emacs Lisp
;; method:
;;
;; (cl-defmethod (this org\.gnome\.Epiphany) Timestamp)
;;   (with-slots (bus service object) this
;;     (dbus-call-method
;;      bus service object "org.gnome.Epiphany" "openBookmarksEditor"
;;      :uint32 Timestamp)))
;;
;; Similar code is generated for the remaining methods, properties and
;; signals of the org.gnome.Epiphany interface and all other
;; interfaces of the D-Bus object.
;;
;; Creating a proxy object works as follows:
;;
;; Input: bus, client, object path
;;
;; 1. Collect all interfaces of the object at the specified path
;; 2. Form class name (1) (which is a symbol) using the sorted
;;    interface names
;; 3. Check whether the symbol has a class value
;; 4.a If there is one, continue with 5.
;; 4.b Create a new class for the object
;;   1. For each interface:
;;     1. Form class name (2) (which is a symbol) using the interface
;;        name
;;     2. Check whether a class exists for the interface
;;     3.a If there is one, continue with next interface
;;     3.b Create a new class
;;       1. Define the new class using the symbol (2)
;;          Parents: `dbus-proxy-interface-object'
;;       2. For each method specified by the interface
;;          1. Transform the method name into a lispy name
;;          2. Create a method definition for the name that calls the
;;             D-Bus method
;;       3. For each property specified by the interface
;;          1. Transform the property name into a lispy name
;;          2. Store a mapping from transformed name to interface name
;;             and property name
;;       4. For each signal specified by the interface
;;          1. Transform the signal name into a lispy name
;;          2. Store a mapping from transformed name to interface name
;;             and signal name
;;   2. Define a (3) class using the symbol (1)
;;      Parents: `dbus-proxy-remote-object' and the classes created in
;;               4.b.1
;; 5. Create an instance of the class from 4.a or (3) for the object
;;    at the specified object path
;;
;; This algorithm is implemented by:
;;
;; + `dbus-proxy-make-remote-proxy'    (1. 2. 5.)
;; + `dbus-proxy-make-interface-class' (4.b.1.)
;; + `dbus-proxy-make-object-class'    (4.b.2.)
;; + `dbus-proxy-make-method-name'     (4.b.1.3.b.2.1)
;; + `dbus-proxy-make-method'          (4.b.1.3.b.2.2)
;; + `dbus-proxy-make-property-name'   (4.b.1.3.b.3.1)
;; + `dbus-proxy-make-signal-name'     (4.b.1.3.b.4.1)
;;
;; TODO mention properties


;;; History:
;;
;; 0.3 - Properties
;;     - Signals
;;
;; 0.2 - Method name transformations
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl-lib))

(require 'eieio)

(require 'dbus)
(require 'dbus-introspection)


;;; Conditions
;;

;; no-such-property

(put 'no-such-property 'error-conditions
     '(error no-such-property))

(put 'no-such-property 'error-message
     "The requested property does not exist.")

;; no-such-signal

(put 'no-such-signal 'error-conditions
     '(error no-such-signal))

(put 'no-such-signal 'error-message
     "The requested signal does not exist.")


;;; Proxy creation
;;

(defvar dbus-proxy-force-global-redefine nil
  "If this variable is non-nil, every proxy creation leads to the
redefinition of the involved object and interface classes.")

(defvar dbus-proxy-enable-logging nil
  "If this variable is non-nil, logging messages will be emitted
during the creation of proxy classes and objects.")

(defun dbus-proxy-make-remote-proxy
  (bus service object
   &optional redefine-classes proxy-object-class)
  "Create and return proxy object for OBJECT of SERVICE on BUS.

If necessary, proxy classes are created for OBJECT and the
interfaces it implements.

The first time an object of a specific class (read: a set of
interfaces) is created, the process can take some time since D-Bus
introspection and the definition of several classes are
required. However, subsequent proxy objects of the same class can
be created without the overhead."

  ;; TODO handle case in which object is not found

  ;; Retrieve names of all interfaces implemented by the object and
  ;; get the corresponding proxy class, creating it if required.
  (let* ((interfaces (dbus-introspect-get-interface-names
		      bus service object))
	 (class      (dbus-proxy-ensure-object-class
		      interfaces bus service object
		      redefine-classes proxy-object-class)))

    ;; The proxy object is an instance of that class. Create the
    ;; instance, passing it the object address.
    (make-instance class
		   (concat (symbol-name bus) ":" service ":" object)
		   :bus     bus
		   :service service
		   :object  object))
  )


;;; Class `dbus-proxy-remote-object'
;;

(defclass dbus-proxy-remote-object ()
  ((bus     :initarg :bus
	    :type    keyword
	    :reader  dbus-object-bus
	    :documentation
	    "The bus this D-Bus object lives on.")
   (service :initarg :service
	    :type    string
	    :reader  dbus-object-service
	    :documentation
	    "The well known service name held by this object.")
   (object  :initarg :object
	    :type    string
	    :reader  dbus-object-path
	    :documentation
	    "The path of this object."))
  "Objects of this class represent D-Bus objects.

Methods of the associated D-Bus object can be called like ordinary
methods.

Properties of the associated D-Bus object are made available using
virtual slots that behave like regular slots."
  :abstract t)

(cl-defmethod slot-missing ((this dbus-proxy-remote-object)
			 slot-name operation &optional new-value)
  "Implement access to slot SLOT-NAME of THIS by calling D-Bus methods."
  (cond
   ;; If SLOT-NAME is a keyword, strip the leading colon and try
   ;; again.
   ((keywordp slot-name)
    (slot-missing
     this
     (intern (substring (symbol-name slot-name) 1))
     operation new-value))

   ;; If SLOT-NAME is a symbol, we have to look up the interface and
   ;; property name to which SLOT-NAME maps. Ask our parents (which
   ;; are all interface classes) to do this.
   ((symbolp slot-name)
    (slot-missing this (dbus-proxy-find-property this slot-name)
		  operation new-value))

   ;; If SLOT-NAME is a cons cell, it contains the interface and
   ;; property name.
   ((listp slot-name)
    (with-slots (bus service object) this
      (destructuring-bind (interface property) slot-name
	(cond

	 ;; Read access
	 ((eq operation 'oref)
	  (dbus-get-property bus service object interface property))

	 ;; Write access
	 ((eq operation 'oset)
	  (dbus-set-property bus service object interface property
			     new-value))

	 ;; Invalid operation
	 (call-next-method))))))
  )

(cl-defmethod dbus-proxy-connect ((this dbus-proxy-remote-object) signal handler)
  "Connect HANDLER to SIGNAL of THIS.
SIGNAL can either be a symbol or a list of the
form (INTERFACE-NAME SIGNAL-NAME). When the first form is used,
the interface providing the signal will be determined
automatically. An error is signaled if SIGNAL is not contained in
any of the interfaces provided by THIS."
  (if (listp signal)
      ;; If SIGNAL is a cons cell, it contains the interface and
      ;; signal name.
      (with-slots (bus service object) this
	(dbus-register-signal
	 bus service object (first signal) (second signal) handler))
    ;; Otherwise SIGNAL is just a symbol and we have to look up the
    ;; interface and signal name to which SIGNAL maps. Ask our parents
    ;; (which are all interface classes) to do this.
    (dbus-proxy-connect this (dbus-proxy-find-signal this signal) handler)))

(cl-defmethod dbus-proxy-disconnect ((this dbus-proxy-remote-object) registration)
  "Disconnect HANDLER from SIGNAL of THIS."
  (dbus-unregister-object registration))

(defclass dbus-proxy-interface-object ()
  ((interface  :allocation :class
	       :type       string
	       :documentation
	       "The name of the interface to which a particular
subclass corresponds.")
   (properties :allocation :class
	       :type       list
	       :documentation
	       "TODO")
   (signals    :allocation :class
	       :type       list
	       :documentation
	       "TODO"))
  "Subclasses of this correspond to D-Bus interfaces.")

(cl-defmethod object-print ((this dbus-proxy-interface-object)
			 &rest strings)
  "Return a textual representation of THIS."
  (with-slots (bus service object) this
    (let ((simple-bus     (eq bus :session))
	  (simple-service (string=
			   (concat "/"
				   (replace-regexp-in-string
				    "\\." "/" service))
			   object)))
      (if (and simple-bus simple-service)
	  (concat "#<dbus-proxy " object ">")
	(concat "#<dbus-proxy"
		(unless simple-bus
		  (concat " bus: " (symbol-name bus)))
		(unless simple-service
		  (concat " service: " service))
		" object: "
		object
		strings
		">"))))
  )


;;; Variables
;;

(defvar dbus-proxy-transform-property-name-function
  #'dbus-proxy-transform-camel-case
  "The value of this is called to transform property names.")

(defvar dbus-proxy-transform-signal-name-function
  #'dbus-proxy-transform-camel-case
  "The value of this is called to transform signal names.")

(defvar dbus-proxy-transform-method-name-function
  #'dbus-proxy-transform-method-name
  "The value of this is called to transform method names.")


;;; Code generation
;;

(defun dbus-proxy-make-property-name (name)
  "Transform NAME to make it usable as a slot name."
  (intern
   (funcall dbus-proxy-transform-property-name-function name)))

(defun dbus-proxy-make-signal-name (name)
  "Transform NAME into a nice signal name."
  (intern
   (funcall dbus-proxy-transform-signal-name-function name)))

(defun dbus-proxy-make-method-name (name)
  "Transform NAME to make it usable as a LISP function name."
  (let ((preferred-name
	 (funcall dbus-proxy-transform-method-name-function name)))
    (if (dbus-proxy-symbol-unsuitable-for-method
	 (intern preferred-name))
	(let ((fallback (concat preferred-name "1")))
	  (warn "Preferred method name `%s' in use; falling back to `%s'"
		preferred-name fallback)
	  (dbus-proxy-make-method-name fallback))
      preferred-name)))

(defun dbus-proxy-make-method (interface method)
  "Construct a stub method for METHOD on INTERFACE."
  (let* ((interface-name      (dbus-introspection-interface-name interface))
	 (interface-symbol    (dbus-proxy-make-interface-class-symbol
			       interface-name))
	 (dbus-method-name    (dbus-introspection-method-name method))
	 (emacs-method-name   (dbus-proxy-make-method-name
			       dbus-method-name))
	 (method-symbol       (intern emacs-method-name))
	 (dbus-method-args    (remove-if-not
			       #'dbus-introspection-arg-p (cddr method)))
	 (dbus-method-in-args (remove-if-not
			       #'dbus-introspection-arg-in-p
			       dbus-method-args))
	 (formal-args         (let ((count 0))
				(mapcar
				 (lambda (arg)
				   (cl-incf count)
				   (make-symbol
				    (or (dbus-introspection-arg-name arg)
					(format "arg-%d" count))))
				 dbus-method-in-args)))
	 (call-args           (dbus-proxy-make-call-args
			       dbus-method-in-args formal-args))
	 (doc                 ""))

    (when dbus-proxy-enable-logging
      (message "Defining interface method   %s:%s with signature `%s' as %s"
	       interface-name dbus-method-name
	       (mapconcat #'dbus-introspection-arg-type dbus-method-args "")
	       emacs-method-name))

    ;; Define method.
    (eval
     `(cl-defmethod ,method-symbol ((this ,interface-symbol) ,@formal-args)
	,doc
	(with-slots (bus service object) this
	  (dbus-call-method
	   bus service object ,interface-name ,dbus-method-name
	   ,@call-args))))

    ;; Return the symbol
    method-symbol)
  )


;;; Interface Class
;;

(defun dbus-proxy-make-interface-class-symbol (interface)
  ""
  (intern interface))

(defun dbus-proxy-get-interface-class (interface)
  "Return the proxy class for INTERFACE or nil."
  (let ((class (intern-soft
		(symbol-name
		 (dbus-proxy-make-interface-class-symbol interface)))))
    (when (class-p class)
      class))
  )

(defun dbus-proxy-make-interface-class (interface) ;; interface name or interface object?
  ""
  (let* ((class-name     (dbus-introspection-interface-name interface))
	 ;; EIEIO globally binds the name `class-symbol'
	 (class-symbol-1 (dbus-proxy-make-interface-class-symbol
			  class-name))
	 (properties     (mapcar
			  (lambda (property)
			    (let ((name (dbus-introspection-property-name
					 property)))
			      (cons (dbus-proxy-make-property-name name)
				    name)))
			  (dbus-introspection-interface-properties
			   interface)))
	 (signals        (mapcar
			  (lambda (signal)
			    (let ((name (dbus-introspection-signal-name
					 signal)))
			      (cons (dbus-proxy-make-signal-name name)
				    name)))
			  (dbus-introspection-interface-signals
			   interface)))
	 (doc            (format
			  "Abstract class implementing the interface %s"
			  class-name)))

    (when dbus-proxy-enable-logging
      (message "Defining interface class  %s" class-name))

    ;; Define the class representing the interface.
    (eval
     `(defclass ,class-symbol-1 (dbus-proxy-interface-object)
	((interface  :initform ,class-name)
	 (properties :initform ,properties)
	 (signals    :initform ,signals))
	,doc
	:abstract t))

    ;; Define the property finder method for this interface.
    (dolist (property properties)

      (when dbus-proxy-enable-logging
	(message "Defining interface property %s:%s as %s"
		 class-name (cdr property) (car property)))

      ;; Generate an accesssor for the property.
      (let ((accessor-name (intern
			    (concat class-name "-" (car property)))))
	(eval
	 `(cl-defmethod ,accessor-name ((this ,class-symbol-1))
	    (slot-value this (quote ,(car property)))))))

    (eval
     `(cl-defmethod dbus-proxy-find-property ((this ,class-symbol-1) property)
	"TODO"
	(let ((property-info (assoc property (oref ,class-symbol-1 properties))))
	  (cond
	   ;; Found property info return this interface and the property
	   ;; name.
	   (property-info
	    (list (oref ,class-symbol-1 interface)
		  (cdr property-info)))

	   ;; Not found, but there is another interface class. Try it,
	   ((next-method-p)
	    (call-next-method))

	   ;; The property could not be found in any interface
	   ;; class. signal an error.
	   (t
	    (signal 'no-such-property (list property)))))))

    ;; Define the signal finder method for this interface.
    (dolist (signal signals)
      (when dbus-proxy-enable-logging
	(message "Defining interface signal   %s:%s as %s"
		 class-name (cdr signal) (car signal))))

    (eval
     `(cl-defmethod dbus-proxy-find-signal ((this ,class-symbol-1) signal)
	"TODO"
	(let ((signal-info (assoc signal (oref ,class-symbol-1 signals))))
	  (cond
	   ;; Found signal info return this interface and the signal
	   ;; name.
	   (signal-info
	    (list (oref ,class-symbol-1 interface)
		  (cdr signal-info)))

	   ;; Not found, but there is another interface class. Try it,
	   ((next-method-p)
	    (call-next-method))

	   ;; The signal could not be found in any interface
	   ;; class. Signal an error.
	   (t
	    (signal 'no-such-signal (list signal)))))))

    ;; Define methods for the method elements of the interface.
    (dolist (method (dbus-introspection-interface-methods interface))
      (dbus-proxy-make-method interface method))

    ;; Return the symbol.
    class-symbol-1)
  )

(defun dbus-proxy-ensure-interface-class (interface &optional redefine)
  ""
  (or (and (not (or redefine dbus-proxy-force-global-redefine))
	   (dbus-proxy-get-interface-class
	    (dbus-introspection-interface-name interface)))
      (dbus-proxy-make-interface-class interface)))


;;; Object Class
;;

(defun dbus-proxy-make-object-class-symbol (interfaces)
  ""
  (intern
   (mapconcat #'identity
	      (sort (copy-list interfaces) #'string<)
	      "-"))
  )

(defun dbus-proxy-get-object-class (interfaces)
  "Return the proxy class for INTERFACES or nil."
  ;; Construct the symbol under which the class should be
  ;; stored. Check whether it is there.
  (let ((class (intern-soft
		(symbol-name
		 (dbus-proxy-make-object-class-symbol interfaces)))))
    (when (class-p class)
      class))
  )

;; TODO &optional interfaces
(defun dbus-proxy-make-object-class
  (interfaces
   &optional redefine-interface-classes proxy-object-class)
  ""
  ;; Make sure there are proxy classes for all the interfaces and
  ;; define the class.
  (let (;; EIEIO globally binds the name `class-symbol'
	(class-symbol-1 (dbus-proxy-make-object-class-symbol
			 (mapcar #'dbus-introspection-interface-name
				 interfaces)))
	(parents        (cons (or proxy-object-class
				  dbus-proxy-remote-object)
			      (mapcar
			       (lambda (interface)
				 (dbus-proxy-ensure-interface-class
				  interface redefine-interface-classes))
			       interfaces)))
	(doc            (format
			 "A class implementing the following D-Bus interfaces:\n\n+ %s."
			 (mapconcat #'dbus-introspection-interface-name
				    interfaces "\n+ "))))

    (when dbus-proxy-enable-logging
      (message "Defining object    class  %s"
	       (mapconcat #'dbus-introspection-interface-name
			  interfaces "-")))

    (eval
     `(defclass ,class-symbol-1 (,@parents)
	()
	,doc
	:method-invocation-order :c3))

    ;; Return the symbol.
    class-symbol-1)
  )

(defun dbus-proxy-ensure-object-class
  (interfaces bus service object
   &optional redefine proxy-object-class)
  ""
  (or (and (not (or redefine dbus-proxy-force-global-redefine))
	   (dbus-proxy-get-object-class interfaces))
      ;; If necessary, retrieve full interface information and define
      ;; the object class.
      (dbus-proxy-make-object-class
       (mapcar
	(lambda (interface)
	  (dbus-introspect-get-interface bus service object interface))
	interfaces)
       (or redefine dbus-proxy-force-global-redefine))
      proxy-object-class))


;;; Utility Functions
;;

(defun dbus-proxy-make-call-args (dbus-args formal-args)
  "Generate typed call arguments based on DBUS-ARGS and FORMAL-ARGS."
  (let ((dbus-args-rest   dbus-args)
	(formal-args-rest formal-args)
	(result))
    (while (and dbus-args-rest formal-args-rest)
      (let* ((formal-arg (car formal-args-rest))
	     (dbus-arg   (car dbus-args-rest))
	     (type       (dbus-introspection-arg-type dbus-arg)))
	;; For simple types, look up the D-Bus type designator and push
	;; it onto the call argument list.
	(when (= (length type) 1)
	  (let ((designator (cdr (assoc (aref type 0)
					dbus-introspection-simple-type-codes))))
	    (if designator
		(push designator result)
	      (warn "Could not find type designator for type `%s'; \
emitting `%s' as untyped argument."
		    type (dbus-introspection-arg-name dbus-arg)))))

	;; Push the argument variable unconditionally.
	(push formal-arg result)

	;; Advance to next pair of arguments.
	(setq dbus-args-rest   (cdr dbus-args-rest)
	      formal-args-rest (cdr formal-args-rest))))
    (nreverse result)))

(defun dbus-proxy-symbol-unsuitable-for-method (symbol)
  "Check whether a D-Bus method can be defined on SYMBOL."
  (and (fboundp symbol)
       (not (generic-p symbol))
       (or (byte-code-function-p (symbol-function symbol))
	   (not (eq 'autoload (car-safe (symbol-function symbol)))))))

(defun dbus-proxy-transform-method-name (name)
  "Transform NAME into suitable LISP function name.
The following literal translations are applied an addition to the
transformation performed by `dbus-proxy-transform-camel-case':

+ Get -> prop-get
+ Set -> prop-set"
  (cond
   ((string= name "Get")
    "prop-get")

   ((string= name "Set")
    "prop-set")

   (t
    (dbus-proxy-transform-camel-case name))))

(defun dbus-proxy-transform-camel-case (name)
  "Transform NAME from CamelCase to dash-separated lower case.
Examples:

+ CamelCase        -> camel-case
+ UPPERCASE        -> uppercase
+ PARTIALUppercase -> partial-uppercase"
  (let ((case-fold-search nil))
    (downcase
     ;; Replace camel-case word boundaries like aB with a-b.
     (replace-regexp-in-string
      (rx (seq (group (char lower)) (group (char upper))))
      "\\1-\\2"
      ;; Replace multiple uppercase letters by capitalized version to
      ;; ensure their treatment as a single word in the next
      ;; step. Example: RGBColor -> RgbColor
      (replace-regexp-in-string
       (rx (seq (group (1+ (char upper))) (char lower)))
       (lambda (match)
	 (concat (capitalize (substring match 0 -2))
		 (substring match -2)))
       name t) t))))

(provide 'dbus-proxy)
;;; dbus-proxy.el ends here
