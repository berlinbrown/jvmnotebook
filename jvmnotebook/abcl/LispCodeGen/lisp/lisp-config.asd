;;;
;;; Berlin Brown
;;;
;;; $Id$
;;;
;;; lisp-config.asd
;;;
;;; To add: asdf:operate 'asdf:load-op :lispgen :force nil
;;;

(defpackage :org.lispgen.asd (:use :asdf :cl))
(in-package :org.lispgen.asd)

(asdf:defsystem :lisp-config
  :name "lisp-config"
  :author "Berlin Brown"
  :version "0.0"
  :depends-on NIL
  :components
  ((:file "package")
   (:file "lisp-unit" :depends-on ("package"))
   (:file "lisp-config" :depends-on ("package"))
))

;;; End of File