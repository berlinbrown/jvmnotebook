;;
;; Berlin Brown
;;
;; $Id$
;;
;; To add:  asdf:operate 'asdf:load-op :lispgen :force nil
;; 

(in-package :org.lispgen.asd)

(asdf:defsystem :test-suite
  :name "test-suite"
  :author "Berlin Brown"
  :version "0.0"
  :depends-on (:codegen-lib :lisp-config)
  :components
  ((:file "test-suite")
))

