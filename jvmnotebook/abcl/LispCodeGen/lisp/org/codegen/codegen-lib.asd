;;
;; Berlin Brown
;;
;; $Id$
;;
;; codegen-lib.asd
;;
;; To add: asdf:operate 'asdf:load-op :lispgen :force nil
;; 

(in-package :org.lispgen.asd)

(asdf:defsystem :codegen-lib
  :name "codegen-lib"
  :author "Berlin Brown"
  :version "0.0"
  :depends-on ("lisp-config")
  :components
  ((:file "codegen-lib")
   (:file "java-template")
   (:file "codegen-php")
   (:file "css-object")
   (:file "php-model")
   (:file "php-controller")
   (:file "execute-view")
   (:file "php-database" :depends-on ("css-object" "execute-view"))
   (:file "php-view" :depends-on 
	  ("codegen-lib" "css-object" "php-database"
	   "execute-view"
	   ))
))

