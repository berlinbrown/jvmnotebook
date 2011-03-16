;;
;; Readfile package
;;
(in-package :org.lispgen.asd)

(defsystem lispgen
  :name "readfile"
  :author "Berlin Brown"
  :version "0.0"
  :maintainer "Berlin Brown"
  :licence "BSD"
  :description "Lisp Applications"
  :depends-on (:codegen-lib)
  :components
  ((:file "lispgen")
   ))

;;
;; End of file
;;
  