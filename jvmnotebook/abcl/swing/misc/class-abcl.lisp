;;
;; Working with classes and jfli
(load "../jfli-abcl.lisp")
(defpackage :simple1
  (:use :common-lisp :java :jfli))

(in-package :simple1)

;; ** ABCL Lisp Helpers
;;----------------------------
;; Example static method call "System.getenv('KEY')
;; (jstatic "getenv" (jclass "java.lang.System") "KEY")
;;
;; jfli:new-class:
;; ---------------------------
;;
;; defmacro new-class (
;;  class-name, super-and-interface-names, 
;;        constructor-defs, method-defs field-defs)
;;
;; Creates, registers and returns a Java object 
;; that implements the supplied interfaces
;; --------------------
;; Arguments: 
;;
;;  * [!] class-name -> string
;;  * [!] super-and-interface-names -> class-name | (class-name interface-name*)
;;  * [!] constructor-defs -> (constructor-def*)
;;  * constructor-def -> (ctr-arg-defs body) 
;;          /the first form in body may be (super arg-name+); this will call the constructor of the superclass
;;           with the listed arguments/
;;  * ctr-arg-def -> (arg-name arg-type)
;;  * method-def -> (method-name return-type access-modifiers arg-defs* body)
;;          /access-modifiers may be nil (to get the modifiers from the superclass), a keyword, or
;;           a list of keywords/
;;  * method-name -> string 
;;  * arg-def -> arg-name | (arg-name arg-type)
;;  * arg-type -> \"package.qualified.ClassName\" | classname. | :primitive
;;  * class-name -> \"package.qualified.ClassName\" | classname. 
;;  * interface-name -> \"package.qualified.InterfaceName\" | interfacename. 
;;
;; Example new-class usage:
;;(new-class
;; "SERVLET1.Servlet1" --> ### class name ###
;; "javax.servlet.GenericServlet"  --> ### super and interface names ###
;; () --> constructor def
;; --> ### method def below ###
;; (("service" :void :public --> method
;;   ((req "javax.servlet.ServletRequest") --> ### arguments ###
;;	(resp "javax.servlet.ServletResponse")) 
;;   --> ### method implementation ###
;;   (with-simple-restart (abort "Exit to hyperspace")
;;						(servlet1-service req resp))))
;; ()
;; )
;; This is an instance of servlet handler object, that used by bridge Java code
;; (defparameter *servlet* (jfli::ref (SERVLET1::servlet1.new)))
;;----------------------------

(defun simple-it ()
  (with-simple-restart 
   (abort "Abort servlet execution")
   (progn (format "err"))))

(jfli:new-class
			"simple1.MyParentClassTestName"
			"org.SimpleParent"
			()
			(("testParentPrintVoid" :void :public
			  ((arg1 "java.lang.String")
			   (arg2 "java.lang.String"))
			  (with-simple-restart (abort "Exit to hyperspace")
								   (simple-it))
			  ))
			())

(setf *gg* (jnew (jconstructor "simple1.MyParentClassTestName")))

(format t "~a~%" *gg*)

(jcall (jmethod "simple1.MyParentClassTestName" "testParentPrintVoid" 
				"java.lang.String" "java.lang.String") *gg* "1" "2")

;; End of Script