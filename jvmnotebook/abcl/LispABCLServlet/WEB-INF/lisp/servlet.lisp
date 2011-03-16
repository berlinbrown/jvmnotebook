;;
;; Servlet.lisp
;;

;;==========================================================
(defpackage :servlet-gen
  (:use :common-lisp :jfli))

(in-package :servlet-gen)

;; Servlet stuff
(def-java-class "javax.servlet.ServletResponse")
(def-java-class "java.io.PrintWriter")
(def-java-class "javax.servlet.http.HttpServletResponse")
(def-java-class "javax.servlet.http.HttpServletRequest")
(def-java-class "javax.servlet.http.HttpSession")

(def-java-class "java.util.Enumeration")

(defpackage :servlet1
  (:use :common-lisp :java
	:jfli :servlet-gen
	:lml2
	"java.lang"
	"java.io"
	"java.util"
	"javax.servlet"
	"javax.servlet.http"))

;;==========================================================

(in-package :servlet1)

(defparameter *hit-count* 0)

;; Helper functions and macros

(defmacro let-if ((var test-form) if-true &optional if-false)
  `(let ((,var ,test-form))
      (if ,var ,if-true ,if-false)))

(defmacro str+ (&rest strings)
  `(concatenate 'string ,@strings))

(defun str->int (str)
  (ignore-errors (parse-integer str)))

(defun unbox (obj)
  (if (java:java-object-p obj)
      (java:jobject-lisp-value obj)
      obj))

;; Servlet entry functions that are redefined in app

(defun analyze-request (request)
	(declare (ignore request)))

(defun process-request (request analyzed-request)
	(declare (ignore request analyzed-request)))

(defun generate-output (request analyzed-request processed-request response)
	(declare (ignore request analyzed-request processed-request
							 response )))

(defun servlet1-service (request response)
  (with-simple-restart (abort "Abort servlet execution")
		       (let* ((analyzed-request
			       (loop (with-simple-restart 
				      (retry "Retry analyze request")
				      (return (analyze-request request)))))
			      (processed-request
			       (loop (with-simple-restart 
				      (retry "Retry process request ~S" analyzed-request)
				      (return (process-request request analyzed-request))))))
			 (loop (with-simple-restart 
				(retry "Retry generating output for ~S, ~S"
				       analyzed-request processed-request)
				(return (generate-output request analyzed-request processed-request
							 response)))))))

(new-class "SERVLET1.Servlet1" "javax.servlet.GenericServlet" ()
	   (("service" :void :public ((req "javax.servlet.ServletRequest")
				      (resp "javax.servlet.ServletResponse"))
				      (with-simple-restart (abort "Exit to hyperspace")
					     (servlet1-service req resp))))
	   ())

(print "loading CPS-transform")
(loop for file in '("one-liners.lisp" "flow-control.lisp" "list.lisp" "string.lisp" "cps.lisp")
      do (format t "loading ~a ~%" file)
      do (load (merge-pathnames (str+ "cps/" file) *load-pathname*)))

;;==========================================================

(in-package :cl-user)

;; This is an instance of servlet handler object, that used by bridge Java code
(defparameter *servlet* (jfli::ref (SERVLET1::servlet1.new)))

(with-simple-restart 
 (abort "Abort loading application code")
 (loop for module in '("cont" "app" "app-pages" "app-logic")
   do (format t "loading app module ~a~%" module)
   do (with-simple-restart 
       (abort (format nil "Abort loading ~a" module))
       (loop 
	(with-simple-restart (retry (format nil "Retry loading ~a" module))
	  (return (load (merge-pathnames
			 (make-pathname :name module
					:type "lisp")
			 *load-pathname*))))))))

;; End of File