;;
;; Load all modules through the ASDF system
;;

(load "asdf")
(setf asdf:*central-registry* '(*default-pathname-defaults*))

;;;
;;; Add the examples,test, and wtk-src directories
;;;
(progn
  ;;;
  ;;; Set the root path
  ;;;
  (defparameter *widget-toolkit-root*
    (make-pathname :directory '(:relative)))		  
  (push (merge-pathnames
         (make-pathname :directory '(:relative "org" "codegen"))
	 *widget-toolkit-root*)
        asdf:*central-registry*)
  (push (merge-pathnames
         (make-pathname :directory '(:relative "org" "tests"))
	 *widget-toolkit-root*)
        asdf:*central-registry*))

;;;
;;; Load the *.asd files
;;;		  
(asdf:operate 'asdf:load-op :lisp-config :force nil)
(asdf:operate 'asdf:load-op :codegen-lib :force nil)
(asdf:operate 'asdf:load-op :test-suite :force nil)
(asdf:operate 'asdf:load-op :lispgen :force nil)

;;; End of File ;;