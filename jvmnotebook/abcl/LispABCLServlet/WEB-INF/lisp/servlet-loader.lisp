;;
;; Berlin Brown
;;  - Lisp Based Servlet Startup
;;

(require 'asdf)
(setq *warn-on-redefinition* nil)

(defvar *jfli-abcl-loaded* nil)

(with-simple-restart (abort "Abort loading servlet")  
  (when (not (probe-file 
	      (merge-pathnames #p"packages/jfli-abcl/jfli-abcl.abcl" 
			       *load-pathname*)))

    (print "INFO: Compiling jfli-abcl..")
    (compile-file 
     (merge-pathnames #p"packages/jfli-abcl/jfli-abcl.lisp" *load-pathname*))
    
    (print "INFO: Compiling lml2..")
    (pushnew (merge-pathnames #p"packages/lml2/" *load-pathname*) 
	     asdf:*central-registry* :test 'equal)
    (asdf:oos 'asdf:compile-op :lml2))
  
  (unless *jfli-abcl-loaded*
    (print "INFO: Loading jfli-abcl..")
    (load (merge-pathnames #p"packages/jfli-abcl/jfli-abcl.abcl" 
			   *load-pathname*))
    (setq *jfli-abcl-loaded* t))
  
  (print "INFO: Loading lml2..")
  (pushnew (merge-pathnames #p"packages/lml2/" *load-pathname*) asdf:*central-registry* :test 'equal)
  (asdf:oos 'asdf:load-op :lml2)
  
  (print "INFO: Loading servlet code..")
  (loop (with-simple-restart (retry "Retry loading servlet")
	  (return (load 
		   (merge-pathnames "servlet.lisp" 
				    *load-pathname*))))))

;; End of File ;;