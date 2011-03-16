;;
;; app.lisp
;;

(in-package :servlet1)

(defun get-request-parameters (request)
  (let ((param-names-enum (HttpServletRequest.getParameterNames request)))
    (loop while (Enumeration.hasMoreElements param-names-enum)
      for pname = (unbox (Enumeration.nextElement param-names-enum))
      collect (intern (string-upcase  pname) :keyword)
      collect (HttpServletRequest.getParameter request pname))))

(defun analyze-request (request)
  (let-if (session (HttpServletRequest.getSession request t))
	  (let ((request-parameters (get-request-parameters request)))
	    (let-if (cont-str (getf request-parameters :cont))
		    (let-if (cont (get-continuation (str->int cont-str)))
			    (let ((verb (getf request-parameters :verb)))
			      (format t "cont: ~a, verb: ~a~%" cont-str verb)
			      (list :continuation cont
				    :verb (intern (string-upcase verb) :keyword)
				    :params request-parameters))
			  (error "wrong continuation"))
		    ()))
	  (error "there is no session")))


(defun generate-page (analyzed-request processed-request)
  (with-output-to-string 
    (*html-stream*)
    (let-if (cont (getf analyzed-request :continuation))
	      (funcall cont (cons (getf analyzed-request :verb)
				  (getf analyzed-request :params)))
	    (go-front-page))))

(defun generate-output (request analyzed-request processed-request response)
  (format t "~A:generating output for ~A, ~A~%" *hit-count* analyzed-request processed-request)
  (HttpServletResponse.setContentType response "text/html; charset=UTF-8")
  (HttpServletResponse.setHeader response "Cache-Control" "no-cache")
  (let* ((out (ServletResponse.getWriter response))
	 (page (time (generate-page analyzed-request processed-request))))
    (PrintWriter.print out page)))

;; End of the File
