;;
;; app-logic.lisp
;;
(in-package :servlet1)

(defmacro vcase (rs &rest cases)
  `(ecase (first ,rs)
     ,@cases))

(defmacro vbind (rs (&rest fields) &body body)
  (let ((rs-sym (gensym)))
    `(let* ((,rs-sym (cdr ,rs))
	    ,@(loop for f in fields
		    if (and (consp f) (eq (car f) 'int))
		    collect (list (second f)
				  `(str->int (getf ,rs-sym 
					      ,(intern (symbol-name (second f)) 
						       :keyword))))
		    else collect (list f
				       `(getf ,rs-sym ,(intern (symbol-name  f) :keyword))))
	    )
      ,@body)))

(defmacro vcase-bind (rs &rest cases)
  (let ((rs-sym (gensym)))
    `(let ((,rs-sym ,rs))
      (vcase ,rs-sym
       ,@(loop for case in cases
	       collect (list (first case)
			     `(vbind ,rs-sym ,(second case) 
			       ,@(cddr case))))))))

(defun/cc user-login ()
  (vcase-bind (gen-login-page)
	      (:login ((int user-id) password)
		      ;;here it should check password
		      user-id)
	      (:exit ())))

(defun/cc get-number ()
  (vbind (gen-enter-number-page)
	 ((int number))
	 number))

(defun/cc numbers-op (sym)
  (let* ((a (get-number))
	 (b (get-number)))
    (gen-show-result-page (format nil "~a ~a ~a = ~a"
				  a sym b (funcall sym a b)))))

(defun/cc show-demo1 ()
  (let-if (user-id (user-login))
	  (let ((username (cadr (assoc user-id *users*))))
	    (loop (vcase (gen-user-menu-page username)
			 (:add-numbers (numbers-op '+))
			 (:mul-numbers (numbers-op '*))
			 (:exit (return)))))))

(defun/cc show-loop-demo ()
  (loop named outer-loop
	for i from 1 to 3
	do (loop for j from 1 to 3
		 do (vcase (gen-num-page (format nil "~a ~a" i j))
			   (:next ())
			   (:exit (return-from outer-loop))))))


(defun/cc go-front-page ()
  (loop
   (vcase (gen-front-page)
	  (:demo1 (show-demo1))
	  (:loop-demo (show-loop-demo)))))

;;; End of File