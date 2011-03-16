;;
;; euler16.lisp

(defun euler16-a ()
  (let ((z (let* ((n (expt 2 1000))
		  (z (format nil "~a" n)))
	     (loop for i across z 
		   sum (digit-char-p i)))))
    (print z)))

(defun euler16-b ()
  "From user JonRock on Project Euler. Use of map and string funcs"
  (let* ((num (expt 2 1000))
	 (z (map 'list #'(lambda (char) 
			   (read-from-string (string char)))
		(prin1-to-string num)))
	 (res (reduce #'+ z)))
    (print res)))
	
(defun main ()
  (euler16-b)
  (format t "~%Done~%"))

(main)