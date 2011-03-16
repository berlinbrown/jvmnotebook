;;
;; euler6.lisp

(defun sum-squares (n)
  (loop for i from 1 to n 
	sum (* i i)))
(defun square-sum (n)
  (let ((a (loop for i from 1 to n
		 sum i)))
    (* a a)))

(defun main ()
  (format t "Running Program")
  (print (- (square-sum 100) (sum-squares 100)))
  (format t "Done"))

(main)