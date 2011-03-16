;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number Two
;; Target Environment: Clisp/Sbcl
;; Date: 5/13/2008
;; Euler Problem (2):
;; --------------
;; Each new term in the Fibonacci sequence is generated 
;; by adding the previous two terms. By starting with 
;; 1 and 2, the first 10 terms will be:
;;
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;; 
;; Find the sum of all the even-valued terms in the 
;; sequence which do not exceed four million
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://www.cliki.net/Fibonacci
;;**********************************************

(defun sum-euler ()
  (+ (loop for x from 0 to 10 by 3 sum x)))

(defun fib (n)
  "Simple Generate Fibonacci Number (fib 10 = 55)"
  (if (< n 2)
      n
    (+ (fib (- n 1)) 
	   (fib (- n 2)))))

(defun tail-fib (n)
  "Tail-recursive computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
    (fib-aux n 0 1)))

(defun new-fib (n)
  "loop-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))

(defun check-eul (x)
  (if (and (evenp x) (< x 4000000)) t))

(defun tail-fib-eul (n)
  "Tail-recursive computation of the nth element of the Fibonacci sequence.
   With modifications for the euler problem"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (format t "-->~a ~a ~a~%" n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
           (fib-aux n 0 1)))

(defun list-fibs (limit)
  (let ((fibs '(2 1)))
    (do ((nextfib (+ (car fibs) (cadr fibs))
                  (+ (car fibs) (cadr fibs))))
        ((> nextfib limit))
      (setq fibs (cons nextfib fibs)))
    fibs))

(defun euler2-web ()
  "From JonRock (LISP)"
  (reduce #'+ (remove-if-not #'evenp (list-fibs 4000000))))

(defun euler2 ()
  "Driver function, invoke the euler calls"
  (progn
    (let ((e (fib 10))
          (f (tail-fib-eul 40))
          (z (euler2-web)))
      (format t "-->~a~%" e)
      (format t "-->(2)~a~%" f)
      (format t "-->(3)~a~%" z))))

(defun main ()
  (format t "INFO: Running Project Euler~%") 
  (euler2)
  (format t "INFO: Done~%"))

(main)

;; End of File

