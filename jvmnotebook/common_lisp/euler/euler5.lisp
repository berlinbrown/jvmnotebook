;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number Five
;; Target Environment: Common Lisp -> Clisp/Sbcl 
;; Date: 5/19/2008
;; --------------
;;
;; 2520 is the smallest number that can be divided by 
;; each of the numbers from 1 to 10 without any remainder.
;;
;; What is the smallest number that is evenly divisible 
;; by all of the numbers from 1 to 20?
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://www.unixuser.org/~euske/doc/cl/loop.html
;;**********************************************

(defun all-evenly (n a b)
  (loop for i from a to b
        always (zerop (mod n i))))

(defun euler5 (max-n a b step)
  (let ((min max-n))
    (loop for i from 1 to max-n by step
        do
        (when (zerop (mod i 10000)) (format t "#[~f]#~%" (/ i max-n)))
        (when (all-evenly i a b)
          (when (< i min)
            (setf min i))))
    min))

(defun main ()
  (format t "INFO: Running Project Euler~%")
  ;; Good primer = 1,000,000,000 
  (print (euler5 50000000000 11 20 20))
  (format t "~%INFO: Done~%"))

(main)