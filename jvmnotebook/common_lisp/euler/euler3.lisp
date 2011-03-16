;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number Three
;; Target Environment: Clisp/Sbcl
;; Date: 5/13/2008
;; Euler Problem (3):
;; --------------
;;
;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://en.wikipedia.org/wiki/Prime_number
;; [3] http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
;; [4] http://www.unixuser.org/~euske/doc/cl/loop.html
;;**********************************************

(defun div-mod (num prime)
  "divmod, return quotient and remainder as a list tuple"
  (list (floor (/ num prime)) (rem num prime)))
	
(defun find-prime (n lst)
  " Loop through all of the primes and check"
  (let ((max 0))
	(loop for prime in (reverse lst)
	   do (let ((a (first (div-mod n prime)))
				(b (second (div-mod n prime))))
			(when (zerop b) (setf max prime))))
	max))
 
(defun sieve-primes(n)
  "Use Sieve_of_Eratosthenes to find all of the primes up to the number.
 [1] create an array and list with initial value 1
 [2] Loop from 2 to n
 [3] Using the step algorithm to nullify the multiples
 Return a list of primes "
  (let ((arr (make-array n :initial-element 1))
        (prime-lst '()))
    (loop for i from 2 to (1- n)
          do (let ((v (aref arr i)))
               (when v
                 (loop for i from (* i 2) to (1- n) by i 
                       do (setf (aref arr i) nil))
                 (push i prime-lst))))
    prime-lst))

(defun main ()
  (format t "INFO: Running Project Euler~%")
  (let ((n 775146))
	(print (find-prime n (sieve-primes n))))  
  (format t "~%INFO: Done~%"))

(main)