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
  ;;(loop for i from 104750 to 105010
  ;;	do (print (length (sieve-primes i))))
  (print (first (sieve-primes 104750)))
  (format t "Done:~%"))

(main)