;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number Four
;; Target Environment: Common Lisp -> Clisp/Sbcl 
;; Date: 5/19/2008
;; --------------
;; A palindromic number reads the same both ways. The largest 
;; palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://www.unixuser.org/~euske/doc/cl/loop.html
;;**********************************************

(defun pow (n e)
  (if (<= e 1) 
      n
    (* n (pow n (1- e)))))

(defun num-str (num)
  (length (format nil "~d" num)))

(defun is-valid (str)
  (zerop (mod (num-str str) 2)))

(defun is-n-valid (a b)
  (is-valid (* a b)))

(defun split-half= (u v)
  "Split the string in half and compare"
  (let* ((str (format nil "~d" (* u v)))
         (n (length str))
         (x (/ (length str) 2))
         (a (reverse (subseq str 0 x)))
         (b (subseq str x n)))
    ;; Return a tuple of <boolean, first num, second num>
    (list (string= a b) u v)))

(defun palindrome (digits)
  (let ((d (1- (pow 10 digits)))
        (max (list 0 nil nil)))
    (loop for a from 1 to d 
          do (loop for b from 1 to d 
                   do (when (is-n-valid a b)
                        (when (first (split-half= a b))
                          (when (> (* a b) (first max))
                            (setf max (list (* a b) a b)))))
                   ))
    max))

(defun main ()
  (format t "INFO: Running Project Euler~%")
  (print (palindrome 3))
  (format t "~%INFO: Done~%"))

(main)