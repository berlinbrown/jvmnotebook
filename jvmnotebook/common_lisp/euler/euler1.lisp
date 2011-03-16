;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number One
;; Target Environment: Clisp/Sbcl
;; Date: 5/13/2008
;; Euler Problem:
;; --------------
;; If we list all the natural numbers below 10 
;; that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
;; The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;;
;; Less verbose example:
;; http://anthonyf.wordpress.com/2006/05/03/project-euler-and-lisp/
;; -----------------
;;(defproblem 1 233168
;;  "Add all the natural numbers below 1000 that are multiples of 3 or 5."
;;  (loop for x from 3 to 999
;;        when (or (zerop (mod x 3))
;;                 (zerop (mod x 5)))
;;        sum x))
;;
;; Or:
;;
;; (+ (loop for x from 3 to 999 by 3 sum x)
;;	  (loop for x from 5 to 999 by 5 sum x))
;;**********************************************

(defun sum-list (lst)
  (loop for x in lst sum x))

(defun accum (val max lst)
  (if (<= val max)
    (accum (+ 1 val) max
           (cons val lst))
    lst))
(defun multiple-three-five (lst)
  (let ((new-lst '()))
    (dolist (val lst new-lst)
      (when (and
             (> val 0)
             (or (= (mod val 3) 0)               
                 (= (mod val 5) 0)))
          (push val new-lst)))))

(defun euler1 ()
  (let* ((acc-lst (accum 0 999 ()))
         (new-lst (multiple-three-five acc-lst)))
    (format t "Sum=~a~%" (sum-list new-lst))))
  
(defun main ()
  (format t "INFO: Running Project Euler~%")
  (euler1)
  (format t "INFO: Done~%"))

(main)

;; End of File

