;;
;; euler8.lisp (modify to find the "correct" solution)
;; Author: Berlin Brown
;; License: Public Domain
;;
;; Find the greatest product of five 
;; consecutive digits in the 1000-digit number
;;
;; [1] http://www.unixuser.org/~euske/doc/cl/loop.html
;;
;; C/Procedural version from an euler user:
;; (cyprys)
;; ---------------------------
;;int main(void) {
;;      int n[1000] = {7,3,1,6,7,1, ... ,6,3,4,5,0};
;;      int maxSum,tmpSum,i,y;
;;      maxSum=tmpSum=i=y=0;
;;      for (i=0;i<995;i++) {
;;              tmpSum = n[i];
;;              for (y=1;y<5;y++)
;;                      tmpSum = tmpSum * n[i+y];
;; 
;;              if (tmpSum > maxSum)
;;                      maxSum = tmpSum;
;;      }
;;      return 0;
;;}
;; ---------------------------

(defparameter *large-num* 
"73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843")

(defun find-great-product ()
  "Use reduce and loop to find the product of a list of 5 digits out of the
 larger 1000 digit number"
  (let ((n (length *large-num*)) (m 0))
    (loop for i from 0 to (- n 5)
          for x = (let ((vals (loop for z from i to (+ i 4)
                                    for b = (digit-char-p (char *large-num* z))
                                    collect (if b b 1))))
                    (reduce #'* vals))
          do (when (> x m) (setf m x)))
    m))
(defun main ()
  (format t "Start~%")
  (print (find-great-product))
  (format t "Done~%"))
(main)