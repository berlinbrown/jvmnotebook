;;
;; Berlin Brown
;;
;; By solving all fifty puzzles find the sum of the 3-digit 
;; numbers found in the top left corner of each solution grid; 
;; for example, 483 is the 3-digit number found in the top left 
;; corner of the solution grid above.

(defun make-sudoku-game ()
  (let ((rows (make-array 9 :initial-element 
			  (make-array 9 :initial-element 0))))
    (print rows)))

(defun main ()
  (format t "Running~%")
  (make-sudoku-game)
  (format t "Done~%"))
(main)