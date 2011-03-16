
(defun fact (n)
  (if (= n 0) 1
    (* n (fact (1- n)))))
(defun digits-list (num)
  (map 'list #'(lambda (char)
		 (read-from-string (string char)))
       (prin1-to-string num)))

(print (reduce #'+ (digits-list (fact 100))))