;;
;; euler17.lisp (neophyte version)
;; (needs to be refactored)

(defparameter *single-set*
  '("zero" "one" "two" "three" "four" "five"
    "six" "seven" "eight" "nine" "ten"
    "eleven" "twelve" "thirteen" "fourteen"
    "fifteen" "sixteen" "seventeen" "eighteen"
    "nineteen" "twenty"))
(defparameter *higher-set*
  '("zero" "one" "twenty" "thirty" 
    "forty" "fifty" "sixty"
    "seventy" "eighty" "ninety" "hundred"))

(defun digit-mod (digit i)
  (cond ((eql digit :one-digit) (nth i *single-set*))
	((eql digit :two-digit)
	 (if (and (>= i 2) (<= i 9)) (nth i *higher-set*) ""))
	((eql digit :three-digit) 
	 (if (= i 10) (nth i *higher-set*) ""))
	(t "")))
(defun to-word (a b c mod)
  (when (string-equal c "zero")
    (setf c ""))
  (format nil "~a~a~a~a" a mod b c))
(defun to-word-3 (a b mod)
  (when (= (length b) 0) (setf mod "hundred"))
  (when (string-equal b "zero")
    (setf b ""))
  (format nil "~a~a~a" a mod b))

(defun dig (val rad) (floor (/ val (expt 10 rad))))
(defun dig-2 (val rad) (- val (* (dig val rad) (expt 10 rad))))

(defun two-digit-a (i)
  "Convert numbers to english, ranging from 21 to 99"  
  (let ((res (if (and (>= i 11) (<= i 19))
		 (nth i *single-set*)
	       (to-word ""
			(digit-mod :two-digit (dig i 1))
			(digit-mod :one-digit (dig-2 i 1)) ""))))
    res))
(defun three-digit (i)
  (let ((a (dig i 2))
	(b (dig-2 i 2)))
    (to-word-3 (nth a *single-set*) (two-digit-a b) "hundredand")))
	     
(defun number-to-word (strt  n)
  "Convert the digit to english word"
  (loop for i from strt to n
	collect (cond ((<= i 20) (nth i *single-set*))
		      ((and (> i 20) (< i 1000))
		       (cond ((and (>= i 21) (<= i 99))
			      (two-digit-a i))
			     ((= i 100) "onehundred")
			     ((> i 100)
			      (three-digit i))))
		      ((= i 1000) "onethousand")
		      (t ""))))

(defun count-str (str)
  (length (remove #\Space (remove #\- str))))

(defun main-b ()
  (format t "Sum of lets from one to one thousand: ~A~%"
	  (loop for x from 1 to 1000
		do (print (format nil "~r" x))
		sum (count-str (format nil "~r" x)))))

(defun main-a ()
  (print (loop for i in (number-to-word 1 1000)
	       do (print i)
	       sum (length i))))

(main-b)