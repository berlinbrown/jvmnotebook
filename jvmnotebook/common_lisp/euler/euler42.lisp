;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number 42 (neophyte version)
;; Target Environment: Common Lisp -> Clisp/Sbcl 
;; Date: 5/19/2008
;; --------------
;; The nth term of the sequence of triangle numbers is given 
;; by, tn = 0.5 * n * (n+1); so the first ten triangle numbers are:
;;
;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;; 
;; By converting each letter in a word to a number corresponding to 
;; its alphabetical position and adding these values we form a word value. 
;; For example, the word 
;; value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
;; is a triangle number then we shall call the word a triangle word.
;;
;; Using words.txt (right click and 'Save Link/Target As...'), 
;; a 16K text file containing nearly two-thousand common English words, 
;; how many are triangle words?
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://www.unixuser.org/~euske/doc/cl/loop.html
;; [3] http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node154.html
;; [4] http://cl-cookbook.sourceforge.net/strings.html
;;**********************************************

(defconstant *max-tri-n* 30)

(defun split (string delim)
   "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them. E.g. #\Space"
   (loop for i = 0 then (1+ j)
          as j = (position delim string :start i)
          collect (subseq string i j)
          while j))
(defun triangle-num (n)
  " (0.5 * n * (n + 1))"
  (loop for i from 1 to n
	 collect (floor (* 0.5 (* i (1+ i))))))

(defun str-triangle (tri str)
  "Determine if a string is a triangle number"
  (if (member (loop for i across str
				 sum (- (char-code i) 64)) tri)
	  1 0))
(defun euler-items (items)
  (loop for str in items 
	 sum (str-triangle (triangle-num *max-tri-n*)
					   (subseq str 1 (1- (length str))))))
(defun euler-line (line)
  (print (euler-items (split line #\Comma))))

(defun read-words ()
  (with-open-file (stream "./words.txt")
	(do ((line (read-line stream nil)
			   (read-line stream nil)))
        ((null line))
	  (euler-line line))))
(defun main ()
  " Euler problem number 42"
  (format t "INFO: Running Project Euler~%")
  (time (read-words))
  (format t "~%INFO: Done~%"))
(main)