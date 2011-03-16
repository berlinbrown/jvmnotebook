;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number 54 (neophyte version)
;; Target Environment: Common Lisp -> Clisp/Sbcl 
;; Date: 5/19/2008
;; --------------
;; Description:
;; --------------
;;
;; The following common lisp code represents a solution to
;; the project euler problem number 54.
;; This is a neophyte/unoptizimed implementation.  Basically,
;; this may not represent the best code but it is intuitive
;; enough for those used to procedural languages to follow
;; along.
;;
;; Euler Description:
;; --------------
;; The file, poker.txt, contains one-thousand random 
;; hands dealt to two players. Each line of the file 
;; contains ten cards (separated by a single space): 
;; the first five are player one's cards and the 
;; last five are player two's cards. You can assume 
;; that all hands are valid (no invalid characters or repeated cards), 
;; each player's hand is in no specific order, 
;; and in each hand there is a clear winner.
;; 
;; How many hands does player one win?
;;
;; See:
;; http://projecteuler.net/index.php?section=problems&id=54
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://www.unixuser.org/~euske/doc/cl/loop.html
;; [4] http://cl-cookbook.sourceforge.net/strings.html
;;**********************************************

(defun process-line-stats (player-one player-two stats 
				      hand-one hand-two player-win) 
  (list hand-one hand-two))

(defun print-stats (player-one player-two stats) 
  stats)

(load "euler54_stats.lisp")

(defparameter *card-values* "23456789TJQKA")

(defstruct game-stats
  (total-games 0)
  (count-valid 0)
  (player-one 0)
  (player-two 0)
  (tie 0)
  (undetermined 0))

(defstruct player-stats
  (high-card-n 0)
  (one-pair-n 0)
  (two-pairs-n 0)
  (three-kind-n 0)
  (straight-n 0)
  (flush-n 0)
  (full-house 0)
  (four-kind-n 0)
  (straight-flush-n 0)
  (royal-flush-n 0))

(defparameter *rank-tabl* (make-hash-table))
(setf (gethash :royal-flush *rank-tabl*) 10)
(setf (gethash :straight-flush *rank-tabl*) 9)
(setf (gethash :four-kind   *rank-tabl*) 8)
(setf (gethash :full-house  *rank-tabl*) 7)
(setf (gethash :flush       *rank-tabl*) 6)
(setf (gethash :straight    *rank-tabl*) 5)
(setf (gethash :three-kind  *rank-tabl*) 4)
(setf (gethash :two-pairs   *rank-tabl*) 3)
(setf (gethash :one-pair    *rank-tabl*) 2)
(setf (gethash :high-card   *rank-tabl*) 1)

;; A hand consists of a list tuple (VALUE/SUIT strings)

(defun quicksort (lis access-fn)
  "Quicksort algorithm from: http://swisspig.net/r/post/blog-200603301157"
  (if (null lis) 
      nil
    (let* ((z (car lis)) 
	   (x (funcall access-fn z))
           (r (cdr lis))
           (fn (lambda (a) (< (funcall access-fn a) x))))
      (append (quicksort (remove-if-not fn r) access-fn)
              (list z)
              (quicksort (remove-if fn r) access-fn)))))

(defun split (string delim)
   "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them. E.g. #\Space"
   (loop for i = 0 then (1+ j)
          as j = (position delim string :start i)
          collect (subseq string i j)
          while j))

(defun hash-table->list (my-hash)
  "Convert the hashtable to a list ((key val-count) (key val-count))"
  (loop for key being the hash-keys of my-hash
         using (hash-value value)
         collect (list (card-value key) value)))

(defun card-value (c)
  "Look up the card value based on the index in the string"
  (1+ (position c *card-values* :test #'equal)))

(defun get-value (hand)
  "Get the value of a hand"
  (first hand))

(defun get-suit (hand)
  "Get the suit of a hand"
  (second hand))

(defun get-hands (str)
  "Convert the string hand data structure into the value/suit structure.
 E.g. convert 5H 5C 6S 7S KD into:
 ((5 H) (5 C)..."
  (loop for hand in (split str #\Space)
         collect (list (char hand 0) (char hand 1))))

(defun score-pair (score-tabl hand)
  "Simple data structure to keep track of pairs.
 List structure: (<count>, <suit>)"
  (let ((s (gethash (get-value hand) score-tabl))
	(suit (get-suit hand)))
    (if s
	(setf (gethash (get-value hand) score-tabl) 
	      (list (1+ (first s)) suit))
      (setf (gethash (get-value hand) score-tabl) 
	    (list 1 suit)))))

(defun find-pairs (hand-lst)
  (let ((score-tabl (make-hash-table :test 'equal)))
        (loop for hand in hand-lst
           do (score-pair score-tabl hand))
        score-tabl))

(defun find-pairs-tabl (hand-one access-fn)
  (let* ((pairs-one (find-pairs (get-hands hand-one)))
	 (pairs-lst-one (hash-table->list pairs-one)))
    (reverse (quicksort pairs-lst-one access-fn))))

(defun same-suit-p (hand)
  "Check the hand score to determine if the player has a flush"
  (every #'(lambda (x y)
	     (let ((a (second (second x)))
		   (b (second (second y))))
	       (eql a b)))
	 hand (cdr hand)))

(defun player-one-higher (player-lst)
  " Does player one have a higher set of cards"
  (let ((ctr 0)
	(res :tie))
    (loop for i in player-lst
	  for ply-one = (first i)
	  for ply-two = (second i)
	  for c = (incf ctr)
	  while (> ply-one ply-two)
	  finally (if (and (= ctr 5) (> ply-one ply-two)) 
		      (setf res :player-one) 
		    (setf res :player-two))) res))

(defun consective-p (hand)
  "Check if the value is consective or not.  The first
 value in the list is the high hand."
  (every #'(lambda (x y)
	     (let ((a (first x))
		   (b (first y)))
	       (and (= 1 (- a b)) (> a b))))
	 hand (cdr hand)))

(defun find-rank-consecutive (score val-score)
  "Use a more robust function for finding consective hands. 
 E.g. Royal Flush.
 The consective rank can only consist of:
 Straight, Straight Flush, or Royal Flush."
  (let ((high-card (first (first val-score)))
	(is-stra (same-suit-p score)))
    (if (consective-p val-score)
	(cond ((and is-stra
		    (= high-card 13)) :royal-flush)
	      (is-stra :straight-flush)
	      (t :straight))
      (when (same-suit-p score) :flush))))

(defun high-value (no-pairs-rank)
  (if no-pairs-rank
      no-pairs-rank
    :high-card))

(defun find-rank (score val-score)
  "Given the score data structure, actually find the poker name
 of the hand.  E.g. Four of a kind.  Return an atom type"
  (let* ((high-card (first score))
	 (count (first (second high-card)))
	 (second-card (second score))
	 (second-count (first (second second-card))))
    (cond ((= count 4) 
	   :four-kind)
	  ((and (= count 3) (= second-count 2)) 
	   :full-house)
	  ((and (> count 1)
		(same-suit-p score))
	   :flush)
	  ((= count 3) 
	   :three-kind)
	  ((and (= count 2) (= second-count 2)) 
	   :two-pairs)
	  ((= count 2) 
	   :one-pair)
	  ((= count 1)
	   (high-value 
	    (find-rank-consecutive score val-score))))))
	   
(defun normalize-score (score)
  (if score score 0))

(defun init-player-list (str-one str-two) 
  " Build a list of card values between two players (<card-one, card-two>, ...)"
  (let ((hands-one (get-hands str-one))
	(hands-two (get-hands str-two)))
    (player-val-list (reverse (quicksort hands-one 
					 #'(lambda (x) 
					     (card-value (first x)))))
		     (reverse (quicksort hands-two 
					 #'(lambda (x) 
					     (card-value (first x))))))))
(defun player-val-list (lst-one lst-two)
  " Build a list of card values between two players (<card-one, card-two>, ...)"
  (if (null lst-one) nil
    (let ((val-one (card-value (first (first lst-one))))
	  (val-two (card-value (first (first lst-two)))))
      (cons (list val-one val-two) 
	    (player-val-list (cdr lst-one) (cdr lst-two))))))

(defun find-winner (a-rank-one a-rank-two sum-one sum-two score-lst)
  (let ((rank-one (normalize-score (gethash a-rank-one *rank-tabl*)))
	(rank-two (normalize-score (gethash a-rank-two *rank-tabl*))))
    (cond ((or (zerop rank-one) (zerop rank-two)) 
	   (list :undetermined rank-one rank-two))
	  ((> rank-one rank-two) (list :player-one rank-one rank-two))
	  ((< rank-one rank-two) (list :player-two rank-one rank-two))
	  ((= rank-one rank-two)
	   (let ((res (list (player-one-higher (fifth score-lst))
			    rank-one rank-two)))
	     res)))))

(defun sum-hand-value (hand)
  (loop for i in hand
	sum (first i)))

(defun invoke-hand-stats (proc-fn player-one player-two 
				  score-lst stats)
  "Helper function for invoking the other stats helpers.
 first: sort by value
 third: sort by rank
 second: sort by value
 fourth: sort by rank"  
  (let* ((rank-one (find-rank (first score-lst) (third score-lst)))
	 (rank-two (find-rank (second score-lst) (fourth score-lst)))
	 (sum-hand-one (sum-hand-value (first score-lst)))
	 (sum-hand-two (sum-hand-value (second score-lst)))
	 (winner (find-winner rank-one rank-two sum-hand-one sum-hand-two score-lst)))
    (funcall proc-fn
	     player-one player-two 
	     stats
	     (second winner) 
	     (third winner) 
	     (first winner))))

(defun process-hand (line proc-fn player-one player-two stats)
  (let* ((hand line)
	 (hand-one (subseq hand 0 14))
	 (hand-two (subseq hand 15))
	 (player-lst (init-player-list hand-one hand-two))
	 ;; Build a list of lists, sorted score hands
	 ;; (<player one hand, sorted by count of card value>,
	 ;;  <player two hand, sorted by count of card value>,
	 ;;  <player one hand, sorted by just card value>,
	 ;;  <player one hand, sorted by just card value>)
         (score-lst (list
		     (find-pairs-tabl hand-one 
				      #'(lambda (x) (first (second x))))
		     (find-pairs-tabl hand-two 
				      #'(lambda (x) (first (second x))))
		     (find-pairs-tabl hand-one #'first)
		     (find-pairs-tabl hand-two #'first)
		     player-lst
		     )))
    (invoke-hand-stats proc-fn player-one player-two 
		       score-lst stats)))
		 
(defun determine-poker-winners (filename)
  "Example filename = ./poker.txt"
  (let ((player-one (make-player-stats))
        (player-two (make-player-stats))
	(stats (make-game-stats)))
    (with-open-file (stream filename)
                    (do ((line (read-line stream nil)
                               (read-line stream nil)))
                        ((null line))
                        (process-hand line 
				      #'process-line-stats
				      player-one 
				      player-two
				      stats
				      )))
    (print-stats player-one player-two stats)))
                         
(defun main ()
  (format t "INFO: Running Project Euler~%")
  ;; Driver test
  (determine-poker-winners "./poker.txt")
  (format t "~%INFO: Done~%"))
(main)

;; End of Script