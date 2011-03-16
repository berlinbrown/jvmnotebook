;;**********************************************
;; Author: Berlin Brown <berlin dot brown @ gmail.com>
;; Overview: Euler Example Number 54 (neophyte version)
;; Target Environment: Common Lisp -> Clisp/Sbcl 
;; Date: 5/19/2008
;; --------------
;; Helper module for collecting game statistics.
;;
;; See:
;; http://projecteuler.net/index.php?section=problems&id=54
;;
;; References:
;; [1] http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
;; [2] http://www.unixuser.org/~euske/doc/cl/loop.html
;; [4] http://cl-cookbook.sourceforge.net/strings.html
;;**********************************************

(defun get-player-stats (player score)
  (cond ((= score 1)
	 (incf (player-stats-high-card-n player)))
	((= score 2)
	 (incf (player-stats-one-pair-n player)))
	((= score 3)
	 (incf (player-stats-two-pairs-n player)))
	((= score 4)
	 (incf (player-stats-three-kind-n player)))
	((= score 5)
	 (incf (player-stats-straight-n player)))
	((= score 6)
	 (incf (player-stats-flush-n player)))
	((= score 7)
	 (incf (player-stats-full-house player)))
	((= score 8)
	 (incf (player-stats-four-kind-n player)))
	((= score 9)
	 (incf (player-stats-straight-flush-n player)))
	((= score 10)
	 (incf (player-stats-royal-flush-n player)))))
			
(defun process-line-stats (player-one player-two stats 
				      hand-one hand-two player-win)
  (incf (game-stats-total-games stats))
  (get-player-stats player-one hand-one)
  (get-player-stats player-two hand-two)
  (when (or hand-one hand-two)
    (incf (game-stats-count-valid stats)))
  (cond ((eql player-win :player-one)
	 (incf (game-stats-player-one stats)))
	((eql player-win :player-two)
	 (incf (game-stats-player-two stats)))
	((eql player-win :tie)
	 (incf (game-stats-tie stats)))
	((eql player-win :undetermined)
	 (incf (game-stats-undetermined stats))))
  (list hand-one hand-two))

(defun print-stats (player-one player-two stats)
  (format t "---------------~%")
  (format t "** Game Statistics~%")
  (format t "** Number of games: ~a~%"
	  (game-stats-total-games stats))
  (format t "** Number of hands: ~a~%"
	  (* 2 (game-stats-total-games stats)))
  (format t "** Valid hands: ~a~%" (game-stats-count-valid stats))
  (format t "** Player One Wins: ~a~%" (game-stats-player-one stats))
  (format t "** Player Two Wins: ~a~%" (game-stats-player-two stats))
  (format t "** Ties: ~a~%" (game-stats-tie stats))
  (format t "** Undetermined: ~a~%" (game-stats-undetermined stats))
  (format t "** ~a~%" player-one)
  (format t "** ~a~%" player-two)
  (format t "---------------"))

;; End of Script