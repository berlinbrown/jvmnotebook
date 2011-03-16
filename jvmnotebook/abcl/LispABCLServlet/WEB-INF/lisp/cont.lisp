;;
;; cont.lisp
;;

(in-package :servlet1)

(defvar *continuations* (make-hash-table))
(defparameter *cc-id* nil)

(let ((i 0))
  (defun next-continuation-id ()
    (incf i)))

(defun set-continuation (k)
  (let ((id (next-continuation-id)))
    (setf (gethash id *continuations*)
	  (cons (get-universal-time)
		k))
    id))

(defun get-continuation (id)
  (prog1 
      (cdr (gethash id *continuations*))
    ))

(defun purge-continuations (&optional (outdate-time 300))
  (loop with newhash = (make-hash-table)
	with boundtime = (- (get-universal-time) outdate-time)
	for id being each hash-key of *continuations* using (hash-value cnt)
	for time = (car cnt)
	for k = (cdr cnt)
	when (> time boundtime)
	do (setf (gethash id newhash)(cons time k))
	finally (setq *continuations* newhash)))

;; End of File
