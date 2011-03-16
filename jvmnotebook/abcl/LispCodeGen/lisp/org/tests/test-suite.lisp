;;;
;;; Berlin Brown
;;;
;;; $Id$
;;;
;;; Example tests: assert-equal, assert-false(true), 
;;;             assert-null
;;;  This module contains the core set of test cases
;;;
(in-package :org.file.lispgen.lib 
	    (:use :cl))

(define-test example-test
  (assert-equal 2 (+ 1 1)))

(define-test safe-string-test
  (assert-equal "\"dogs cats\"" (safe-namestring "dogs cats")))

;;; End of File ;;