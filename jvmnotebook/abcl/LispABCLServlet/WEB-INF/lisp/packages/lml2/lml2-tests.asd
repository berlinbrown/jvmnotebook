;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lml2-tests.asd
;;;; Purpose:       ASDF system definitionf for lml2 testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: lml2-tests.asd,v 1.1 2003/06/20 04:12:29 kevin Exp $
;;;; *************************************************************************

(defpackage #:lml2-tests-system
  (:use #:asdf #:cl))
(in-package #:lml2-tests-system)

(defsystem lml2-tests
    :depends-on (:rt :lml2)
    :components
    ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'lml2-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

