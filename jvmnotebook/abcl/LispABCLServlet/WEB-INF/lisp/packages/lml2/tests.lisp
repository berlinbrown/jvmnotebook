;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.lisp
;;;; Purpose:       tests file
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id: tests.lisp,v 1.3 2003/07/15 19:25:28 kevin Exp $
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:cl)
(defpackage #:lml-tests
  (:use #:lml2 #:cl #:rtest))
(in-package #:lml-tests)

(rem-all-tests)

(deftest lml.0
  (with-output-to-string (s)
    (let ((*html-stream* s))
      (html (:div))))
  "<div></div>")

(deftest lml.1
  (with-output-to-string (s)
    (let ((*html-stream* s))
      (html ((:span class 'foo) "Foo Bar"))))
  "<span class=\"foo\">Foo Bar</span>")

(deftest lml.2
  (with-output-to-string (s)
    (let ((*html-stream* s))
      (html ((:table class "foo" :style "width:80%")
	     "Foo" " Bar" " test"))))
  "<table class=\"foo\" style=\"width:80%\">Foo Bar test</table>")

(deftest lml.3
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (a 5.5d0))
      (html (:p (:princ a)))))
  "<p>5.5d0</p>")

(deftest lml.4
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (a 0.75))
      (html ((:img :src "http://localhost/test.png" :width a)))))
  "<img src=\"http://localhost/test.png\" width=\"0.75\" />")

(deftest lml.5
  (with-output-to-string (s)
    (let ((*html-stream* s))
      (html
       (:div "Start"
	     (:p "Testing")))))
  "<div>Start<p>Testing</p></div>")

(deftest lml.6
  (with-output-to-string (s)
    (let ((*html-stream* s))
      (html
       ((:div :style "font-weight:bold")
	"Start"
	((:p class 'a_class) "Testing")))))
  "<div style=\"font-weight:bold\">Start<p class=\"a_class\">Testing</p></div>")

(deftest lml.7
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (class "aclass"))
      (html
       ((:div :optional (:class class))
	"bod"))))
  "<div class=\"aclass\">bod</div>")

(deftest lml.8
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (class nil))
      (html
       ((:div :optional (:class class))
	"bod"))))
  "<div>bod</div>")

(deftest lml.9
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (do-class t)
	  (class "aclass"))
      (html
       ((:div :when (:class do-class class))
	"bod"))))
  "<div class=\"aclass\">bod</div>")

(deftest lml.10
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (do-class nil)
	  (class "aclass"))
      (html
       ((:div :when (:class do-class class))
	"bod"))))
  "<div>bod</div>")


(deftest lml.11
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (v 10))
      (html
       ((:div :fformat (:onclick "a&b('~A')" v))))))
  "<div onclick=\"a&b('10')\"></div>")

(deftest lml.12
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (v 10))
      (html
       ((:div :format (:onclick "a&b('~A')" v))))))
  "<div onclick=\"a&amp;b('10')\"></div>")

(deftest lml.13
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (selector t)
	  (v 10))
      (html
       ((:div :if (:width selector 1 2))))))
  "<div width=\"1\"></div>")

(deftest lml.14
  (with-output-to-string (s)
    (let ((*html-stream* s)
	  (selector nil)
	  (v 10))
      (html
       ((:div :if (:width selector 1 2))))))
  "<div width=\"2\"></div>")
