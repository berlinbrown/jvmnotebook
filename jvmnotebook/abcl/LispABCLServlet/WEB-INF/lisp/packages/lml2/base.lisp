;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base.lisp
;;;; Purpose:       Lisp Markup Language functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: base.lisp,v 1.7 2003/07/12 17:54:05 kevin Exp $
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)


(defun lml-format (str &rest args)
  (when (streamp *html-stream*)
    (if args
	(apply #'format *html-stream* str args)
	(write-string str *html-stream*))))

(defun lml-princ (s)
  (princ s *html-stream*))

(defun lml-print (s)
  (format *html-stream* "~A~%" s))

(defun lml-write-char (char)
  (write-char char *html-stream*))

(defun lml-write-string (str)
  (write-string str *html-stream*))

(defun lml-print-date (date)
  (lml-write-string (date-string date)))

(defun dtd-prologue (&optional (format :xhtml11))
  (case format
    ((:xhtml :xhtml11 :xhtml10-strict :xhtml10-transitional :xhtml10-frameset :xml)
     (lml-write-string +xml-prologue-string+)
     (lml-write-char #\newline)))
  (case format
    ((:xhtml11 :xhtml)
     (lml-write-string +xhtml11-dtd-string+))
    (:xhtml10-strict
     (lml-write-string +xhtml10-strict-dtd-string+))
    (:xhtml10-transitional
     (lml-write-string +xhtml10-transitional-dtd-string+))
    (:xhtml10-frameset
     (lml-write-string +xhtml10-frameset-dtd-string+))
    (:html
     (lml-write-string +html4-dtd-string+)))
  (lml-write-char #\newline))


(defmacro html-file-page ((out-file &key (format :xhtml11))
			  &body body)
  `(with-open-file (*html-stream*
		    (lml-file-name ,out-file :output)
		    :direction :output
		    :if-exists :supersede)
     (dtd-prologue ,format)
     (html
      ((:html :xmlns "http://www.w3.org/1999/xhtml")
       ,@body))))
		     

(defmacro alink (url desc)
  `(html
    ((:a :href ,url) ,desc)))

(defmacro alink-c (class url desc)
  `(html
    ((:a :class ,class :href ,url) ,desc)))
