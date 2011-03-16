;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Purpose:       General purpose utilities
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  June 2002
;;;;
;;;; $Id: utils.lisp,v 1.5 2003/07/15 21:49:36 kevin Exp $
;;;;
;;;; This file, part of LML2, is copyrighted and open-source software.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defun print-file-contents (file &optional (strm *standard-output*))
  "Opens a reads a file. Returns the contents as a single string"
  (when (probe-file file)
    (let ((eof (cons 'eof nil)))
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil eof) 
                   (read-line in nil eof)))
            ((eq line eof))
          (write-string line strm)
          (write-char #\newline strm))))))

(defun date-string (ut)
  (check-type ut integer)
  (multiple-value-bind (sec min hr day mon year dow daylight-p zone)
      (decode-universal-time ut)
    (declare (ignore daylight-p zone))
    (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~d ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~d ~2,'0d:~2,'0d:~2,'0d" 
	    dow day (1- mon) year hr min sec)))

(defun lml-quit (&optional (code 0))
  "Function to exit the Lisp implementation."
    #+allegro (excl:exit code)
    #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
    #+(or cmu scl) (ext:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+gcl (lisp:bye code)
    #+lispworks (lw:quit :status code)
    #+lucid (lcl:quit code)
    #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0) (t 1)))
    #+openmcl (ccl:quit code)
    #+(and mcl (not openmcl)) (declare (ignore code))
    #+(and mcl (not openmcl)) (ccl:quit)
    #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl mcl)
    (error 'not-implemented :proc (list 'quit code)))


(defun lml-cwd ()
  "Returns the current working directory. Based on CLOCC's DEFAULT-DIRECTORY function."
  #+allegro (excl:current-directory)
  #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
  #+(or cmu scl) (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #+sbcl (sb-unix:posix-getcwd/)
  #+mcl (ccl:mac-default-directory)
  #-(or allegro clisp cmu scl sbcl cormanlisp lispworks lucid mcl) (truename "."))


#+ignore
(defun fformat (&rest args)
  (declare (dynamic-extent args))
  (apply (if (find-package 'kmrcl)
	     (symbol-function (intern (symbol-name #:fformat)
				      (symbol-name #:kmrcl)))
	     #'format)
	 args))

(defmacro fformat (stream control-string &rest args)
  (if stream
      `(funcall (formatter ,control-string) ,stream ,@args)
      `(format nil ,control-string ,@args)))
  
