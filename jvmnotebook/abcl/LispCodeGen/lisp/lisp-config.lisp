;;;
;;; Berlin Brown
;;;
;;; $Id$
;;;
;;; lisp-config.lisp
;;;
;;; This module contains configuration variables used
;;; throughout the application
;;;

(in-package :org.file.lispgen.lib
	    (:use :cl))

;; Here are the standard set of config variables
(defparameter *java-runtime* nil)
(defparameter *java-compiler* nil)
(defparameter *jdk-home* nil)
(defparameter *sftp-enable* nil)

(defparameter *ftp-host* "ohio")
(defparameter *ftp-remote-dir* "samb/apps")
(defparameter *ftp-username* "USERNAME")
(defparameter *ftp-password* "PASSWORD")

;; Database Variables: Example DB Name = blogsys, User = blogger
(defconstant *db-hostname* "localhost")
(defconstant *db-name* "blogsys")
(defconstant *db-user* "blogger")
(defconstant *db-password* "blogger")

;; Enable a secure ftp connection or not
;; normally a SFTP server will bind to port 22
;; Set to 't' to enable.
(setf *sftp-enable* nil)

;; java-runtime is set in lispgen.lisp
(setf *java-runtime* nil)
(setf *java-compiler* nil)

;;
;; Edit the JDK-HOME directory value, make sure to add the
;; suffix, \\
;;
(setf *jdk-home* "C:\\Program Files\\Java\\jdk1.5.0\\")

;;; End of file ;;

