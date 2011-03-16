;;;----------------------------------------------------------
;;;
;;; Berlin Brown
;;;
;;; $Id$
;;;
;;; berlin.brown at gmail.com
;;; 6/28/2005
;;;
;;;  This module contains calls launch the system, including
;;; witing and compiling the java application.
;;;
;;; java build scripts based on 'ABCL' startup
;;;
;;; note: terpri = lisp carriage-return
;;;----------------------------------------------------------
(in-package :org.file.lispgen.lib 
	    (:use :cl))

;;----------------------------------------------------------
;; Load the Config
;;----------------------------------------------------------
;; 
;; Check if the java home directory and executables are valid
;;
(defun set-java-command ()
  (format t "~%JDK Home: ~A~%" *jdk-home*)
  (setf *java-runtime* (probe-file (merge-pathnames "bin\\java.exe"
						    *jdk-home*)))  
  (setf *java-compiler* (probe-file (merge-pathnames "bin\\javac.exe"
						    *jdk-home*)))

  ;; Check if the executables are valid
  (unless *java-runtime*
    (error "Can't find Java runtime executable.~%### Make sure to visit the lisp-config.lisp file"))
  (unless *java-compiler*
    (error "Can't find Java compiler executable.~%### Make sure to visit the lisp-config.lisp file"))
  (format t "~%Setting Java Runtime: ~A~%" *java-runtime*))

;;
;; Launch the java runtime and write the lisp stream batch program
;; This function will create a batch program through the stream utilities, 
;; and then launch the batch script.
;; The java compile and run are included within this script.
;;
;; @param host			FTP host server to connect to
;; @param init-dir		Initial remote directory
;; @param filename-upload	Path of the file to upload
;; @param java-app-name		Name of the class with the main entry point
;;
(defun stream-command-compile (host init-dir filename-upload java-app-name)
  (let* ((java-namestring (safe-namestring *java-runtime*))
	 (javac-namestring (safe-namestring *java-compiler*))	 
	 status)
    (with-open-file (stream
		     (merge-pathnames "stream-run.bat" "./")
		     :direction :output
		     :if-exists :supersede)
		    (princ javac-namestring stream)
		    (write-string " -classpath ./;../lib/jftp.jar; " stream)
		    (princ "*.java" stream)
		    (write-char #\space stream)		    
		    ;; Write args with: (write-string "EXAMPLE ARG" stream)
		    ;; Write lisp carriage-return
		    (terpri stream)

		    ;; Run the application
		    (princ java-namestring stream)
		    (write-string " -classpath ./;../lib/jftp.jar; " stream)
		    (write-string 
		     (format nil " ~A \"~A\" \"~A\" \"~A\" ~A ~A" 
			     java-app-name host 
			     init-dir filename-upload *ftp-username* *ftp-password*) stream)
		    (terpri stream)
		    (close stream))		    
    (setf status
	  (run-shell-command "stream-run.bat"))
    ))

;;
;; Generate the java source code and compile
;;
;; Use the :sftp-enable t - to enable the SFTP system
;; @param host			FTP host server to connect to
;; @param init-dir		Initial remote directory
;; @param filename-upload	Path of the file to upload
;; @param java-app-name		Name of the class with the main entry point
;;
(defun generate-and-compile (host init-dir filename &key sftp-enable)
  (let* ((program-compile-run (if sftp-enable "SFtpUpload" "FtpUpload"))
	 (java-src (format nil "~A.java" program-compile-run)))
      ;;; Write the secure source or the default FTP source
      (write-file-str java-src
		      (if sftp-enable *secure-ftp*
			  (ftp-upload *ftp-username* *ftp-password*)))
      (stream-command-compile host init-dir filename program-compile-run)))

;;----------------------------------------------------------
;; Main - Entry point of the system
;; In order to enable or disable SFTP support, use the
;; sftp-enable flag.
;;----------------------------------------------------------
(defun main ()
  "Starting executing, entry point"
  (format t "~%.Lisp PHP Code Generator~%")
  (set-java-command)

  (write-view)
  ;; Send the modules to the remote server 
  ;; example "ohio" "samb/apps" "testall.php"
  (dolist (file-snd (view-list-files))
    (format t " ** .processing - ~A~%" file-snd)
    (generate-and-compile
     *ftp-host* *ftp-remote-dir* file-snd
     :sftp-enable *sftp-enable*))
  (format t "~%.done"))

;;; Run Main
(progn (terpri) 
       (format t "Current Date and Time: ~A" (print-date nil))
       ;;(run-all-tests :org.file.lispgen.lib) 
       (time (main))
       (terpri))


;;
;; End of file
;;
