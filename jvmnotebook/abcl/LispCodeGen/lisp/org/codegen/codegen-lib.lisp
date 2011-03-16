;;;
;;; Berlin Brown
;;;
;;; Misc lisp utilities
;;;
(in-package :org.file.lispgen.lib
	    (:use :cl))

;;
;; transform-field-list: Transform a list of strings into
;;  concatenated string.
;;
;; @param the-list    A list of field names
;; @return            A String with a fieldname list
;;
;; Example: (transform-field-list (list "a" "b"))
;;
(defun transform-field-list (the-list) 
  (funcall (lambda (strings) (format nil "~{~A~^,~}" strings)) the-list))

;;
;; Print the Date
;; 
;;"~D.~2,'0D.~2,'0D ~2,'0D:~2,'0D:~2,'0D"
(defun print-date (stream &optional (ut (get-universal-time)))
  (multiple-value-bind (ss mm hh d m y dy dls tz)
      (decode-universal-time ut)
    (format stream "~D.~2,'0D.~2,'0D ~2,'0D:~2,'0D:~2,'0D" y m d hh mm ss)))

;;
;; Write a string to file
;;
(defun write-file-str (file-name full-string)
  (format t "# Writing file ~A~%" file-name)
  (let ((out-file
         (open file-name :direction :output)))
    (when out-file      
      (format out-file "~A" full-string)
      (close out-file)))
  (format t "# End ~%"))

;;
;;   Windows will need a executable strings that are wrapped 
;; double around quotes.
;;
;; @param pathname  perform windows oriented string cleanups
;;     			this includes wrapping double quotes around strings
;;
(defun safe-namestring (pathname)
  (let ((string (namestring pathname)))
    (when (position #\space string)
      (setf string (concatenate 'string "\"" string "\"")))
    string))

;;
;; Wrapper function for launching a shell command
;; You can add the '#+clisp for gnu clisp only support
;;
;; @param command 	absolute path of the command to run
;; @param :directory	directory to begin in
;; @param :output	redirect output, default is stdout
;;
;; @return 		return the status of the command launch
;;
(defun run-shell-command (command &key directory (output *standard-output*))
  (declare (ignore output))
  (let (status old-directory)
    (when directory
      (setf old-directory (ext:cd))
      (ext:cd directory))
    (unwind-protect
        (setf status (ext:run-shell-command command))
      (when old-directory
        (ext:cd old-directory)))
    (cond ((numberp status)
           status)
          ((eq status t)
           0)
          (t
           -1))))

;;; End of File ;;