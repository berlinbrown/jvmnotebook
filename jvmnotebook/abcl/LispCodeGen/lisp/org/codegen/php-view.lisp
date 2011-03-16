;;;
;;; Berlin Brown
;;;
;;; View - What will be output to the system
;;;
(in-package :org.file.lispgen.lib 
	    (:use :cl))

(defconstant *index-page* "testall.php")
(defconstant *form-page* "formpage.php")

;;
;; Once the model and controller are in sync,
;; generate the view, the application
;;
(defun generate-view ()
  (format nil "
<html>
 <head>
  <title>~A</title>
  <style type=\"text/css\">
   ~A
  </style>
 </head>
 <body>empty</body>
</html>
"
  "The Title"
  (generate-css)
  "phpinfo();"))

;;
;; Concatenate the list of transmitted links and build
;; a portion of the web page with 
;;
;; Through the mapcar function, convert the page string to an
;; anchor tag
;;
(defun convert-elem-link () 
  (mapcar #'(lambda (elem)
	      (format nil "echo '<p><a href=\"~A\">~A</a>';~%" elem elem)) 
	  (view-list-files))
  )

;;
;; Convert the list object to a large concatenated string
;;
(defun generate-index-links ()  
  (concatenate 'string 
   "<?php " (format nil "~%~%") 
   (apply #'concatenate 'string (convert-elem-link))
   "echo '<br>'; echo date('l dS of F Y h:i:s A');" " ?>"))

(defun generate-listpage (title)
    (format nil "
<html>
 <head>
  <title>~A</title>
  <style type=\"text/css\">
   ~A
  </style>
 </head>
 <body>
  <div class='greenframe'>
   ~A
  </div>
  <p>
  <i>(last edited: ~A)</i> 
 </body>
</html>
" 
  title
  (generate-css)
  (generate-index-links) (print-date nil)))

;;
;; The following functions provide utilities for 
;; generating the system objects.
;;

;;
;; Generate a list of the files to add to the deployment
;; queue.  These files are specific to the database
;;
;; This public function is used by 'main' @see 'lispgen'
;;
(defun basic-list-files ()
  (list *index-page*))

(defun view-list-files ()
  (append 
   (basic-list-files)
   (db-list-files)))
  
(defun write-view ()
  ;;; Write the main form page
  (let ((list-src (generate-listpage "List All Page")))
    (write-file-str *index-page*
		    (format nil "~A" list-src)))
  (db-write-view)
  )

;;; End of File ;;