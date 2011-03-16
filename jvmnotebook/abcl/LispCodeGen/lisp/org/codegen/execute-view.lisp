;;;
;;; Berlin Brown
;;;
;;; $Id$
;;;
;;; 7/21/2005
;;;
;;; This module is used for the view aspect of the execute pages
;;;
;;; The insert page will only need to know type of table to insert
;;; into.
;;;

(in-package :org.file.lispgen.lib 
	    (:use :cl))

(defconstant *exec-view-page* "akitainsert.php")
(defconstant *exec-action-page* "akitaaction.php")

(defconstant *execute-footer* 
" 
 </body>
</html>
")

(defun execute-view-header (title)
  (format nil "
<html>
 <head>
  <title>~A</title>
  <style type=\"text/css\">
   ~A
  </style>
 </head>
 <body>
" "Insert Title" (generate-css)))

;;----------------------------------------------------------
;;
;; Process Execute-operations
;;
;;----------------------------------------------------------
(defun generate-exec-actions (action field-list value-list table-name)
  (concatenate-view
   (list
    "<?php" (string #\Newline)
    (format nil
"
 /// Generate - INSERT oriented action
 $field_names = '~A';
 $table_names = ' ~A';

 $valmodelname = $_POST['modelname'];
 $valdescr = $_POST['description'];
 $valbody = $_POST['mainbody'];
 
 if (!$valmodelname) {
  $valmodelname = '';
 }
 if (!$valdescr) {
  $valdescr = '';
 }
 if (!$valbody) {
  $valbody = '';
 }
 
 $transfvals = \"~A\"; 
 $mnquery = \" insert into \".$table_names.\"(\".$field_names.\") \";
 $vals = \" values(\".$transfvals.\")\";
 
 $finalsql = $mnquery.$vals;
 echo('<p>'.$mnquery);
 
 $result = mysql_query($finalsql);

 if ($result == 0) {
  echo('<b>'.mysql_error().'</b>');
 } else {
  echo('<br><b>Connection Passed</b><p>');  
 } // end

" (transform-field-list field-list) table-name 
   (transform-field-list value-list))
    (string #\Newline) "?>"
    )))

;;
;; Generate the PHP database connection sequence
;;
(defun connect-code (hostname db-name user-name password)
  (let ((connect-vars (format nil " ~A~% ~A~% ~A~% ~A~%"
			      (format nil "$host = '~A';" hostname)
			      (format nil "$user = '~A';" user-name)
			      (format nil "$password = '~A';" password)
			      (format nil "$database = '~A';" db-name)
			      ))
	(connect-stub
"
 $conn_res = mysql_connect($host, $user, $password);
 mysql_select_db($database);
 echo '<br>'.$conn_res.'<br>';
 // Collect the incoming messages
 $msg = $_GET['connectmsg'];
 if ($msg) {
  echo('Valid Incoming Message'.$msg);
 } else {
  echo('Invalid Incoming Message');
 } // end // 
"))
    (concatenate 'string
		 "<?php" (string #\Newline)
		 connect-vars
		 connect-stub
		 (string #\Newline) "?>"
		 )))


(defun exec-footer ()
  (format nil
"
 <p>
 <a href='akitacategory.php'>Categories</a>
 <p>
 <i>(edited: ~A)</i>
"
  (print-date nil)))

(defun generate-exec-section ()
  (format nil
"
 ~A
"
  (connect-code 
   *db-hostname* *db-name* *db-user* *db-password*)
  ))

;;----------------------------------------------------------
;;
;; generate-form-section
;;
;;----------------------------------------------------------
(defun generate-form-section ()
"
 <div class='greenframe'>
  <form action='akitaaction.php' method='post'> 
   <!-- Name Object -->
   <p> 
   <b>Name</b><br>
   <input type='text' name='modelname' value='Name' maxlength='255'>
   <!-- Description Model Object -->
   <p>
   <b>Description</b><br>
   <textarea name='description' rows='4' cols='50'></textarea>
   <!-- Body text -->
   <p>
   <b>Body</b>
   <br>
   <textarea name='mainbody' rows='6' cols='50'></textarea>
    
   <!-- Description Object -->
    <p>
    <input type='text' name='miscmodel' maxlength='255'>
    <p>
    <input type='submit' value='Add Record'>     
  </form>
  <p>
  <a href='akitacategory.php'>Article Categories</a>
 </div>
"
  )

;;----------------------------------------------------------
;;
;; Utility Functions
;;
;;----------------------------------------------------------
(defun concatenate-view (the-list)
  (apply #'concatenate 'string the-list))

(defun tile-view-exec ()
  (concatenate-view 
   (list    
    (execute-view-header "View Create Title")
    (generate-form-section)
    *execute-footer*
    )
  ))

(defun tile-confirm-exec ()
  (concatenate-view
   (list
    (execute-view-header "Execute Action")
    "<div class='greenframe'>"
    (generate-exec-section)
    (generate-exec-actions "INSERT"
			   (list "name" "title_html" "description" 
				 "date_added" "author_id" "status")
			   (list "'\".$valmodelname.\"'" 
				 "'\".$valmodelname.\"'" 
				 "'\".$valdescr.\"'"
				 "NOW()" "1" "'visible'")
			   "akita_category")
    (exec-footer)
    "</div>"
    *execute-footer*
    )
  ))
      
;;----------------------------------------------------------
;;
;; Write the code generated modules to the filesystem
;;
;;----------------------------------------------------------
(defun write-action-file ()
  (let ((list-src
	 (tile-confirm-exec)))
    (write-file-str *exec-action-page*
		    (format nil "~A" list-src))))

(defun write-execute-file ()
  (let ((list-src 
	 (tile-view-exec)))
    (write-file-str *exec-view-page*
		    (format nil "~A" list-src))))

(defun write-exec-all ()
  (progn
    (write-execute-file)
    (write-action-file)
    ))

;;; End of File ;;
