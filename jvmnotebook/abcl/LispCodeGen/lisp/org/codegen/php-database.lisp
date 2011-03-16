;;;
;;; Berlin Brown
;;; 
;;; $Id
;;;
;;; Generate stubs for database manipulation
;;;
;;; Create a CRUD(Create Read Update Delete) 
;;;  oriented web PHP application
;;;
(in-package :org.file.lispgen.lib (:use :cl))

(defconstant *category-page* "akitacategory.php")
(defconstant *dbview-page* "akitaview.php")

;;
;; Generate a list of the files to add to the deployment
;; queue.  These files are specific to the database
;;
;; @see php-view#view-list-files()
;;
(defun db-list-files ()
  (list *category-page* *dbview-page*
	*exec-view-page* *exec-action-page*))

;;
;; Generate the section of code for performing the query 
;; against the database
;;
;; Example SQL string sections:
;;
;; Field Names: 'a.name, a.description, a.date_added, b.name, b.obj_id, b.category_id'
;; Table Names: 'akita_category as a, akita_pages as b'
;; Where: ' where a.obj_id = b.category_id'
;;
(defun generate-select-stub ( field-list table-str where )
  (format nil
" 
 $pageidvar = $_GET['pageid'];

 $field_names = '~A';
 $table_names = ' ~A';
 $where_stmt = ' ~A';
 $query = 'select '.$field_names.' from '.$table_names.' '.$where_stmt;
 $result = mysql_query($query);

 if ($result == 0) {
  echo('<b>'.mysql_error().'</b>');
 } else {

  echo('<br><b>Connection Passed</b><p>');  
  echo('<table><tr>');
  for ($i = 0; $i < mysql_num_fields($result); $i++) { 
    echo('<td><b>'.mysql_field_name($result, $i).'</b></td>');    
  } // end - for //

  echo('</tr><tr><td colspan=\"6\">');
  for ($i = 0; $i < mysql_num_rows($result); $i++) {
   $row = mysql_fetch_row($result);
   echo($row[2].':<b>'.$row[0].'</b> - <a href=\"akitaview.php?pageid='.$row[4].'\">'. $row[1] .'</a> ('.$row[3].' '.$row[4].' ## '.$row[5].')<p>');
  }
  echo('</td></tr></table>');
 } // end //
"
  (transform-field-list field-list)
  table-str
  where))

;;
;; Generate the SQL connection string
;;
(defun generate-db-stub (hostname db-name user-name password 
				  field-list table-str where-stmt)
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
		 (generate-select-stub field-list table-str where-stmt)
		 (string #\Newline) "?>"
		 )))

(defun db-view-page (title db-stub)
    (format nil
"<html>
 <head>
  <title>~A</title>
  <style type=\"text/css\">
   ~A
  </style>
 </head>
 <body>
    <div class='greenframe'>
    ~A    
    <p>
    <a href='akitainsert.php'>New Category</a>
    <p>
    <i>edited: ~A</i>
    </div>    
 </body>
</html>
"
  title
  (generate-css)
  db-stub
  (print-date nil)
  ))

;;
;; This function performs writing the PHP
;; source to the filesystem.
;;
(defun db-write-view ()
  (let ((list-src
	 (db-view-page "DB View Page"		       
		       (generate-db-stub
			*db-hostname* *db-name* *db-user* *db-password*
			(list " distinct a.name"
			      "a.description" 
			      "a.date_added"
			      "b.name"
			      "b.obj_id"
			      "b.category_id")
			" akita_category as a, akita_pages as b"
			" LEFT JOIN akita_pages ON a.obj_id = b.category_id")
			;;" where a.obj_id = b.category_id")
		       )))
    (write-file-str *category-page*
     (format nil "~A" list-src)))
  
    (let ((list-src 
	   (db-view-page "View Results Pge"       
			 (generate-db-stub 
			  *db-hostname* *db-name* *db-user* *db-password*
			  (list "a.name"
				"a.description" 
				"a.date_added"
				"b.name"
				"b.obj_id"
				"b.category_id")
			  " akita_category as a, akita_pages as b"
			  " where a.obj_id = b.category_id AND b.obj_id='.$pageidvar.'")
			 )))
      (write-file-str *dbview-page*
		      (format nil "~A" list-src)))
    ;; @see execute-view.lisp - write the CREATE pages
    (write-exec-all)
  )

;;; End of File ;;