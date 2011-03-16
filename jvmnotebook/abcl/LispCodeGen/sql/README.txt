##
## Berlin Brown
##
## Lisp Generator
##
## 7/8/2005
##
-------------------------------------------------
README:
-------------------------------------------------

This application is targeted at the GNU CLisp lisp implementation.

-------------------------------------------------
Lisp Notes:

 *Only For Developers*
-------------------------------------------------

 *lisp\asdf.lisp - contains the ASDF lisp package system
 
 *lisp\lispgen.asd - contains the ASDF definition for this 
 					project.  Edit this file when adding
 					new modules.
 					
 					If you create a new sub-directory, you will
 					need to add an additional .ASD file.
 					
 					Example ASD file:
 					
 					(asdf:defsystem :widget-examples
  						:name "widget-examples"
  						:depends-on (:widget-src)
					 	:components
  						((:file "widget-examples") 
   						(:file "simple-wtk-win" :depends-on ("widget-examples"))
   							(:file "orkit-red" :depends-on ("widget-examples"))
   							(:file "drawing-example" :depends-on ("widget-examples"))
						))
						
					And to load:
					
					(asdf:operate 'asdf:load-op :widget-src :force nil)
					(asdf:operate 'asdf:load-op :widget-examples :force nil)
 					
 *lisp\testall.lisp - If you need to add a package sub-directory,
 					you will add the relative path here.
 						
 					To add multiple directories:
 					
					(push (merge-pathnames
					 (make-pathname :directory '(:relative "examples" "ftp"))
	 					*widget-toolkit-root*)
						asdf:*central-registry*))
 					
 *lisp\package.lisp - Define the project and main package.  If you would
 					like to add exports, add them here.
 					
 					
 lisp\lisp-unit.lisp - Unit testing. 				
 
 Note: Example SFTP Connection:  70.85.100.4
 
 Example remote directory:
  "/home/berlinbrown/public_html/apps"

 Example database connection:
 
 	DB: blogsys
 	user: blogger
 	pwd: blogger
 	
 	Syntax for MySQL connection:
 	 mysql -u blogger -pblogger blogsys
 					 					
-------------------------------------------------
Contact:
-------------------------------------------------
 
 Berlin Brown
 berlin.brown at gmail.com
 
