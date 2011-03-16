##
## Berlin Brown
## berlin.brown at gmail.com
##
## Lisp Generator - README.txt
##
## (PHP, HTML, Java Bean, Generic Code Generation System)
##
## 7/8/2005
##

License - N/A

-------------------------------------------------
Overview:
-------------------------------------------------

This application is a powerful approach to code generation
for server-side development as well as other forms of programming.
Code generation is an approach for introducing an intermediary between
the process of creating the software and the intended requirement on
what the system should do.  This can greatly reduce defects and increase
your return on investment(ROI).

At present, the Lisp modules need some refactoring, but already you can
see the benefit of creating the majority of the logic in actions in a
Lisp environment over working soley with inflexible tools.

Java(Sun's Java Development Kit) is used for the deployment process.
You will need to download a valid runtime environment in order to 
transfer your generated code remotely.

You will notice, a generic java application gets generated through
the lisp system.  This program's task is to deploy the other
generated code through a FTP and SFTP java library.  If the java runtime
is installed properly, this aspect of the process will work accordingly.

Note: This application is targeted at the GNU CLisp lisp implementation.

-------------------------------------------------
Similar Technology:
-------------------------------------------------

FireStorm from Codefutures:
"The award-winning FireStorm/DAO makes software developers more 
productive by automatically generating the Java code for accessing databases."

http://www.codefutures.com/

-------------------------------------------------
Example of PHP Code Generated Application
-------------------------------------------------
The default PHP application should include the ability to
perform all CRUD(Create Read Update Delete) operations on 
any object in the system.

-------------------------------------------------
Glossary, Terms and Application Specific Vocabulary:
-------------------------------------------------

* Java: Java is an object-oriented programming language as well as a cross-platform
	set of tools used for software development created by Sun Microsystems in 1995.
 
* Lisp: Common Lisp is a dialect of lisp, the functional programming language.
	* "Supports programming techniques such as imperative, 
		functional and object-oriented programming".
    * "Is dynamically typed, but with optional type declarations 
    	that can improve efficiency or safety".
    * "Is extensible through standard features such as Lisp macros 
    	(compile-time code rearrangement accomplished by the program itself) and reader macros (extension of syntax to give special meaning to characters reserved for users for this purpose).

* PHP: PHP is an open-source script language used
	for developing server-side applications and dynamic web content.
    	
* FTP: FTP stands for file transfer protocol and is used to transfer files from
	one system to a remote server.  Most FTP servers bind to port 21.  This application
	uses FTP in order to deploy generated modules.

* SFTP: SFTP stands for secure FTP.  SFTP useds SSH for the file transfer.  Most
	SFTP servers are found on port 22.  This application also uses SFTP to deploy
	the code generated modules.
    
(Some of these definitions were provided by the wikipedia system)

-------------------------------------------------

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
 
 *Adding new lisp source files*
 If you plan to add a source module, add the name of the file to the
 *.asd file.  You may also consider adding the public functions to the
 package.lisp file.

 Example database connection:
 
 	DB: blogsys
 	user: blogger
 	pwd: blogger
 	
 	Syntax for MySQL connection:
 	 mysql -u blogger -pblogger blogsys

-------------------------------------------------
j-ftp - Common classes and methods.
-------------------------------------------------

BasicConnection <<interface>>:
 - removeFileOrDir(String file)
 - upload
 - chdir
  					 					
-------------------------------------------------
Contact:
-------------------------------------------------
 
 Berlin Brown
 berlin.brown at gmail.com
 
