;;################################################
#|
 For use with ABCL (known as Armed Bear Common Lisp, source created specifically for reddit)
 Author: Berlin Brown <berlin dot brown at gmail.com>
 Date: 4/15/2008
 
 "The opposite of a correct statement is a false statement." -- Niels Bohr
 
 *LICENSE* (new bsd license):
 -----------------------
 [2] http://www.opensource.org/licenses/bsd-license.php

 *Short Overview*
 ---------------
 Swing Client View (an swing example implemented for ABCL lisp)

 *Full Overview*
 ---------------
 I always have trouble finding the right tool to create simple, throw-away
 UIs.  I created this code to launch a set of perl test scripts.  I needed basic
 menu-item/button action handlers and a text-area to view and/or parse the incoming
 text that would have normally been piped to standard output.

 I ended up choosing common-lisp as opposed to python or ruby because the syntax is just as light, 
 powerful generaly-purpose language; it is perfectly suited for my task.  I commonly use emacs, 
 so I am already equipped with an editor.  I decided to use Swing and ABCL for the 
 UI toolkit, and for no real particular reason.  If I can create this UI using swing running on the JVM, 
 it will be 100 times easier using another toolkit.  Plus, swing and the jvm is reliatively
 portable.  In the future, I hope to convert this to a more portable MVC architecture.
 This Swing widget creation code could operate as the view for a larger system and then I could
 easily switch with another lisp gui toolkit library.

 This procedural oriented example could just as easily be converted to other 
 popular dynamic JVM languages Jython, JRuby, Clojure and (the static/strong-typed Scala).

 *Setup and Environment*
 -----------------------
 ABCL runs like any other java application.  Once, you compile the single jar and resource
 bundled  *.cls lisp binary files, a script is available that you can run the ABCL main class:
 
 org.armedbear.lisp.Main

 You will need a working java runtime and JDK (preferablly Sun's jre/jdk 1.5 or greater), 
 a common lisp implementation (e.g. CMUCL, SBCL, or CLISP).
 Download the latest ABCL from sourceforge [1] http://armedbear.org/abcl.html 
 Any of the common lisp implemenations will work.  In the past, I have compiled 
 ABCL on win32 cygwin with clisp and Ubuntu Linux with SBCL.  I didn't 
 experience any issues.  You may have some trouble configuring the customization
 file to point to your JDK.  I had to modify the script and just removed the operating
 system detect code.

 I added this to the buttom of the ABCL customizations.lisp file 
 to override any pre-existing definitions:
 (setq *jdk*           "C:\\Program Files\\Java\\jdk1.5.0_11\\")
 (setq *java-compiler* "javac")
 
 Follow the steps in the README file:
 Start your common lisp implemenation and get to the lisp REPL prompt:

 Load build-abcl.lisp:

    (load "build-abcl.lisp")

 Then do:

    (build-abcl:build-abcl :clean t :full t)

 *Tested with JDK/JRE version*:
 ---------------------------------
 Java version "1.5.0_11"
 Java(TM) 2 Runtime Environment, Standard Edition (build 1.5.0_11-b03)
 Java HotSpot(TM) Client VM (build 1.5.0_11-b03, mixed mode)

--------------------------------------
 ** ABCL Lisp Helpers **
--------------------------------------
 Example static method call "System.getenv('KEY')
 (jstatic "getenv" (jclass "java.lang.System") "KEY")

 Fields:
 (jfield-raw "java.lang.Boolean" "TRUE")

--------------------------------------
 ** Useful Java Swing Constructors **
 Also see: [3] http://java.sun.com/javase/6/docs/api/javax/swing/JPanel.html
--------------------------------------

 JTextField() / JTextField(String text, int columns)
 JPanel()
 JScrollPane() / JScrollPane(Component view) / JScrollPane(Component view, int vsbPolicy, int hsbPolicy)

 *Notes*
 [4] ABCL is not forgiving with passing the wrong type to a java method.  Make
 sure the right type is passed to the right argument.  ABCL returns generic errors:

 Debugger invoked on condition of type JAVA-EXCEPTION:
 #<JAVA-EXCEPTION {4654F6}>

|#
;;################################################

(defpackage :swing-view
  (:use :common-lisp :java))
(in-package :swing-view)

;;----------------------------
;; ** Java Class String Constant Definitions **
;;----------------------------
(defconstant *default-path* "file://./latin_text.txt")
(defconstant j-string "java.lang.String")
(defconstant j-component "java.awt.Component")
(defconstant j-container "java.awt.Container")
(defconstant j-layout-manager "java.awt.LayoutManager")
(defconstant j-borderlayout "java.awt.BorderLayout")
(defconstant j-actionevent "java.awt.event.ActionEvent")
(defconstant j-abstractaction "javax.swing.AbstractAction")
(defconstant j-boxlayout "javax.swing.BoxLayout")
(defconstant j-jbutton "javax.swing.JButton")
(defconstant j-jframe "javax.swing.JFrame")
(defconstant j-jmenu "javax.swing.JMenu")
(defconstant j-jmenubar "javax.swing.JMenuBar")
(defconstant j-jmenuitem "javax.swing.JMenuItem")
(defconstant j-jpanel "javax.swing.JPanel")
(defconstant j-jscrollpane "javax.swing.JScrollPane")
(defconstant j-jtextarea "javax.swing.JTextArea")
(defconstant j-jtextfield "javax.swing.JTextField")
(defconstant j-scrollpaneconstants "javax.swing.ScrollPaneConstants")
(defconstant j-uimanager "javax.swing.UIManager")

(defparameter *scroll-h-always* (jfield-raw j-scrollpaneconstants 
											"HORIZONTAL_SCROLLBAR_ALWAYS"))
(defparameter *scroll-v-always* (jfield-raw j-scrollpaneconstants
											"VERTICAL_SCROLLBAR_ALWAYS"))

(defparameter *bl-north* (jfield j-borderlayout "NORTH")
  "Definition for swing constants")
(defparameter *bl-east* (jfield j-borderlayout "EAST")
  "Definition for swing constants")
(defparameter *bl-center* (jfield j-borderlayout "CENTER")
  "Definition for swing constants")
(defparameter *bl-west* (jfield j-borderlayout "WEST")
  "Definition for swing constants")
(defparameter *bl-south* (jfield j-borderlayout "SOUTH")
  "Definition for swing constants")
(defparameter *box-y-axis* (jfield j-boxlayout "Y_AXIS")
  "Definition for swing constants")
(defparameter *box-x-axis* (jfield j-boxlayout "X_AXIS")
  "Definition for swing constants")

;;----------------------------
;; ** Java method definitions **
;;----------------------------
(defparameter *method-set-layout* 
  (jmethod j-container "setLayout" j-layout-manager))
(defparameter *method-jmenu-add* (jmethod j-jmenu "add" j-jmenuitem))
(defparameter *method-jpanel-add* (jmethod j-jpanel "add" j-component))
(defparameter *method-container-add* (jmethod j-container "add" j-component))
(defparameter *method-container-add-2* (jmethod j-container "add" 
												j-component "java.lang.Object"))

;; Java constructor definitions
(defparameter *new-jmenu-item* (jconstructor j-jmenuitem j-string))
(defparameter *new-scroll-pane* 
  (jconstructor j-jscrollpane j-component "int" "int"))
(defparameter *new-scroll-pane-2*  (jconstructor j-jscrollpane "int" "int"))
(defparameter *new-textarea* (jconstructor j-jtextarea "int" "int"))

;;----------------------------
;; Function implementations
;;----------------------------
(defun j-true () (make-immediate-object t :boolean))
(defun j-false () (make-immediate-object nil :boolean))

(defun to-java-string (s)
  (jnew (jconstructor "java.lang.String" "java.lang.String") s))

(defun init-swing (jframe)
  "Define the frame properties. Add the button panel and menu"
  (progn (setNativeLookUI)
		 (initTextAreaLayout (get-content-pane jframe))))

(defun get-content-pane (jframe)
  "Translated to natural language: Using the instance of
 the JFrame object called jframe, invoke the getContentPane method
 and return an instance of the Container class"
  (jcall (jmethod j-jframe "getContentPane") jframe))

(defun new-borderlayout ()
  "Create an instance of the border layout class"
  (jnew (jconstructor j-borderlayout)))

(defun new-box-layout (panel axis)
  (jnew (jconstructor j-boxlayout j-container "int") panel axis))

(defun path-textfield ()
  (jnew (jconstructor j-jtextfield j-string "int")
		*default-path* 40))
	
(defun initTextAreaLayout (content-pane)
  (let* ((text-field (path-textfield))
		 (contentArea (jnew *new-textarea* 25 60 ))
		 (topPanel (jnew (jconstructor j-jpanel)))
		 (buttonPanel (jnew (jconstructor j-jpanel)))
		 (scrollPane (jnew *new-scroll-pane* contentArea 
						   *scroll-v-always* *scroll-h-always*)))
	(format t "INFO: content-pane obj:= [ ~a | ~a ]~%" content-pane topPanel)
	(jcall *method-set-layout* topPanel
		   (new-box-layout topPanel *box-y-axis*))
	;; Add TO the top panel; in java this will look like: topPanel.add(pathField)
	(jcall *method-jpanel-add* topPanel text-field)
	(jcall *method-set-layout* content-pane (new-borderlayout))
	(jcall *method-container-add-2* content-pane topPanel *bl-north*)
	(jcall *method-container-add-2* content-pane scrollPane *bl-center*)))

(defun setNativeLookUI ()
  "Use the operating system native UI look and feel, do not use the Swing oriented look"
  (jstatic "setLookAndFeel"
		   (jclass j-uimanager)
				   "com.sun.java.swing.plaf.windows.WindowsLookAndFeel"))
(defun createReaderFrame ()
  " Create the simple reader frame "
  (let ((simpleFrame (jnew (jconstructor j-jframe))))
	(init-swing simpleFrame)
	(jcall (jmethod j-jframe "pack") simpleFrame)
	(jcall (jmethod j-jframe "setVisible" "boolean")
		   simpleFrame (j-true))))

(defun lisp-main ()
  "Main entry point, create the jframe and attach the widget components then
 start the jframe thread"
  (format t "INFO: creating panel objects~%")
  #+abcl
  (createReaderFrame))

(lisp-main)

;;################################################
;; End of Script
;;################################################