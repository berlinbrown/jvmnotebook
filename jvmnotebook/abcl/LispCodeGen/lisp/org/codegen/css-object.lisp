;;;
;;; Berlin Brown
;;; 7/14/2005
;;; 
;;; css-object.lisp
;;;
(in-package :org.file.lispgen.lib 
	    (:use :cl))

(defun generate-css ()
  (format nil "
                BODY
		{      
		  margin: 0px 0px 6px 0px;
		  FONT-FAMILY: Arial, Verdana, Sans;
		  BACKGROUND-COLOR: #ffffff;
		  color: #777777;
		}
                IMG
		{
		 BORDER-RIGHT: 0px;
		 BORDER-TOP: 0px;
		 BORDER-LEFT: 0px;
	 	 BORDER-BOTTOM: 0px
		}

                A:link
		{   
		 COLOR: #1A3391;
		 FONT-SIZE: 14pt;			
		}	
		A:visited
		{
		 COLOR: #1A3391;
		 FONT-SIZE: 14pt;
		}
		A:hover
		{
		 COLOR: #1A3391;
		 CURSOR: wait;     
		 FONT-SIZE: 14pt;
		 background-color: #E9E9E9;			
		}
                /** green frame */
                .greenframe {
                 margin: 4px 4px 4px 4px;
                 padding: 4px 4px 4px 4px;
                 border: 3px solid #81E81D;
                }
"
  ))

;;; End of File ;;