;;
;; Berlin Brown
;; 7/10/2005
;;  
;; This module contains the java source code, header and footer
;;

(in-package :org.file.lispgen.lib
	    (:use :cl))

(defparameter *java-header* NIL)
(defparameter *java-footer* NIL)
(defparameter *secure-ftp* NIL)

(setf *java-header* "///
/// Code Generated, do not edit
///
import net.sf.jftp.net.ConnectionHandler;
import net.sf.jftp.net.ConnectionListener;
import net.sf.jftp.net.DataConnection;
import net.sf.jftp.net.FtpConnection;
import net.sf.jftp.net.SftpConnection;
import net.sf.jftp.net.BasicConnection;
import net.sf.jftp.util.Log;
import net.sf.jftp.util.Logger;
import net.sf.jftp.config.Settings;

import java.io.*;

/**
 * See FtpDownload.java for comments.
 */
public class FtpUpload implements Logger, ConnectionListener {

    private boolean isThere = false;
    
    private ConnectionHandler handler = new ConnectionHandler();
    
    public FtpUpload(String host, String dir, String file) {    

 	Log.setLogger(this);	
 	FtpConnection con = new FtpConnection(host);
	con.addConnectionListener(this);
	con.setConnectionHandler(handler);
        /// continuation below
")

;;
;; We will need to set the username and password
;; for the connection.  This function allows us
;; to insert those two values within our output string
;;
(defun set-connection-parms (username password)
  (let* ((with-username
	  (format NIL "con.login(\"~A\",\"~A\");" username password))
	 ;;; Continue with rest of java code
	 ;;; First-arg = username
	 ;;; Second-arg = continuation of the java code string
	 (result 
	  (format NIL "~A~A" with-username
"
	while(!isThere) {
	    try { Thread.sleep(40); }
	    catch(Exception ex) { ex.printStackTrace(); }
	}

	con.chdir(dir);
        con.removeFileOrDir(file);
	con.upload(file);
    }
")))
    ;;; Return result
    result))

(setf *java-footer* "/// Continue
    public static void main(String argv[]) {
 
	if (argv.length == 5) {
            /// last - 2 not used
	    FtpUpload f = new FtpUpload(argv[0], argv[1], argv[2]); 
	} else {	    
	    FtpUpload g = 
		new FtpUpload(\"upload.sourceforge.net\", \"/incoming\", \"test.txt\");
	}
    }

    public void updateRemoteDirectory(BasicConnection con) { 
 	System.out.println(\"New path is: \" + con.getPWD());
    }
    
    public void connectionInitialized(BasicConnection con) {    
  	isThere = true;
    }
    
    public void updateProgress(String file, String type, long bytes) {}    
    public void connectionFailed(BasicConnection con, String why) {System.out.println(\"Connection Failed\");}
    public void actionFinished(BasicConnection con) {}    
    public void debug(String msg) {System.out.println(msg);}    
    public void debugRaw(String msg) {System.out.print(msg);}    
    public void debug(String msg, Throwable throwable) {}    
    public void warn(String msg) {}    
    public void warn(String msg, Throwable throwable) {}    
    public void error(String msg) {}    
    public void error(String msg, Throwable throwable) {}
    public void info(String msg) {}  
    public void info(String msg, Throwable throwable) {}    
    public void fatal(String msg) {}
    public void fatal(String msg, Throwable throwable) {}

} /// end of the class //
")

;;;
;;; Define the simple secure FTP program
;;;
(setf *secure-ftp* "/// do not edit
import net.sf.jftp.net.ConnectionHandler;
import net.sf.jftp.net.ConnectionListener;
import net.sf.jftp.net.DataConnection;
import net.sf.jftp.net.FtpConnection;
import net.sf.jftp.net.SftpConnection;
import net.sf.jftp.net.BasicConnection;
import net.sf.jftp.util.Log;
import net.sf.jftp.util.Logger;
import net.sf.jftp.config.Settings;

public class SFtpUpload {
	public static void main(String[] args) {
		try {
			if (args.length == 5) {
				/// 0:host 1:dir 2:filename  3:user 4:pwd
				String host = args[0];
				String dir = args[1];
				String file = args[2];
				String user = args[3];
				String pwd = args[4];
				SftpConnection conn = new SftpConnection(host);
				conn.login(user, pwd);
				//if (conn.chdir(conn.getPWD()) || conn.chdir(\"/\")) {
				//	;
				//}
				conn.chdir(dir);
                                conn.removeFileOrDir(file);
				conn.upload(file);
				// set the username and password
				conn.disconnect();
			} else {
				throw new Exception(\"Invalid Args - SFtpUpload\");
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
} /// end of the class //
")

;;;
;;;  External Function - set the username and password
;;; Return the string containg the full java class.
;;;
(defun ftp-upload (username password)
  (format nil "~A~A~A"
	  *java-header*
	  (set-connection-parms username password)
	  *java-footer*
  ))

;;;
;;; End of Lisp File
;;;