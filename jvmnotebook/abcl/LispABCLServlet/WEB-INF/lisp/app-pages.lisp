;;
;; app-pages.lisp
;;

(in-package :servlet1)

(defmacro make-page (title body)
  `(let/cc k
    (let ((*cc-id* (set-continuation k)))
      (dtd-prologue :xhtml10-strict)
      (html ((:html :xmlns "http://www.w3.org/1999/xhtml")
	     (:head (:title ,title)
		    ((:link :rel "stylesheet" :href "default.css"
			    :type "text/css")))
	     ,body)))))

(defmacro std-form (&body body)
  `((:form :action "LspExec" :method "get")
    ,@body))

(defmacro form-submit (value &optional name)
  `((:input :type "submit" :value ,value ,@(if name (list :name name)))))

(defmacro form-hidden (name value)
  `((:input :type "hidden" :name ,name :value ,value)))

(defmacro verb-form (verb &body body)
  `(std-form (form-hidden "verb" ,verb)
    (form-hidden "cont" *cc-id*)
    ,@body))

(defun form-select (name options)
  (html ((:select :name name)
	 (loop for (value text) in options
	       do (html ((:option :value value)
			 (:princ text)))))))

(defun/cc gen-front-page ()
  (make-page "demo abcl servlet"
	     (:body (:p "this is a demo servlet written in Armed Bear Common Lisp 
with logic based on continuations")
		    (verb-form :demo1 (form-submit "demo1"))
		    (verb-form :loop-demo (form-submit "loop demo")))))

(defun/cc gen-num-page (str)
  (make-page "loop demo"
	     (:body (:p (:princ str))
		    (verb-form :next (form-submit "next"))
		    (verb-form :exit (form-submit "exit"))
		    )))

(defparameter *users* '((1 "user1")
			(2 "user2")
			(3 "user3")))

(defun/cc gen-login-page ()
  (make-page "demo: login"
	     (:body (:h1 "login")
		    (:p "select user:")
		    (verb-form :login
			       (form-select :user-id *users*)
			       ((:input :type :password :name "password"))
			       (form-submit "login"))
		    (verb-form :exit (form-submit "exit")))))

(defun/cc gen-user-menu-page (username)
  (make-page "user menu"
	     (:body (:h1 "menu for " (:princ username))
		    (verb-form :add-numbers (form-submit "add numbers"))
		    (verb-form :mul-numbers (form-submit "mul numbers"))
		    (verb-form :exit (form-submit "exit")))))

(defun/cc gen-enter-number-page ()
  (make-page "enter number"
	     (:body (verb-form :number 
			       ((:input :name :number))
			       (form-submit "enter number"))
		    (verb-form :number
			       (form-hidden :number 2)
			       (form-submit "2"))
		    (verb-form :number
			       (form-hidden :number 5)
			       (form-submit "5")))))

(defun/cc gen-show-result-page (str)
  (make-page "result"
	     (:body (:p (:princ str))
		    (verb-form :ok (form-submit "ok")))))


;; End of File
		    
			       
		    

