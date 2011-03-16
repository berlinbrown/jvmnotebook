(print "lisp dbgr is starting..")
(terpri)

(defvar *swank-loaded* nil)
(defvar *swank-server-started* nil)

(print "loading swank..")
(terpri)

(unless *swank-loaded*
	(load "h:\\lisp\\slime-new2\\slime\\swank-loader.lisp")
	(setq *swank-loaded* t))

(print "starting swank server..")
(terpri)

(unless *swank-server-started*
	(swank:create-server :port 4005 :dont-close t)
	(setq *swank-server-started* t))

(setq swank:*global-debugger* t)
(setq swank:*globally-redirect-io* t)
