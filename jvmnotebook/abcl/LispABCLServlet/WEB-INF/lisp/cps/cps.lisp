;;;; -*- lisp -*-

;;;_* Description

;;  This file contains a CPS transformer. 

;; PASS1: a) rename local variables and functions to allow the order
;;        of certain operations to be changed without changing the
;;        semantics of the code. b) expand all macros and
;;        symbolmacros.

;;        This pass is handled by the cps-rename function.

;; PASS2: Convert the code into CPS.

;;        This pass is handled by the cps-transform function.

;; For every special operator we define 3 functions: a transformer, a renamer
;; (which must also do macroexpansion if neccessary) and a requries test. The
;; requires test tells us if a particular form needs to be CPS'd or not
;; (basically it just check whether that form contains a call to call/cc or
;; not. Most of these functions are defined with the macrso defcps-transformer,
;; defcps-rename and defcps-requires. The exceptions are the handres for atoms
;; and general function/macro application which need more information then the
;; macros provide.

;;;_* Entry Point

(defmacro with-call/cc (&body body)
  "Execute BODY with quasi continutations.

BODY may not refer to macrolets and symbol-macrolets defined
outside of BODY.
"
  (case (length body)
    (0 NIL)
    (1 (to-cps (first body)))
    (t (to-cps `(progn ,@body)))))

(defun toplevel-k (value)
  (throw 'done value))

(defvar *env* :unitialized)

(defun to-cps (form &optional (k '#'toplevel-k))
  `(drive-cps ,(%to-cps form k)))

(defun %to-cps (form k)
  (let ((*env* nil))
    (setf form (cps-rename form)))
  (let ((*env* nil))
    (setf form (cps-transform form k)))
  form)

(defun drive-cps (cps-lambda)
  (catch 'done
    (loop for f = cps-lambda then (funcall f))))

(defvar *call/cc-returns* nil
  "Set to T if CALL/CC should call it's continuation, otherwise
  the lambda passed to CALL/CC must call the continuation
  explicitly.")

(defmacro let/cc (k &body body)
  `(call/cc (lambda (,k)
              (flet ((,k (v)
                       (funcall ,k v)))
                ,@body))))

(defparameter *non-cps-calls* (list)
  "When transforming code to CPS this variable is used to handle
  the names of all function which are called in \"direct\"
  stile.")

(defun add-non-cc (symbol)
  (unless (eq (symbol-package symbol) #.(find-package "COMMON-LISP"))
    (pushnew symbol *non-cps-calls*)))

(defun assert-non-cc (symbols while-defining)
  "Assert that the functions named by SYMBOLS are not CC
  functions. We also record that this assertion was made while
  compiling the function/method while-defining."
  (dolist (sym symbols)
    (if (get sym 'call/cc)
	(error "Attempting to register ~S as a non CC function.
However it was previously registered as a CC function in ~S."
	       sym (get sym 'call/cc))
	(setf (get sym 'non-call/cc) while-defining))))

(defun assert-cc (name &optional (while-defining :toplevel))
  (if (get name 'non-call/cc)
      (error "Attempting to register ~S as a CC function.
However it was previously registered an a non CC function in ~S."
	     name (get name 'non-call/cc))
      (setf (get name 'call/cc) while-defining)))

(defun fmakun-cc (name)
  (remf (symbol-plist name) 'call/cc)
  (remf (symbol-plist name) 'non-call/cc)
  name)

(defun gen-cc-definiton (what &key name block-name args body)
  (assert-cc name)
  (multiple-value-bind (declares doc-strings body)
      (split-body-into-parts body)
    (let* ((k (gensym "K-"))
	   (*non-cps-calls* (list))
	   (code (to-cps `(block ,block-name ,@body) k)))
      (assert-non-cc *non-cps-calls* name)
      `(progn
	 (,what ,name ,args
	     ,@doc-strings
	     ,@declares
	     ;; capture the value of *k* at the time we are called
	     (let ((,k (or *k* #'toplevel-k)))
	       (declare (ignorable ,k))
	       ,code))
	 (assert-non-cc ',*non-cps-calls* ',name)
	 (assert-cc ',name)
	 ',name))))

(defmacro defun/cc (name args &body body)
  (gen-cc-definiton 'defun 
		    :name name
		    :block-name name
		    :args args
		    :body body))

(defmacro defmethod/cc (name lambda-list &body body)
  "Define a method which can be used \"normally\" with in
  CALL/CC."
  (when (symbolp lambda-list)
    (error "DEFMETHOD/CC can't be used in non-standard method combination."))
  (gen-cc-definiton 'defmethod
		    :name name
		    :block-name (if (listp name)
				    (second name)
				    name)
		    :args lambda-list
		    :body body))

(defmacro defgeneric/cc (&rest stuff)
  `(defgeneric ,@stuff))

;;;_* Code

;;;_ * Helpers

(defvar *k* nil
  "The holder for inter-call continuations. See the application
  cps-transformer for details.")

(defun register (type name &optional (data (gensym)))
  (unless (member type '(:flet :let :macrolet :symbol-macrolet :block :tag))
    (warn "Unknow TYPE: ~S." type))
  (setf *env* (cons (list type name data) *env*))
  data)

(defun lookup (type name)
  (loop 
     for (itype iname data) in *env*
     when (and (eql itype type)
	       (eql name iname))
     do (return data)
     finally (return nil)))

(defmacro k (value)
  ``(lambda ()
      (funcall ,k ,,value)))

(defun lookup-cps-handler (form)
  "Simple function which tells us what cps-handler should deal
  with FORM. Signals an error if we don't have a handler for
  FORM."
  (if (atom form)
      'atom
      (case (car form)
        ((block declare flet function go if labels let let* macrolet progn quote return-from setq
          symbol-macrolet tagbody call/cc the unwind-protect)
         (car form))
        ((catch eval-when load-time-value locally multiple-value-call multiple-value-prog1 progv throw)
         (error "~S not allowed in CPS code (in form ~S), please refactor." (car form) form))
        (t 'application))))

;;;_ * Handlers

;;;_  * Code for defining handlers

(defvar *cps-handlers* (make-hash-table :test 'eq))

(defun find-cps-handler (form)
  (gethash (lookup-cps-handler form) *cps-handlers*))

(defclass cps-handler ()
  ((transformer :accessor handler.transformer)
   (requires-checker :accessor handler.requires-checker)
   (rename :accessor handler.rename)))

(defun ensure-exists-handler (name)
  (unless (gethash name *cps-handlers*)
    (setf (gethash name *cps-handlers*)
          (make-instance 'cps-handler))))

(defun cps-transform (form k)
  (if (cps-requires form)
      (funcall (handler.transformer (find-cps-handler form)) form k)
      (k form)))

(defmacro defcps-transformer (name args &body body)
  `(progn
     (ensure-exists-handler ',name)
     (setf (handler.transformer (gethash ',name *cps-handlers*))
           (lambda (form k)
             (declare (ignorable k))
             (let ((*env* *env*))
               (destructuring-bind ,args (cdr form)
                 ,@body))))
     ',name))

(defun cps-requires (form)
  (funcall (handler.requires-checker (find-cps-handler form)) form))

(defmacro defcps-requires (name args &body body)
  `(progn 
     (ensure-exists-handler ',name)
     (setf (handler.requires-checker (gethash ',name *cps-handlers*))
           (lambda (form)
             (let ((*env* *env*))
               (destructuring-bind ,args (cdr form)
                 ,@body))))
     ',name))

(defun cps-rename (form)
  (funcall (handler.rename (find-cps-handler form)) form))

(defmacro defcps-rename (name args &body body)
  `(progn 
     (ensure-exists-handler ',name)
     (setf (handler.rename (gethash ',name *cps-handlers*))
           (lambda (form)
             (let ((*env* *env*))
               (destructuring-bind ,args (cdr form)
                 ,@body))))
     ',name))

;;;_  * Actual handlers

;;;_   * Helpers for dealing with declarations and bodys

(defun cps-requires-body (body)
  (multiple-value-bind (declares actual-body)
      (split-into-body-and-declares body)
    (declare (ignore declares))
    (some #'cps-requires actual-body)))

(defun cps-transform-body (body k)
  "Unlike most cps transformers this returns a list."
  (multiple-value-bind (declares actual-body)
      (split-into-body-and-declares body)
    (if declares
        `((declare ,@declares) ,(cps-transform `(progn ,@actual-body) k))
        (list (cps-transform `(progn ,@actual-body) k)))))

;;;_   * atoms

;;; atom - do this "manually" since it isn't applied to lists

(ensure-exists-handler 'atom)

(setf (handler.rename (gethash 'atom *cps-handlers*))
      (lambda (atom)
        (acond
         ((lookup :let atom) it)
         ((lookup :symbol-macrolet atom) (cps-rename it))
         (t atom))))

(setf (handler.transformer (gethash 'atom *cps-handlers*))
      (lambda (atom k)
        (k atom)))

(setf (handler.requires-checker (gethash 'atom *cps-handlers*))
      (lambda (atom)
        (declare (ignore atom))
        nil))

;;;_   * Application

;;; application - do this "manually" since we need the name of the
;;; form

(ensure-exists-handler 'application)

(setf (handler.rename (gethash 'application *cps-handlers*))
      (lambda (form)
        (destructuring-bind (op &rest args)
            form
          (acond
           ((lookup :flet op) `(,it ,@(mapcar #'cps-rename args)))
           ((lookup :macrolet op) (cps-rename (funcall it (cdr form))))
	   #+clisp
	   ((and (consp op) (eql 'setf (car op)))
	    `(funcall ',(get (second op) 'system::setf-function)
		      ,@(mapcar #'cps-rename args))) 
           ((macro-function op)
            (multiple-value-bind (expansion expanded)
                (macroexpand-1 form nil)
              (if expanded
                  (cps-rename expansion)
                  `(,op ,@(mapcar #'cps-rename args)))))
           (t `(,op ,@(mapcar #'cps-rename args)))))))

(setf (handler.transformer (gethash 'application *cps-handlers*))
      (lambda (form k)
        (destructuring-bind (op &rest args)
            form
          (if (and args (some #'cps-requires args))
              ;; need to cps transforme the args for this function
              ;; call.
              (loop
                 with renames = (mapcar (lambda (v)
                                          (if (constantp v) 
                                              v
                                              (gensym)))
                                        (reverse args))
                 with code = (cond
                               ((lookup :flet op) `(,op ,k ,@(reverse renames)))
                               ((get op 'call/cc) `(lambda ()
                                                     (throw 'done
                                                       (let ((*k* ,k))
                                                         (,op ,@(reverse renames))))))
                               (t
				(add-non-cc op)
				(k `(,op ,@(reverse renames)))))
                 for a in (reverse args)
                 for a* in renames
                 unless (constantp a)
                   do (setf code (cps-transform a `(lambda (,a*) ,code)))
                 finally (return code))
              (cond
               ((lookup :flet op) `(,op ,k ,@args))
               ((get op 'call/cc) `(lambda ()
                                     (throw 'done
                                       (let ((*k* ,k))
                                         (,op ,@args)))))
               (t
		(add-non-cc op)
		(k form)))))))

(setf (handler.requires-checker (gethash 'application *cps-handlers*))
      (lambda (form)
        ;; if the car is a lambda renaming may be required
	(cond
	  ((and (consp (car form))
		(eql 'lambda (caar form)))
	   (or (cps-requires (car form))
	       (some #'cps-requires (cdr form))))
	  #+clisp
	  ((and (consp (car form))
		(eql 'setf (caar form)))
	   (some #'cps-requires (cdr form)))
	  ((lookup :flet (car form)) t)
	  ((get (car form) 'call/cc) t)
	  (t
	   (add-non-cc (car form))
	   (some #'cps-requires (cdr form))))))

;;;_   * BLOCK

(defcps-transformer block (name &rest body)
  (register :block name k)
  (cps-transform `(progn ,@body) k))

(defcps-requires block (name &rest body)
  (declare (ignore name))
  (cps-requires `(progn ,@body)))

(defcps-rename block (name &rest body)
  (register :block name)
  `(block ,(lookup :block name) ,@(mapcar #'cps-rename body)))

;;;_   * RETURN-FROM

(defcps-transformer return-from (block-name &optional value)
  (cps-transform value (lookup :block block-name)))

(defcps-requires return-from (block-name &optional value)
  (declare (ignore block-name value))
  ;; since return-from is a control flow operator this should always be cps'd
  t)

(defcps-rename return-from (block-name &optional value)
  (aif (lookup :block block-name)
       `(return-from ,it ,(cps-rename value))
       (error "Can't return outside of cps block: ~S." block-name)))

;;;_   * CALL/CC

(defun make-call/cc-k (k)
  (lambda (value)
    (catch 'done
      (loop
         for f = (funcall k value)
         then (funcall f)))))

(defcps-transformer call/cc (func)
  (if (and (listp func)
           (eql 'lambda (car func)))
      (destructuring-bind ((kk) &body body)
          (cdr func)
        (cps-transform `(let ((,kk (make-call/cc-k ,k)))
                          ,@body)
                         (if *call/cc-returns*
                             k
                             '#'toplevel-k)))
        (cps-transform `(funcall ,func (make-call/cc-k ,k))
                       (if *call/cc-returns*
                           k
                           '#'toplevel-k))))

(defcps-requires call/cc (func)
  (declare (ignore func))
  t)

(defcps-rename call/cc (func)
  (if (and (listp func)
           (eql 'lambda (car func)))
      `(call/cc (function
                 (lambda ,(second func)
                  ,@(mapcar #'cps-rename (cddr func)))))
      form))

;;;_   * FUNCTION

(defcps-transformer function (func)
  (k `(function ,func)))

(defcps-requires function (func)
  (etypecase func
    (symbol nil)
    (cons
     (when (some #'cps-requires (cddr func))
       (error "Attempting to create a LAMBDA which contains a call to call/cc (~S)." func))
     nil)))

(defcps-rename function (func)
  (if (and (consp func)
           (eql 'lambda (car func)))
      `(function (lambda ,(second func)
                   ,@(multiple-value-bind (declares body)
                         (split-into-body-and-declares (cddr func))
                       (if declares
                           `((declare ,@declares) ,@(mapcar #'cps-rename body))
                           (mapcar #'cps-rename body)))))
      (aif (lookup :flet func)
           `(function ,it)
           `(function ,func))))

;;;_   * TAGBODY

(defcps-transformer tagbody (&rest forms)
  (let (blocks current-block-name current-block-body)
    (dolist (f (reverse forms))
      (if (symbolp f)
          (progn
            (push `(,f ()
                       ,(cps-transform `(progn ,@current-block-body)
                                       (with-unique-names (v)
                                         `(lambda (,v)
                                            (declare (ignore ,v))
                                            ,(if current-block-name
                                                 ;; since we're traversing in reverse order
                                                 ;; current-block-name being true implies that
                                                 ;; we're NOT the last block. so we need to
                                                 ;; continue with the next block.
                                                 `(,current-block-name)
                                                 ;; last block, all done
                                                 `(funcall ,k nil))))))
                  blocks)
            (setf current-block-body nil
                  current-block-name f))
          (push f current-block-body)))
    `(labels ,blocks
       (,(first forms)))))

(defcps-requires tagbody (&rest forms)
  (some #'cps-requires forms))

(defcps-rename tagbody (&rest forms)
  ;; register, and rename, all the tags in this tagbody
  (setf forms (mapcar (lambda (f)
                        (if (symbolp f)
                            (register :tag f (gensym (strcat "TAGBODY-TAG-" f "-")))
                            f))
                      forms))
  ;; cps-rename all the forms, leave the tags alone.
  (setf forms (mapcar (lambda (f)
                        (if (symbolp f)
                            f
                            (cps-rename f)))
                      forms))
  ;; insert a new tag as the first element, unless one already exists
  (unless (symbolp (first forms))
    (push (gensym "TAGBODY-START-") forms))
  `(tagbody ,@forms))

;;;_   * GO

(defcps-transformer go (tag-name)
  `(,tag-name))

(defcps-requires go (tag-name)
  (declare (ignore tag-name))
  t)

(defcps-rename go (tag-name)
  (aif (lookup :tag tag-name)
       `(go ,it)
       (error "Go outside tag body: ~S." tag-name)))

;;;_   * IF

(defcps-transformer if (test then &optional else)
  ;; try and produce a minimal transformation of the if
  (if (and (not (cps-requires test))
           (not (cps-requires then))
           (not (cps-requires else)))
      (k `(if ,test ,then ,else))
      (let ((then-branch (if (cps-requires then)
                             (cps-transform then k)
                             (k then)))
            (else-branch (if (cps-requires else)
                             (cps-transform else k)
                             (k else))))
        (if (cps-requires test)
            (cps-transform test (with-unique-names (v)
                                  `(lambda (,v)
                                     (if ,v
                                         ,then-branch
                                         ,else-branch))))
            `(if ,test
                 ,then-branch
                 ,else-branch)))))

(defcps-requires if (test then &optional else)
  (or (cps-requires test)
      (cps-requires then)
      (cps-requires else)))

(defcps-rename if (test then &optional else)
  `(if ,(cps-rename test)
       ,(cps-rename then)
       ,(cps-rename else)))

;;;_   * DECLARE

(defcps-rename declare (&rest clauses)
  `(declare
    ,@(mapcar (lambda (clause)
                (case (car clause)
                  (type
                   (list* 'type (second clause)
                          (mapcar #'cps-rename (cddr clause))))
                  ((ignore ignorable dynamic-extent)
                   (list* (first clause)
                          (mapcar #'cps-rename (cdr clause))))
                  ((special) (error "SPECIAL declares not allowed in CALL/CC code."))
                  ((inline notinline optimize) clause)
                  (t
                   ;; assume it's a type-spec
                   (list* (car clause)
                          (mapcar #'cps-rename (cdr clause))))))
              clauses)))

(defcps-requires declare (&rest clauses)
  (declare (ignore clauses))
  nil)

(defcps-transformer declare (&rest clauses)
  `(declare ,@clauses))

;;;_   * FLET

(defcps-rename flet (binds &body body)
  (let ((orig-env *env*))
    `(flet ,(mapcar (lambda (flet)
                      `(,(register :flet (first flet)) ,(second flet)
                         ,@(let ((*env* orig-env))
                             (mapcar #'cps-rename (cddr flet)))))
                    binds)
       ,@(mapcar #'cps-rename  body))))

(defcps-transformer flet (binds &body body)
  `(flet ,(mapcar (lambda (flet)
                    `(,(first flet) (,(register :flet (first flet)) ,@(second flet))
		       (declare (ignorable ,(lookup :flet (first flet))))
                       ,@(cps-transform-body (cddr flet) (lookup :flet (first flet)))))
                  binds)
     ,@(cps-transform-body body k)))

(defcps-requires flet (binds &body body)
  (or (some #'cps-requires-body (mapcar #'cddr binds))
      (cps-requires-body body)))

;;;_   * LABELS

(defcps-transformer labels (binds &body body)
  (dolist* (bind binds)
    (register :flet (first bind)))
  `(labels ,(mapcar (lambda (flet)
                       `(,(first flet) (,(lookup :flet (first flet)) ,@(second flet))
                          ,@(cps-transform-body (cddr flet) (lookup :flet (first flet)))))
                     binds)
     ,@(cps-transform-body body k)))

(defcps-rename labels (binds &body body)
  (dolist* ((name args &rest body) binds)
    (declare (ignore args body))
    (register :flet name))
  `(labels
       ,(mapcar (lambda (bind)
                  `(,(lookup :flet (car bind)) ,(second bind) 
                     ,@(mapcar #'cps-rename (cddr bind))))
                binds)
     ,@(mapcar #'cps-rename body)))

(defcps-requires labels (binds &body body)
  (or (some #'cps-requires-body (mapcar #'cddr binds))
      (cps-requires-body body)))

;;;_   * LET

(defcps-transformer let (binds &rest body)
  (if binds
      (destructuring-bind (var value)
          (ensure-list (first binds))
        (multiple-value-bind (decs new-body)
            (extract-associated-declares var body)
          (if (cps-requires value)
              (cps-transform value
                             `(lambda (,var)
                                ,@decs
                                ,@(if (cdr binds)
                                      (list (cps-transform `(let ,(cdr binds) ,@new-body) k))
                                      (cps-transform-body new-body k))))
              `(let ((,var ,value))
                 ,@decs
                 ,@(if (cdr binds)
                       (list (cps-transform `(let ,(cdr binds) ,@new-body) k))
                       (cps-transform-body new-body k))))))
      `(let () ,@(cps-transform-body body k))))

(defcps-rename let (binds &rest body)
  `(let
       ,(loop
           with orig-env = *env*
           for bind in binds
           for (var value) = (ensure-list bind)
           for new-name = (gensym (strcat "CPS-LET-" (string var) "-"))
           ;; the new bindings will be a gensym and they'll be
           ;; initialized to the value renamed in the old env.
           collect `(,new-name ,(let ((*env* orig-env)) (cps-rename value)))
           do (register :let var new-name))
     ,@(mapcar #'cps-rename body)))

(defcps-requires let (binds &rest body)
  (or (some #'cps-requires
            (mapcar #'second
                    (mapcar #'ensure-list binds)))
      (some #'cps-requires body)))

;;;_   * LET*

(defcps-transformer let* (binds &rest body)
  ;; since renameing is done before transformering this is ok.
  (cps-transform `(let ,binds ,@body) k))

(defcps-rename let* (binds &rest body)
  `(let*
       ,(loop 
           for bind in binds
           for (var value) = (ensure-list bind)
           for new-name = (gensym (strcat "CPS-LET-" (string var) "-"))
           collect `(,new-name ,(cps-rename value))
           do (register :let var new-name))
     ,@(mapcar #'cps-rename body)))

(defcps-requires let* (binds &rest body)
  (cps-requires `(let ,binds ,@body)))

;;;_   * MACROLET

(defcps-rename macrolet (macros &rest body)
  (dolist* ((name args &rest body) macros)
    (register :macrolet name (with-unique-names (macro-form)
                               (eval `(lambda (,macro-form)
                                        (destructuring-bind ,args ,macro-form
                                          ,@body))))))
  (cps-rename `(progn ,@body)))

;;;_   * PROGN

(defcps-transformer progn (&rest body)
  (cond
    ((cdr body)
     (if (cps-requires (first body))
	 (cps-transform (first body)
			(with-unique-names (v)
			  `(lambda (,v)
			     (declare (ignore ,v))
			     ,(cps-transform `(progn ,@(cdr body)) k))))
	 `(progn ,(first body)
		 ,(cps-transform `(progn ,@(cdr body)) k))))
    (body (cps-transform (first body) k))
    (t (cps-transform NIL k))))

(defcps-requires progn (&rest body)
  (some #'cps-requires body))

(defcps-rename progn (&rest body)
  (cond
    ((cdr body) `(progn ,@(mapcar #'cps-rename body)))
    (body (cps-rename (car body)))
    (t (cps-rename nil))))

;;;_   * QUOTE

(defcps-rename quote (thing)
  (declare (ignore thing))
  form)

(defcps-transformer quote (thing)
  (declare (ignore thing))
  (k form))

(defcps-requires quote (thing)
  (declare (ignore thing))
  nil)

;;;_   * SETQ

(defcps-rename setq (&rest vars-values)
  (block rename-setq
    `(progn
       ,@(loop
            for (var value) on vars-values by #'cddr
            collect (acond
                     ((lookup :symbol-macrolet var) (cps-rename `(setf ,it ,value)))
                     ((lookup :let var) `(setq ,it ,(cps-rename value)))
                     (t `(setq ,var ,(cps-rename value))))))))

(defcps-requires setq (&rest vars-values)
  (loop
     for (nil form) on vars-values
     when (cps-requires form)
       return t
     finally (return nil)))

(defcps-transformer setq (var value)
  ;; due to what the renamer does we can assume that setq will get
  ;; transformed with only two values.
  (if (cps-requires value)
      (cps-transform value (with-unique-names (v)
                             `(lambda (,v)
                                ,(k `(setq ,var ,v)))))
      (k `(setq ,var ,value))))

;;;_   * SYMBOL-MACROLET

(defcps-rename symbol-macrolet (macros &body body)
  (dolist* ((var expansion) macros)
    (register :symbol-macrolet var expansion))
  (cps-rename `(progn ,@body)))

;;;_   * UNWIND-PROTECT

(defcps-rename unwind-protect (protected-form &rest cleanups)
  `(unwind-protect
        ,(cps-rename protected-form)
     ,@(mapcar #'cps-rename cleanups)))

(defcps-requires unwind-protect (protected-form &rest cleanups)
  (when (or (cps-requires protected-form)
            (some #'cps-requires cleanups))
    (error "UNWIND-PROTECT can not contain calls to CALL/CC."))
  nil)

;;;_   * THE

(defcps-rename the (type value)
  `(the ,type ,(cps-rename value)))

(defcps-requires the (type value)
  (declare (ignore type))
  (cps-requires value))

(defcps-transformer the (type value)
  (if (cps-requires value)
      (cps-transform value (with-unique-names (v)
			     `(lambda (,v)
				,(k `(the ,type ,v)))))
      (k `(the ,type ,value))))

;;; net yet implemented

;;;_   * Dealing with DECLARE

(defun declare-form-p (form)
  (and (consp form)
       (eql 'declare (car form))))

(defun split-into-body-and-declares (body)
  (loop for form on body
        while (declare-form-p (car form))
        collect (car form) into declares
        finally (return (values (mapcan #'cdr declares) form))))

(defun extract-associated-declares (var body)
  "Returns a declare form containig all the options for VAR in
  the declare forms in BODY."
  (multiple-value-bind (declares body)
      (split-into-body-and-declares body)
    (let ((associated-declares '()))
      (dolist* ((type . args) declares)
        (case type
          ((ignore ignorable dynamic-extent)
           (if (member var args)
               (progn
                 (push `(declare (,type ,var)) associated-declares)
                 (aif (remove var args)
                      (push `(declare (,type ,@it)) body)))
               (push `(declare (,type ,@args)) body)))
          ((special)
           (error "SPECIAL declares not allowed in CALL/CC code."))
          ((inline notinline optimize)
           (push `(declare (,type ,@args)) body))
          ((type)
           (destructuring-bind (type-spec . vars)
               args             
             (if (member var vars)
                 (progn
                   (push `(declare (,type ,type-spec ,var)) associated-declares)
                   (aif (remove var vars)
                        (push `(declare ,type ,type-spec ,@it) body)))
                 (push `(declare (,type ,type-spec ,@vars)) body))))
          (t (if (member var args)
                 (progn
                   (push `(declare (,type ,var)) associated-declares)
                   (awhen (remove var args)
                     (push `(declare (,type ,@it)) body)))
                 (push `(declare (,type ,@args)) body)))))
      (values associated-declares body))))

(defun lambda-form-p (form)
  (and (consp form)
       (or (eql 'cl:lambda (car form))
           (and (eql 'cl:function (car form))
                (consp (second form))
                (eql 'cl:lambda (first (second form)))))))

;;; util

(defun split-body-into-parts (body)
  "Returns the declares, the doc string, and any forms in BODY."
  (flet ((ret (dec doc body)
           (return-from split-body-into-parts (values dec
                                                      (when doc
                                                        (list doc))
                                                      body))))
    (loop
       with doc-string = nil
       with declares = '()
       for form* on body
       for form = (car form*)
       do (cond
            ((and (stringp form) (cdr form*))
             (setf doc-string form))
            ((stringp form)
             (ret declares doc-string form*))
            ((and (consp form) (eql 'cl:declare (car form)))
             (push form declares))
            (t
             (ret declares doc-string form*))))))

;;;_* Emacs Buffer Locals
