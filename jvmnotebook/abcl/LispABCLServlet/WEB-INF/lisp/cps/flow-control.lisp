;;;; -*- lisp -*-

(defmacro if-bind (var test then &optional else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  `(let ((,var ,test))
     (if ,var ,then ,else)))

(defmacro aif (test then &optional else)
  "Just like IF-BIND but the var is always IT."
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  "Just like @code{WHEN} except @var{var} will be bound to the
  result of @var{test} in @var{body}."
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  "Just like @code{WHEN} expect the symbol @code{IT} will be
  bound to the result of @var{test} in @var{body}."
  `(when-bind it ,test ,@body))

(defmacro cond-bind (var &rest clauses)
  "Just like @code{COND} @var{var} will be bound to the result of
  the first cond which returns true."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
                  (progn ,@body)
                  (cond-bind ,var ,@others)))
      nil))

(defmacro acond (&rest clauses)
  "Just like @code{COND-BIND} except the var is automatically
  @code{IT}."
  `(cond-bind it ,@clauses))

(defmacro whichever (&rest possibilities)
  "Evaluates one (and only one) of its args, which one is chosen at random"
  `(ecase (random ,(length possibilities))
     ,@(loop for poss in possibilities
             for x from 0
             collect (list x poss))))

(defmacro xor (&rest datums)
  "Evaluates the args one at a time. If more than one arg returns true
  evaluation stops and NIL is returned. If exactly one arg returns
  true that value is retuned."
  (let ((state (gensym "XOR-state-"))
        (block-name (gensym "XOR-block-"))
        (arg-temp (gensym "XOR-arg-temp-")))
    `(let ((,state nil)
           (,arg-temp nil))
       (block ,block-name
         ,@(loop
              for arg in datums
              collect `(setf ,arg-temp ,arg)
              collect `(if ,arg-temp
                           ;; arg is T, this can change the state
                           (if ,state
                               ;; a second T value, return NIL
                               (return-from ,block-name nil)
                               ;; a first T, swap the state
                               (setf ,state ,arg-temp))))
         (return-from ,block-name ,state)))))

(defmacro switch ((obj &key (test #'eql)) &body clauses)
  "Evaluate the first clause whose car satisfies @code{(funcall
  test car obj)}."
  ;; NB: There is no need to do the find-if and the remove here, we
  ;; can just as well do them with in the expansion
  (let ((default-clause (find-if (lambda (c) (eq t (car c))) clauses)))
    (when default-clause
      (setf clauses (remove default-clause clauses :test #'equalp)))
    (let ((obj-sym (gensym))
          (test-sym (gensym)))
      `(let ((,obj-sym ,obj)
             (,test-sym ,test))
         (cond
           ,@(mapcar (lambda (clause)
                       (let ((keys (ensure-list (car clause)))
                             (form (cdr clause)))
                         `((or ,@(mapcar (lambda (key)
					   `(funcall ,test-sym ',key ,obj-sym))
					 keys))
			   ,@form)))
                     clauses)
           ,@(when default-clause
                   `((t ,@(cdr default-clause)))))))))

(defmacro eswitch ((obj &key (test #'eql)) &rest body)
  "Like @code{SWITCH} but signals an error if no clause succeds."
  (let ((obj-sym (gensym)))
    `(let ((,obj-sym ,obj))
       (switch (,obj-sym :test ,test)
               ,@body
               (t (error "Unmatched SWITCH. Testing against ~A." ,obj-sym))))))

(defmacro cswitch ((obj &key (test #'eql)) &rest body)
  "Like @code{SWITCH} but signals a continuable error if no
  clause matches."
  (let ((obj-sym (gensym)))
    `(let ((,obj-sym ,obj))
       (switch (,obj-sym :test ,test)
               ,@body
               (t (cerror "Unmatched SWITCH. Testing against ~A." ,obj-sym))))))

(defmacro with* (&body body)
  (cond
    ((cddr body)
     (append (first body) `((with* ,@(cdr body)))))
    ((cdr body)
     `(,@(first body) ,(second body)))
    (body (first body))
    (t nil)))

(defmacro mvprog1 (form &body body)
  `(let ((values (multiple-value-list ,form)))
     ,@body
     (values-list values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright (c) 2002-2005, Edward Marco Baringer
;;;; All rights reserved. 
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:
;;;; 
;;;;  - Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 
;;;;  - Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;
;;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;;    of its contributors may be used to endorse or promote products
;;;;    derived from this software without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
