;;;;
;;;; Define LET expression in term of LAMBDA.
;;;;

(in-package #:cl-donuts.examples)

(defmacro/iter my-let (bindings &body body)
  (for binding in bindings)
  (destructuring-bind (argument &optional value (type t)) binding
    (collect argument                into arguments)
    (collect value                   into values)
    (collect `(type ,type ,argument) into types))
 `(funcall (lambda ,arguments (declare ,@types) ,@body) ,@values))

(macroexpand-1
 '(my-let ((a 2 fixnum)
           (b 2 fixnum))
   (* a b)))

(FUNCALL
 (LAMBDA (A B)
   (DECLARE (TYPE FIXNUM A)
            (TYPE FIXNUM B))
   (* A B))
 2
 2)

;=> 4
