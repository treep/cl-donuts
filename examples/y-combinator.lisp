;;;;
;;;; Y-COMBINATOR and Z-COMBINATOR examples (very slow :))
;;;;
;;;;   Y is always call-by-value
;;;;   Z is call-by-name in lazy languages
;;;;

(in-package #:cl-user)

;;;; abbrevations

(setf (symbol-function '!)  (symbol-function 'funcall)
      (symbol-function '!!) (symbol-function 'apply))

(defmacro ? (args &body body)
  `(lambda ,args ,@body))

;;;; combinators

(defstruct combinator
  (name     nil :type symbol)
  (function nil :type function))

(defmethod print-object ((combinator combinator) stream)
  (print-unreadable-object (combinator stream :type t)
    (format stream "~A" (combinator-name combinator))))

(defconstant +y-combinator+
  (make-combinator
   :name     'y-combinator
   :function (? (f) (! (? (g) (! g g))
                       (? (g) (! f (? (&rest a)
                                     (!! (! g g) a))))))))

(defconstant +z-combinator+
  (make-combinator
   :name     'z-combinator
   :function (? (f) (! (? (g) (! f (? (x) (! (! g g) x))))
                       (? (g) (! f (? (x) (! (! g g) x))))))))

;;;; default combinator and default combinator rebindins

(defparameter *default-combinator* +y-combinator+)

(defmacro with-y-combinator (&body body)
  `(let ((*default-combinator* +y-combinator+))
     ,@body))

(defmacro with-z-combinator (&body body)
  `(let ((*default-combinator* +z-combinator+))
     ,@body))

;;;; recursive functions application and abstraction

(defun x-call (x-function &rest args)
  (apply (funcall (combinator-function *default-combinator*) x-function) args))

(defmacro x-function ((name &rest args) &body body)
  `(lambda (,name)
     (lambda ,args
       (macrolet ((,name (&rest args)
                    `(funcall ,',name ,@args)))
         ,@body))))

;;;; recursive functions definition

(defmacro x-defun (name args &body body)
  `(defun ,name ,args
     (x-call (x-function (,name ,@args) ,@body) ,@args)))

;;;; examples

(x-call
 (x-function (factorial n)
   (if (zerop n)
       1
       (* n (factorial (1- n)))))
 5)

(x-defun factorial (n)
  (if (zerop n)
      1 
      (* n (factorial (1- n)))))

(x-defun fib (n)
  (case n
    (0 0)
    (1 1)
    (otherwise (+ (fib (- n 1))
                  (fib (- n 2))))))
