
(defpackage #:scheme.internal
  (:use     #:common-lisp
            #:donuts.abs
            #:sb-walker)) ;; see src/pcl/walk.lisp

(defpackage #:scheme
  (:export  #:define
            #:define-syntax-rule))

(in-package #:scheme.internal)

(defmacro scheme:define (form &rest value)
  (case-match form
    ((cons name args) `(defun ,name ,args ,@value))
    (symbol           `(defparameter ,form ,@value))))

(defmacro scheme:define-syntax-rule ((name &rest args) rule)
  `(defmacro ,name ,args
     ,(sb-walker:walk-form
       `,rule
       nil
       (lambda (form context env)
        (declare (ignore context env))
        (if (symbolp form)
            (if (boundp form)
                (symbol-value form)
                (error "Symbol ~A is unbound." form))
            form)))))

(in-package #:donuts.examples)
(use-package :scheme)

(define x 5)
(define-syntax-rule (macro) (print x))
(let ((x 10)) (macro))
;=> 5

(define-syntax-rule (macro2) (print y))
; => The variable Y is unbound.
