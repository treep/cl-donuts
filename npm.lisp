;;;;
;;;; npm.lisp -- implementation of the nested pattern matching over ADTs.
;;;;

(in-package #:cl-donuts)

;;; One level pattern matching.
;;;
(defmacro/iter case-match (expression &rest cases)
  (for case in cases)
  (let ((type-cons (first case))
        (case-body (rest  case)))
    (etypecase type-cons
      ;; atomic type
      (symbol (gen `(,type-cons ,@case-body)))
      ;; structure type
      (cons   (let ((type-cons* (first type-cons))
                    (type-args  (rest type-cons)))
                (gen `(,type-cons*
                       (let ,(iter (for accessor in (accessors-of type-cons*))
                                   (for arg in type-args)
                                   (collect `(,arg (,accessor expression))))
                         ,@case-body)))))))
  `(let ((expression ,expression))
     (etypecase expression ,@gens)))

;;; Nested pattern matching compiler.
;;;
(defmacro match (expression &rest cases)
  (error "XXX: NPM is not implemented yet.~%")
)
