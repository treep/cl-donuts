;;;;
;;;; npm.lisp -- implementation of the nested pattern matching over ADTs.
;;;;

(in-package #:donuts.abs)

;;; One level pattern matching.
;;;
(defmacro/iter case-match (expr &rest cases)
  (for case in cases)
  ;; for each case in cases we take constructor expression and associated body.
  (destructuring-bind (cons-expr &rest case-body) case
    (etypecase cons-expr
      ;; atomic type (just like in typecase)
      ;; TODO: atoms (like in case)
      (symbol (if (eq '_ cons-expr)
                  (gen `(t ,@case-body))
                  (gen case)))
      ;; structure type, type constructor and its arguments
      ;; TODO: n+k pattern, as-pattern.
      (cons   (destructuring-bind (cons &rest args) cons-expr
                (gen `(,cons
                       ;; we need to generate LET binding (get accessors
                       ;; for this structure type and bind variables from
                       ;; the constructor expression)
                       (let ,(iter (for accessor in (accessors-of cons))
                                   (for arg in args)
                                   ;; do nothing for `_' pattern in the
                                   ;; constructor expression
                                   (unless (eq '_ arg)
                                     (collect `(,arg (,accessor expr)))))
                         ,@case-body)))))))
  ;; KLUDGE: gensyms, but... ok I need to drop out this `defmacro/iter' and
  ;; just use iterate with clauses.
  `(let ((expr ,expr))
     (etypecase expr ,@gens)))

;;; Nested pattern matching compiler.

(defun match->match2 (exprs cases)
  (when cases
    `(match2 ,exprs
       ,(first cases)
       ,(match->match2 exprs (rest cases)))))

(defmacro match (exprs &rest patts)
)

;;; Define function with matcher.
;;;
;;;   This is template since real matcher isn't written yet.
;;;
(defmacro defun/match (name &body body)
  (with-gensyms (arg)
    `(defun ,name (,arg)
       (case-match ,arg
         ,@body))))
