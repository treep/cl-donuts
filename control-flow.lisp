;;;;
;;;; control-flow.lisp -- control flow additions.
;;;;

(in-package #:cl-donuts)

;;; The MANY macro.
;;;
;;; For example:
;;;
;;;   (many defconstant
;;;     (+a+ 1)
;;;     (+b+ 2))
;;;
;;;   (many (foo a b c)
;;;     bar
;;;     (baz q w e))
;;;
(defmacro many (thing &rest forms)
  `(values
    ,@(map-each (form forms)
        (if (atom form)
            (if (atom thing)
                `(,thing ,form)
                `(,@thing ,form))
            (if (atom thing)
                `(,thing ,@form)
                `(,@thing ,@form))))))

;;; DEFMACRO/ITER is like simple DEFMACRO but:
;;;
;;;   * Forms from the BODY expressed inside of the ITER macro.
;;;
;;;   * Last form from the BODY is placed under (finally (return ...)).
;;;
;;;   * GEN clause collects values into GENS list (code place holder for macros).
;;;
(defmacro-clause (gen expr)
  `(collect ,expr into gens))

(defmacro-clause (result expr)
  `(finally (return ,expr)))

(defmacro iter* (&rest forms)
  `(iter
     ,@(butlast forms)
     (finally (return ,(last* forms)))))

(defmacro defmacro/iter (name args &body body)
  `(defmacro ,name ,args
     (iter* ,@body)))
