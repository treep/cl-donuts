
(in-package #:cl-donuts)

;;; The MAP-EACH macro.
;;;
;;;   Writing
;;;
;;;     (map-each (e list)
;;;       (do-some e))
;;;
;;;   instead of
;;;
;;;     (mapcar #'(lambda (e)
;;;                 (do-some e))
;;;             list)
;;;
(defmacro map-each ((element list) &body body)
  `(mapcar #'(lambda (,element)
               ,@body)
           ,list))

;;; The FILTER-EACH macro.
;;;
(defmacro filter-each ((element list) &body body)
  `(mapcan #'(lambda (,element)
               (and (progn ,@body)
                    (list ,element)))
           ,list))

(defun filter (predicate list)
  (filter-each (e list)
    (funcall predicate e)))

;;; The COLLECT-EACH macro.
;;;
;;; Comment from CMU CL: "the ultimate collection macro...".
;;; 
;;; Collect some values somehow. Each of the collections specifies a
;;; bunch of things which collected during the evaluation of the body
;;; of the form. The name of the collection is used to define a local
;;; macro, a la MACROLET. Within the body, this macro will evaluate
;;; each of its arguments and collect the result, returning the
;;; current value after the collection is done. The body is evaluated
;;; as a PROGN; to get the final values when you are done, just call
;;; the collection macro with no arguments.
;;;
;;; INITIAL-VALUE is the value that the collection starts out with,
;;; which defaults to NIL. FUNCTION is the function which does the
;;; collection. It is a function which will accept two arguments: the
;;; value to be collected and the current collection. The result of
;;; the function is made the new value for the collection. As a
;;; totally magical special-case, FUNCTION may be COLLECT, which tells
;;; us to build a list in forward order; this is the default. If an
;;; INITIAL-VALUE is supplied for COLLECT, the stuff will be RPLACD'd
;;; onto the end. Note that FUNCTION may be anything that can appear
;;; in the functional position, including macros and lambdas.
(defmacro collect-each (collections &body body)
  ;; Helper functions for COLLECT, which become the expanders of the
  ;; MACROLET definitions created by COLLECT.
  ;;
  ;; COLLECT-NORMAL-EXPANDER handles normal collection macros.
  ;;
  ;; COLLECT-LIST-EXPANDER handles the list collection case. N-TAIL
  ;; is the pointer to the current tail of the list, or NIL if the list
  ;; is empty.
  (flet ((collect-normal-expander (n-value fun forms)
           `(progn
              ,@(map-each (form forms)
                  `(setq ,n-value (,fun ,form ,n-value)))
              ,n-value))
         (collect-list-expander (n-value n-tail forms)
           (let ((n-res (gensym)))
             `(progn
                ,@(map-each (form forms)
                    `(let ((,n-res (cons ,form nil)))
                       (cond (,n-tail (setf (cdr ,n-tail) ,n-res)
                                      (setq ,n-tail ,n-res))
                             (t       (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                ,n-value))))
    (let (macros binds)
      (dolist (spec collections)
        (unless (<= 1 (length spec) 3)
          (error "malformed collection specifier: ~S" spec))
        (let* ((name (first spec))
               (default (second spec))
               (kind (or (third spec) 'collect))
               (n-value (gensym (concatenate 'string
                                             (symbol-name name)
                                             "-N-VALUE-"))))
          (push `(,n-value ,default) binds)
          (if (eq kind 'collect)
              (let ((n-tail (gensym (concatenate 'string
                                                 (symbol-name name)
                                                 "-N-TAIL-"))))
                (if default
                    (push `(,n-tail (last ,n-value)) binds)
                    (push n-tail binds))
                (push `(,name (&rest args)
                              (collect-list-expander ',n-value ',n-tail args))
                      macros))
              (push `(,name (&rest args)
                            (collect-normal-expander ',n-value ',kind args))
                    macros))))
      `(macrolet ,macros (let* ,(nreverse binds) ,@body)))))

;;; Folders.

(defun foldl (function sequence &optional initial-value)
  (reduce function sequence :initial-value initial-value))

(defun foldr (function sequence &optional initial-value)
  (reduce function sequence :initial-value initial-value :from-end t))

;;; Evil folder -- that function know past and know future.
;;;
(defun fold (function list &optional initial-value)
  (let ((result initial-value)
        (list (copy-list list)))
    (tagbody
     :start
     (unless (endp list)
       (setq result (funcall function (car list) (cdr list) result))
       (setq list (cdr list))
       (go :start)))
    result))

;;;

(defun append* (thing list)
  (if (atom thing)
      (cons thing list)
      (append thing list)))

(declaim (inline last*))
(defun last* (list)
  (first (last list)))

(declaim (inline listify))
(defun listify (thing)
  (if (atom thing)
      (list thing)
      thing))
