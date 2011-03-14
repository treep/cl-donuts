;;;;
;;;; adt.lisp -- implementation of the (inductive) algebraic data types.
;;;;

(in-package #:cl-donuts)

;;; We assume that ADT is an (typological) isomorphism in the following form:
;;;
;;;   NewType ~ TypeExpr(NewType)
;;;
;;; Where NewType is
;;;
;;;   NewType ::= TypeName | TypeName [TypeParameter*]
;;;   TypeName, TypeParameter is symbols
;;;
;;; And TypeExpr is a disjoint union (or sum type) over several product types, i.e.:
;;;
;;;   TypeExpr      ::= ProductType <|> ProductType <|> ... <|>
;;;   ProductType   ::= AtomicType | StructureType
;;;   AtomicType    ::= Constructor
;;;   StructureType ::= Constructor [Type | Accessor :: Type]*
;;;   Type          ::= NewType (for RDT) | SomeTypeInUniverse
;;;   Constructor, Accessor is symbols
;;;   SomeTypeInUniverse is a valid type expression in the host language (CL)
;;;
;;; It means that:
;;;
;;;   <|>           is typological union
;;;   `Constructor' is n-ary typological operation
;;;
;;; CL specific notes:
;;;
;;;   Atomic types maps into `defstruct' and `defconstant'
;;;   Structure type types maps into `defstruct'
;;;   Variant types maps into `deftype/or'
;;;
;;; Also, type parameters is not implemented in this version (heterogeneity by default).
;;;
(defgeneric accessors-of (constructor))

(defun error-in-structure-type (product-type constructor)
  (error "Bad structure type definition: ~A~%Use atomic type: ~A~%Or add slots: (~A ...)~%"
         product-type constructor constructor))

(defmacro/iter defdata (type-name &rest type-expression)
  (for product-type in type-expression)
  (etypecase product-type
    ;; atomic type
    (symbol (collect product-type into constructors)
            (gen `(defstruct (,product-type (:constructor ,product-type ()))))
            (gen `(defconstant ,product-type (,product-type))))
    ;; structure type
    (cons   (destructuring-bind (constructor &rest slots) product-type
              (unless slots
                (error-in-structure-type product-type constructor))
              (collect constructor into constructors)
              (multiple-value-bind (accessors defs)
                  (iter*
                    (for slot in slots)
                    (etypecase slot
                      (symbol (let ((accessor (intern (string (gensym (string constructor))))))
                                (collect accessor into accessors)
                                (gen `(,accessor nil :type ,slot))))
                      (cons   (let ((accessor (first slot)))
                                (collect accessor into accessors)
                                (gen `(,accessor ,(third slot) :type ,(aif (second slot) it t))))))
                    (values accessors gens))
                (gen `(defstruct (,constructor
                                   (:constructor ,constructor ,accessors)
                                   (:conc-name ||))
                        ,@defs))
                (gen `(defmethod accessors-of ((symbol (eql ',constructor)))
                        (declare (ignore symbol))
                        '(,@accessors)))))))
  `(progn
     (deftype ,type-name () '(or ,@constructors))
     ,@gens))
