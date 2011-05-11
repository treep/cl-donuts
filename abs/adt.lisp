;;;;
;;;; adt.lisp -- implementation of the (inductive) algebraic data types.
;;;;

(in-package #:donuts.abs)

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

(defgeneric accessors-of (constructor))

(defun error-in-structure-type (product-type constructor)
  (error "Bad structure type definition: ~A~%Use atomic type: ~A~%Or add slots: (~A ...)~%"
         product-type constructor constructor))

(defmacro/iter defdata (type-spec &rest type-expression)
  (with type-name = (etypecase type-spec
                      (symbol type-spec)
                      (cons   (first type-spec))))
  (with virtualp  = (when (consp type-spec)
                      (find :virtual type-spec)))
  (for product-type in type-expression)
  (etypecase product-type
    ;; atomic type
    (symbol (collect product-type into constructors)
            (unless virtualp
              (gen `(defstruct (,product-type (:constructor ,product-type ())))))
            (unless virtualp
              (gen `(defconstant ,product-type (,product-type)))))
    ;; structure type
    (cons   (destructuring-bind (constructor &rest args) product-type
              (unless args
                (error-in-structure-type product-type constructor))
              (collect constructor into constructors)
              (multiple-value-bind (accessors slots)
                  (iter*
                    (for arg in args)
                    (etypecase arg
                      ;; just type, without accessor name
                      (symbol (let ((accessor (intern (string (gensym (string constructor))))))
                                (collect accessor into accessors)
                                (gen `(,accessor nil :type ,arg))))
                      ;; accessor name, and maybe type, initial-value and read-only properties
                      (cons   (destructuring-bind (accessor &optional (type t) &key initial-value read-only) arg
                                (collect accessor into accessors)
                                (gen `(,accessor ,initial-value :type ,type :read-only ,read-only)))))
                    (values accessors gens))
                (unless virtualp
                  (gen `(defstruct (,constructor
                                     (:constructor ,constructor ,accessors)
                                     (:conc-name ||))
                          ,@slots)))
                (gen `(defmethod accessors-of ((symbol (eql ',constructor)))
                        (declare (ignore symbol))
                        '(,@accessors)))))))
  (unless virtualp
    (gen `(deftype ,type-name () '(or ,@constructors))))
  `(values ,@gens))
