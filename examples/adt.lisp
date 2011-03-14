;;;;
;;;; examples/adt.lisp -- define some ADTs.
;;;;

(in-package #:cl-donuts.examples)

(defdata .list .nil (.cons t t))

;;; Expands to:
;;;
;;; (PROGN
;;;   (DEFTYPE .LIST () '(OR .NIL .CONS))
;;;   (DEFSTRUCT (.NIL (:CONSTRUCTOR .NIL NIL)))
;;;   (DEFCONSTANT .NIL (.NIL))
;;;   (DEFSTRUCT (.CONS (:CONSTRUCTOR .CONS (.CONS930 .CONS931))
;;;                     (:CONC-NAME CL-DONUTS::||))
;;;     (.CONS930 NIL :TYPE T)
;;;     (.CONS931 NIL :TYPE T))
;;;   (DEFMETHOD ACCESSORS-OF ((SYMBOL (EQL (QUOTE .CONS))))
;;;     (DECLARE (IGNORE SYMBOL))
;;;     '(.CONS930 .CONS931)))
;;;
;;; Note: there is no normal accessors for the .CONS structure, so only one way
;;;       for slot access is pattern matching over that structure. Also:
;;;
;;;   (funcall (first (accessors-of '.cons)) (.cons 1 2)) => 1

(defdata list.
  nil.
  (cons. (first. t)
         (rest. t)))

;;; Expands to:
;;;
;;; (PROGN
;;;   (DEFTYPE LIST. () '(OR NIL. CONS.))
;;;   (DEFSTRUCT (NIL. (:CONSTRUCTOR NIL. NIL)))
;;;   (DEFCONSTANT NIL. (NIL.))
;;;   (DEFSTRUCT
;;;       (CONS. (:CONSTRUCTOR CONS. (FIRST. REST.))
;;;              (:CONC-NAME CL-DONUTS::||))
;;;     (FIRST. NIL :TYPE T)
;;;     (REST. NIL :TYPE T))
;;;   (DEFMETHOD ACCESSORS-OF ((SYMBOL (EQL (QUOTE CONS.))))
;;;     (DECLARE (IGNORE SYMBOL))
;;;     '(FIRST. REST.)))
;;;
;;; Note: in that version we can use FIRST./REST. accessors directly:
;;;
;;;   (rest. (cons. 1 2)) => 2

(defdata .list.
  .nil.
  (.cons. t .list.))

;;; Expands to:
;;;
;;; (PROGN
;;;   (DEFTYPE .LIST. () '(OR .NIL. .CONS.))
;;;   (DEFSTRUCT (.NIL. (:CONSTRUCTOR .NIL. NIL)))
;;;   (DEFCONSTANT .NIL. (.NIL.))
;;;   (DEFSTRUCT
;;;       (.CONS. (:CONSTRUCTOR .CONS. (.CONS.989 .CONS.990))
;;;               (:CONC-NAME CL-DONUTS::||))
;;;     (.CONS.989 NIL :TYPE T)
;;;     (.CONS.990 NIL :TYPE .LIST.))
;;;   (DEFMETHOD ACCESSORS-OF ((SYMBOL (EQL (QUOTE .CONS.))))
;;;     (DECLARE (IGNORE SYMBOL))
;;;     '(.CONS.989 .CONS.990)))
;;;
;;; Note: if we try using this (real) lists as conses we get the assertion:
;;;
;;;   (.cons. 5 6) => The value 6 is not of type (OR .NIL. .CONS.).

(defdata list[fixnum]
  empty/list[fixnum]
  (cons/list[fixnum] fixnum list[fixnum]))

;;; Expands to:
;;;
;;; (PROGN
;;;   (DEFTYPE LIST[FIXNUM] () '(OR EMPTY/LIST[FIXNUM] CONS/LIST[FIXNUM]))
;;;   (DEFSTRUCT (EMPTY/LIST[FIXNUM] (:CONSTRUCTOR EMPTY/LIST[FIXNUM] NIL)))
;;;   (DEFCONSTANT EMPTY/LIST[FIXNUM] (EMPTY/LIST[FIXNUM]))
;;;   (DEFSTRUCT
;;;       (CONS/LIST[FIXNUM]
;;;        (:CONSTRUCTOR CONS/LIST[FIXNUM]
;;;         (CONS/LIST[FIXNUM]999 CONS/LIST[FIXNUM]1000))
;;;        (:CONC-NAME CL-DONUTS::||))
;;;     (CONS/LIST[FIXNUM]999 NIL :TYPE FIXNUM)
;;;     (CONS/LIST[FIXNUM]1000 NIL :TYPE LIST[FIXNUM]))
;;;   (DEFMETHOD ACCESSORS-OF ((SYMBOL (EQL (QUOTE CONS/LIST[FIXNUM]))))
;;;     (DECLARE (IGNORE SYMBOL))
;;;     '(CONS/LIST[FIXNUM]999 CONS/LIST[FIXNUM]1000)))
;;;
;;; and:
;;;
;;;   (cons/list[fixnum] "4" empty/list[fixnum]) => The value "4" is not of type FIXNUM.

;;; Note: we can think about polymorphic data type (forall a (list a)) in two ways:
;;;
;;;   * Asume that (forall a (list a)) is inductive union of all concrete types
;;;     (some a (list a)).
;;;
;;;   * Or asume that List = (forall a (list a)) = (forall T (list T)), and
;;;     concrete type (some b (list b)) is application of (forall T (list T)) to
;;;     type b (that can be implemented as type-assertion, or maybe via. some
;;;     subtyping method).

;;; Since this data types is normal CLOS structures we can use any methods,
;;; for example:

(defdata tree
  (leaf   (value))
  (branch (left  tree)
          (right tree)))

(defmethod print-object ((leaf leaf) stream)
  (format stream "~A" (value leaf)))

(defmethod print-object ((branch branch) stream)
  (format stream "<~A . ~A>" (left branch) (right branch)))

;;; Then:
;;;
;;; (branch (leaf "string") (leaf 1000))
;;; => <string . 1000>
