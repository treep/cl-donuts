;;;;
;;;; examples/npm.lisp -- pattern matching examples.
;;;;

(in-package #:cl-donuts.examples)

;;; Lets take some RDT:

(defdata tree
  (leaf   (value))
  (branch (left  tree)
          (right tree)))

(defmethod print-object ((leaf leaf) stream)
  (format stream "~A" (value leaf)))

(defmethod print-object ((branch branch) stream)
  (format stream "<~A . ~A>" (left branch) (right branch)))

;;; Then one level pattern matching (or `core-expression') such that

(case-match (branch (leaf 3) (leaf 4))
  ((leaf x)     `(:leaf ,x))
  ((branch l r) `(:branch ,l ,r)))

;;; expands to:
;;;
;;; (LET ((EXPRESSION (BRANCH (LEAF 3) (LEAF 4))))
;;;   (ETYPECASE EXPRESSION
;;;      (LEAF   (LET ((X (VALUE EXPRESSION)))
;;;                `(:LEAF ,X)))
;;;      (BRANCH (LET ((L (LEFT EXPRESSION))
;;;                    (R (RIGHT EXPRESSION)))
;;;                `(:BRANCH ,L ,R)))))

;;; XXX: NPM is not implemented yet.
