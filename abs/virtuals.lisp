;;;;
;;;; virtuals.lisp -- define some virtual data types.
;;;;

(in-package #:donuts.abs)

(defdata (list :virtual)
   nil
   (cons (first)
         (rest)))
