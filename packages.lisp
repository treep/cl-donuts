
(defpackage #:cl-donuts
  (:use     #:common-lisp
            #:alexandria
            #:iterate
            #:anaphora)
  (:export  ;; lists
            #:map-each
            #:filter-each
            #:filter
            #:collect-each
            #:foldl
            #:foldr
            #:fold
            #:append*
            #:last*
            #:listify
            ;; control flow
            #:many
            #:gen
            #:gens
            #:result
            #:iter*
            #:defmacro/iter
            ;; I/O
            #:read-sexp-from-file
            ;; ADTs
            #:accessors-of
            #:defdata
            ;; PM
            #:case-match
            #:match))

(defpackage #:cl-donuts.examples
  (:use     #:common-lisp
            #:alexandria
            #:iterate
            #:anaphora
            #:cl-donuts))
