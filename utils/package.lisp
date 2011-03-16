
(defpackage #:donuts.utils
  (:use     #:common-lisp
            #:donuts.base)
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
	    #:read-sexp-from-file))
