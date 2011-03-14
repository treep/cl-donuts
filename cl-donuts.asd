
(defsystem cl-donuts
  :description "Common Lisp + more non-standarted friends."
  :version     "0.0.1"
  :licence     "Public Domain / 0-clause MIT"
  :serial      t
  :depends-on  (#:alexandria #:iterate #:anaphora)
  :components  ((:file "packages")
                ;; some random utilits
                (:file "lists")
                (:file "control-flow")
                (:file "io")
                ;; some weight
                (:file "adt")
                (:file "npm")))
