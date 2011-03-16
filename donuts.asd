
(defsystem donuts
  :description "Functional programming abstractions for Common Lisp."
  :version     "0.0.2"
  :licence     "Public Domain / 0-clause MIT"
  :serial      t
  :depends-on  (#:alexandria #:iterate #:anaphora)
  :components  (;; make the `donuts.base' package which collect simbols from the
                ;; `alexandria', `iterate' and `anaphora' packages
                (:file "base-package")
                ;; some random utilits (which is not so important, anyway)
                (:module "utils"
                         :components ((:file "package")
                                      (:file "lists")
                                      (:file "control-flow")
                                      (:file "io")))
                ;; implement target abstractions
                (:module "abs"
                         :components ((:file "package")
                                      (:file "adt")
                                      (:file "npm")))))
