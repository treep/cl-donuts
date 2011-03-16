;;;;
;;;; base-package.lisp -- make the `donuts.base' package which collect simbols from
;;;;                      the `alexandria', `iterate' and `anaphora' packages.
;;;;

(in-package #:cl-user)

(defun external-symbols (package-designator)
  "Return all external symbols from package."
  (loop :for symbol :being :the :external-symbol :in package-designator
        :collect symbol))

(defmacro defpackage-wrapper (name packages)
  "Define package NAME with import all external symbols from the PACKAGES and
export those symbols from created package."
  (loop :for package :in packages
        :for symbols := (external-symbols package)
        :for exports := (append exports symbols)
        :collect `(:import-from ,package ,@symbols) :into imports
        :finally (return `(defpackage ,name ,@imports (:export ,@exports)))))

(defpackage-wrapper #:donuts.base
  (#:alexandria #:iterate #:anaphora))
