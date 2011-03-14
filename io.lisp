
(in-package :cl-donuts)

(defun read-sexp-from-file (pathname-designator)
  "Read exactly one s-expression from file."
  (with-open-file (stream pathname-designator)
    (let* ((result (read stream))
           (eof-result (cons nil nil))
           (after-result (read stream nil eof-result)))
      (unless (eql after-result eof-result)
        (error "more than one expression in file ~S" pathname-designator))
      result)))
