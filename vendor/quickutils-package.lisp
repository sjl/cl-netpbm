(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "TRIVIAL-PPM.QUICKUTILS")
    (defpackage "TRIVIAL-PPM.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use :cl))))

(in-package "TRIVIAL-PPM.QUICKUTILS")

;; need to define this here so sbcl will shut the hell up about it being
;; undefined when compiling quickutils.lisp.  computers are trash.
(defparameter *utilities* nil)

