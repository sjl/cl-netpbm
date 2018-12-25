(defpackage :trivial-ppm
  (:use :cl :trivial-ppm.quickutils)
  (:export
    :read-from-file
    :read-from-stream
    :write-to-file
    :write-to-stream)
  (:shadow :read-byte))
