(defpackage :netpbm
  (:use :cl :netpbm.quickutils)
  (:export
    :read-from-file
    :read-from-stream
    :write-to-file
    :write-to-stream)
  (:shadow :read-byte))
