#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :cl-netpbm)
(time (asdf:test-system :cl-netpbm))
(quit)
