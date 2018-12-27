#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :netpbm)
(time (asdf:test-system :netpbm))
(quit)
