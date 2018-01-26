#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :trivial-ppm)
(time (asdf:test-system :trivial-ppm))
(quit)
