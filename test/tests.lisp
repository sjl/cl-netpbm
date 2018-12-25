(in-package :trivial-ppm/test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(symb 'test- name)
    (let ((*package* ,*package*)
          (r #(255 0 0))
          (g #(0 255 0))
          (b #(0 0 255))
          (k #(0 0 0))
          (w #(255 255 255)))
      (declare (ignorable r g b k w))
      ,@body)))

(defun make-image-array (initial-data)
  (make-array (list (length (elt initial-data 0))
                    (length initial-data))
              :initial-contents (transpose initial-data)))

(defmacro check (form expected-data expected-format expected-bit-depth)
  (with-gensyms (data format bit-depth)
    `(multiple-value-bind (,data ,format ,bit-depth) ,form
       (is (equalp (make-image-array ,expected-data) ,data))
       (is (eql ,expected-format ,format))
       (is (= ,expected-bit-depth ,bit-depth)))))

(defun run-tests ()
  (1am:run))


;;;; Tests --------------------------------------------------------------------
(define-test 1x1-black-ascii-pbm
  (check (trivial-ppm:read-from-file "test/data/1x1-black.ascii.pbm")
         '((0))
         :pbm
         1))

(define-test 1x1-black-ascii-pgm
  (check (trivial-ppm:read-from-file "test/data/1x1-black.ascii.pgm")
         '((0))
         :pgm
         255))

(define-test 1x1-black-ascii-ppm
  (check (trivial-ppm:read-from-file "test/data/1x1-black.ascii.ppm")
         `((,k))
         :ppm
         255))

(define-test 4x3-rgb.ascii-ppm
  (check (trivial-ppm:read-from-file "test/data/4x3-rgb.ascii.ppm")
         `((,r ,r ,r ,r)
           (,g ,g ,g ,g)
           (,b ,b ,b ,b))
         :ppm
         255))


(define-test 1x1-black-binary-pbm
  (check (trivial-ppm:read-from-file "test/data/1x1-black.binary.pbm")
         '((0))
         :pbm
         1))

(define-test 1x1-black-binary-pgm
  (check (trivial-ppm:read-from-file "test/data/1x1-black.binary.pgm")
         '((0))
         :pgm
         255))

(define-test 1x1-black-binary-ppm
  (check (trivial-ppm:read-from-file "test/data/1x1-black.binary.ppm")
         `((,k))
         :ppm
         255))

(define-test 4x3-rgb.binary-ppm
  (check (trivial-ppm:read-from-file "test/data/4x3-rgb.binary.ppm")
         `((,r ,r ,r ,r)
           (,g ,g ,g ,g)
           (,b ,b ,b ,b))
         :ppm
         255))


;;;; Fuzzer -------------------------------------------------------------------
(defparameter *fuzz-test-count* 500)


(defun random-bit ()
  (random 2))

(defun random-gray ()
  (random 256))

(defun random-color ()
  (make-array 3 :initial-contents (list (random 256)
                                        (random 256)
                                        (random 256))))

(defun random-format ()
  (ecase (random 3)
    (0 :pbm)
    (1 :pgm)
    (2 :ppm)))

(defun make-random-array ()
  (let* ((width (1+ (random 50)))
         (height (1+ (random 50)))
         (format (random-format))
         (data (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref data x y)
              (ecase format
                (:pbm (random-bit))
                (:pgm (random-gray))
                (:ppm (random-color))))))
    (values data format)))


(define-test fuzz-ascii
  (dotimes (i *fuzz-test-count*)
    (multiple-value-bind (original original-format) (make-random-array)
      (write-to-file "test/data/fuzz.ascii" original
                     :if-exists :supersede
                     :format original-format
                     :encoding :ascii)
      (multiple-value-bind (new new-format)
          (read-from-file "test/data/fuzz.ascii")
        (is (eql original-format new-format))
        (is (equalp original new))))))

(define-test fuzz-binary
  (dotimes (i *fuzz-test-count*)
    (multiple-value-bind (original original-format) (make-random-array)
      (write-to-file "test/data/fuzz.binary" original
                     :if-exists :supersede
                     :format original-format
                     :encoding :binary)
      (multiple-value-bind (new new-format)
          (read-from-file "test/data/fuzz.binary")
        (is (eql original-format new-format))
        (is (equalp original new))))))

(define-test fuzz-convert
  (dotimes (i *fuzz-test-count*)
    (multiple-value-bind (original original-format) (make-random-array)
      (write-to-file "test/data/fuzz.convert.in" original
                     :if-exists :supersede
                     :format original-format
                     :encoding :ascii)
      (uiop:run-program (list "convert" "-format" (ecase original-format
                                                    (:ppm "ppm")
                                                    (:pgm "pgm")
                                                    (:pbm "pbm"))
                              "test/data/fuzz.convert.in"
                              "test/data/fuzz.convert.out"))
      (multiple-value-bind (new new-format)
          (read-from-file "test/data/fuzz.convert.out")
        (is (eql original-format new-format))
        (is (equalp original new))))))
