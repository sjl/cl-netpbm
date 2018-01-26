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
