(in-package :trivial-ppm)

;;;; Utils --------------------------------------------------------------------
(defun skip-comment (stream)
  (peek-char #\newline stream nil nil)
  (read-char stream nil nil))

(defun skip-whitespace (stream)
  (loop :while (eql #\# (peek-char t stream nil nil))
        :do (skip-comment stream)))

(defun peek (stream)
  (peek-char nil stream nil nil))


(defun read-number (stream)
  (skip-whitespace stream)
  (loop :with i = 0
        :for ch = (peek stream)
        :while ch
        :for digit = (digit-char-p ch)
        :while digit
        :do (read-char stream)
        :do (setf i (+ (* i 10) digit))
        :finally (return i)))

(defun write-number (value stream)
  (format stream "~D " value))


(defun read-magic-byte (stream)
  (assert (eql (read-byte stream) (char-code #\P)))
  (code-char (read-byte stream)))


(defun file-format (magic-byte)
  "Return `(values format binary?)` for the given magic byte."
  (ecase magic-byte
    (#\1 (values :pbm nil))
    (#\2 (values :pgm nil))
    (#\3 (values :ppm nil))
    (#\4 (values :pbm t))
    (#\5 (values :pgm t))
    (#\6 (values :ppm t))))

(defun magic-byte (file-format binary?)
  (if binary?
    (ecase file-format
      (:pbm #\4)
      (:pgm #\5)
      (:ppm #\6))
    (ecase file-format
      (:pbm #\1)
      (:pgm #\2)
      (:ppm #\3))))


(defun pixel-type (format bit-depth)
  (ecase format
    (:pbm 'bit)
    (:pgm `(integer 0 ,bit-depth))
    (:ppm `(simple-array (integer 0 ,bit-depth) (3)))))


;;;; PPM ----------------------------------------------------------------------
(defun read% (stream format binary?)
  (let* ((width (read-number stream))
         (height (read-number stream))
         (bit-depth (if (eql :pbm format) 1 (read-number stream)))
         (data (make-array (list width height)
                 :element-type (pixel-type format bit-depth)))
         (reader (if binary? #'read-byte #'read-number)))
    (when binary?
      (read-char stream)) ; chomp last newline before bytes
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref data x y)
              (ecase format
                (:pbm (funcall reader stream))
                (:pgm (funcall reader stream))
                (:ppm (make-array 3
                        :initial-contents (list (funcall reader stream)
                                                (funcall reader stream)
                                                (funcall reader stream))
                        :element-type 'fixnum))))))
    data))


(defun write% (data stream format binary? maximum-value)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format :ascii))
        (writer (if binary? #'write-byte #'write-number)))
    (destructuring-bind (width height) (array-dimensions data)
      (format stream "P~D~%~D ~D~%~D~%"
              (magic-byte format binary?) width height maximum-value)
      (dotimes (y height)
        (dotimes (x width)
          (let ((pixel (aref data x y)))
            (ecase format
              (:pbm (funcall writer pixel stream))
              (:pgm (funcall writer pixel stream))
              (:ppm (progn (funcall writer (aref pixel 0) stream)
                           (funcall writer (aref pixel 1) stream)
                           (funcall writer (aref pixel 2) stream))))))
        (unless binary? (terpri stream))))))


;;;; API ----------------------------------------------------------------------
(defun read-from-stream (stream)
  (multiple-value-bind (format binary?)
      (file-format (read-magic-byte stream))
    (read% (flexi-streams:make-flexi-stream stream :external-format :ascii)
           format binary?)))

(defun write-to-stream (stream data &key
                        (format :ppm)
                        (encoding :binary)
                        (bit-depth (ecase format (:pbm 1) ((:pgm :ppm) 255))))
  (check-type format (member :ppm :pgm :pbm))
  (check-type encoding (member :binary :ascii))
  (write% data stream format (eql :binary encoding) bit-depth))


(defun read-from-file (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (read-from-stream s)))

(defun write-to-file (path data &key
                      (format :ppm)
                      (encoding :binary)
                      (bit-depth (ecase format (:pbm 1) ((:pgm :ppm) 255))))
  (with-open-file (s path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-to-stream s data :format format :encoding encoding :bit-depth bit-depth)))
