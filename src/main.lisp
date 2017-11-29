(in-package :trivial-ppm)

;;;; Utils --------------------------------------------------------------------
(defun skip-comment (stream)
  (peek-char #\newline stream nil nil)
  (read-char stream nil nil))

(defun skip-whitespace (stream)
  (when (eql #\# (peek-char t stream nil nil))
    (skip-comment stream)))

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

(defun read-magic-byte (stream)
  (assert (eql (read-char stream) #\P))
  (read-char stream))

(defun read-header (stream)
  (values (read-number stream)
          (read-number stream)
          (read-number stream)))


(defun file-format (magic-byte)
  (ecase magic-byte
    (#\1 :pbm-ascii)
    (#\2 :pgm-ascii)
    (#\3 :ppm-ascii)
    (#\4 :pbm-binary)
    (#\5 :pgm-binary)
    (#\6 :ppm-binary)))

(defun magic-byte (file-format)
  (ecase file-format
    (:pbm-ascii #\1)
    (:pgm-ascii #\2)
    (:ppm-ascii #\3)
    (:pbm-binary #\4)
    (:pgm-binary #\5)
    (:ppm-binary #\6)))


;;;; PPM ----------------------------------------------------------------------
(declaim (inline read-ppm-from-stream))

(defun read-ppm-from-stream (reader stream binary?)
  (multiple-value-bind (width height bit-depth)
      (read-header stream)
    (let ((data (make-array (list height width)
                  :element-type `(simple-array (integer 0 ,bit-depth) (3)))))
      (when binary?
        (read-char stream)) ; chomp last newline before bytes
      (dotimes (y height)
        (dotimes (x width)
          (setf (aref data y x)
                (make-array 3 :initial-contents (list (funcall reader stream)
                                                      (funcall reader stream)
                                                      (funcall reader stream))
                  :element-type 'fixnum))))
      data)))


(defun read-ppm-ascii-from-stream (stream)
  (read-ppm-from-stream #'read-number stream nil))

(defun read-ppm-binary-from-stream (stream)
  (read-ppm-from-stream #'read-byte stream t))


(defun write-pixel-ascii (stream pixel)
  (format stream "~D ~D ~D "
          (aref pixel 0)
          (aref pixel 1)
          (aref pixel 2)))

(defun write-pixel-binary (stream pixel)
  (write-byte (aref pixel 0) stream)
  (write-byte (aref pixel 1) stream)
  (write-byte (aref pixel 2) stream))


(declaim (inline write-ppm-to-stream))

(defun write-ppm-to-stream (file-format data stream maximum-value writer)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format :ascii)))
    (destructuring-bind (height width) (array-dimensions data)
      (format stream "P~D~%~D ~D~%~D~%"
              (magic-byte file-format) width height maximum-value)
      (dotimes (row height)
        (dotimes (col width)
          (funcall writer stream (aref data row col)))))))

(defun write-ppm-ascii-to-stream (data stream maximum-value)
  (write-ppm-to-stream :ppm-ascii data stream maximum-value #'write-pixel-ascii))

(defun write-ppm-binary-to-stream (data stream maximum-value)
  (write-ppm-to-stream :ppm-binary data stream maximum-value #'write-pixel-binary))


;;;; API ----------------------------------------------------------------------
(defun read-from-stream (stream)
  (let ((stream (flexi-streams:make-flexi-stream stream :external-format :ascii)))
    (ecase (file-format (read-magic-byte stream))
      (:ppm-ascii (read-ppm-ascii-from-stream stream))
      (:ppm-binary (read-ppm-binary-from-stream stream)))))

(defun write-to-stream (stream data &key
                        (maximum-value 255)
                        (format :binary))
  (ccase format
    (:ascii (write-ppm-ascii-to-stream data stream maximum-value))
    (:binary (write-ppm-binary-to-stream data stream maximum-value))))
