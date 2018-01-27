(in-package :trivial-ppm)

;;;; Utils --------------------------------------------------------------------
(defun skip-comment (stream)
  (peek-char #\newline stream nil nil)
  (read-char stream nil nil))

(defun skip-whitespace (stream)
  (loop :while (eql #\# (peek-char t stream nil nil))
        :do (skip-comment stream)))


(defun read-number (stream)
  "Read the next ASCII-encoded number from `stream`."
  (skip-whitespace stream)
  (loop :with i = 0
        :for ch = (peek-char nil stream nil nil)
        :while ch
        :for digit = (digit-char-p ch)
        :while digit
        :do (read-char stream)
        :do (setf i (+ (* i 10) digit))
        :finally (return i)))

(defun write-number (value stream)
  "Write `value` to stream as an ASCII-encoded number."
  (format stream "~D " value))


(defun read-magic-byte (stream)
  "Read the initial `P#` from `stream`, returning the magic `#` character."
  (assert (eql (read-byte stream) (char-code #\P)))
  (code-char (read-byte stream)))


(defun file-format (magic-byte)
  "Return `(values format binary?)` for the given magic byte character."
  (ecase magic-byte
    (#\1 (values :pbm nil))
    (#\2 (values :pgm nil))
    (#\3 (values :ppm nil))
    (#\4 (values :pbm t))
    (#\5 (values :pgm t))
    (#\6 (values :ppm t))))

(defun magic-byte (file-format binary?)
  "Return the magic byte character to use for the given format/encoding combination."
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
  "Return the type specifier for a pixel of an image with the given `format` and `bit-depth`."
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
                (:pbm (- 1 (funcall reader stream)))
                (:pgm (funcall reader stream))
                (:ppm (make-array 3
                        :initial-contents (list (funcall reader stream)
                                                (funcall reader stream)
                                                (funcall reader stream))
                        :element-type 'fixnum))))))
    (values data format bit-depth)))


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
              (:pbm (funcall writer (- 1 pixel) stream))
              (:pgm (funcall writer pixel stream))
              (:ppm (progn (funcall writer (aref pixel 0) stream)
                           (funcall writer (aref pixel 1) stream)
                           (funcall writer (aref pixel 2) stream))))))
        (unless binary? (terpri stream))))))


;;;; API ----------------------------------------------------------------------
(defun read-from-stream (stream)
  "Read a PPM image file from `stream`, returning an array of pixels and more.

  `stream` must be a binary input stream.

  The primary return value will be a 2D array with dimensions `(width height)`.
  Each element of the array will be a single pixel whose type depends on the
  image file format:

  * PBM: `bit`
  * PGM: `(integer 0 bit-depth)`
  * PPM: `(simple-array (integer 0 bit-depth) (3))`

  Two other values are returned:

  * The format of the image that was read (one of `:pbm`, `:pgm`, `:ppm`).
  * The bit depth of the image.

  "
  (multiple-value-bind (format binary?)
      (file-format (read-magic-byte stream))
    (read% (flexi-streams:make-flexi-stream stream :external-format :ascii)
           format binary?)))

(defun write-to-stream (stream data &key
                        (format :ppm)
                        (encoding :binary)
                        (bit-depth (ecase format (:pbm 1) ((:pgm :ppm) 255))))
  "Write a PPM image array `data` to `stream`.

  Nothing is returned.

  `format` must be one of `:pbm`, `:pgm`, `:ppm`.

  `encoding` must be one of `:binary`, `:ascii`.

  `bit-depth` must be the desired bit depth of the image (the maximum value any
  particular pixel can have).  For PBM images it must be `1`.

  For PBM and PGM images, `data` must be a two dimensional array of integers
  between `0` and `bit-depth` inclusive.

  For PPM images, `data` must be a two dimensional array of pixels, each of
  which must be a 3 element vector of integers between `0` and `bit-depth`
  inclusive.

  "
  (check-type format (member :ppm :pgm :pbm))
  (check-type encoding (member :binary :ascii))
  (if (eql format :pbm)
    (check-type bit-depth (eql 1))
    (check-type bit-depth (integer 1 *)))
  (write% data stream format (eql :binary encoding) bit-depth)
  (values))


(defun read-from-file (path)
  "Read a PPM image file from `path`, returning an array of pixels and more.

  The primary return value will be a 2D array with dimensions `(width height)`.
  Each element of the array will be a single pixel whose type depends on the
  image file format:

  * PBM: `bit`
  * PGM: `(integer 0 bit-depth)`
  * PPM: `(simple-array (integer 0 bit-depth) (3))`

  Two other values are returned:

  * The format of the image that was read (one of `:pbm`, `:pgm`, `:ppm`).
  * The bit depth of the image.

  "
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (read-from-stream s)))

(defun write-to-file (path data &key
                      (if-exists nil if-exists-given)
                      (format :ppm)
                      (encoding :binary)
                      (bit-depth (ecase format (:pbm 1) ((:pgm :ppm) 255))))
  "Write a PPM image array `data` to a file at `path`.

  Nothing is returned.

  `format` must be one of `:pbm`, `:pgm`, `:ppm`.

  `encoding` must be one of `:binary`, `:ascii`.

  `bit-depth` must be the desired bit depth of the image (the maximum value any
  particular pixel can have).  For PBM images it must be `1`.

  For PBM and PGM images, `data` must be a two dimensional array of integers
  between `0` and `bit-depth` inclusive.

  For PPM images, `data` must be a two dimensional array of pixels, each of
  which must be a 3 element vector of integers between `0` and `bit-depth`
  inclusive.

  "
  (flet ((write-it (stream)
           (write-to-stream stream data
                            :format format
                            :encoding encoding
                            :bit-depth bit-depth)))
    (if if-exists-given
      (with-open-file (s path :direction :output :if-exists if-exists :element-type '(unsigned-byte 8))
        (write-it s))
      (with-open-file (s path :direction :output :element-type '(unsigned-byte 8))
        (write-it s))))
  (values))
