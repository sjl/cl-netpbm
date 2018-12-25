(in-package :trivial-ppm)

;;;; Peekable Streams ---------------------------------------------------------
(defstruct (peekable-stream (:conc-name nil)
                            (:constructor make-peekable-stream (s)))
  (p nil :type (or null (unsigned-byte 8)))
  (s (error "Required") :type stream))

(defun actually-read-byte (stream &optional eof-error-p)
  (cl:read-byte (s stream) eof-error-p nil))

(defun read-byte (stream &optional (eof-error-p t))
  (if (p stream)
    (prog1 (p stream)
      (setf (p stream) nil))
    (actually-read-byte stream eof-error-p)))

(defun peek-byte (stream)
  (when (null (p stream))
    (setf (p stream) (actually-read-byte stream)))
  (p stream))

(defun unread-byte (stream byte)
  (assert (null (p stream)))
  (setf (p stream) byte)
  (values))


;;;; Utils --------------------------------------------------------------------
;;; TODO: We're explicit about ASCII values here, but other places in the code
;;; rely on char-code and friends returning ASCII.  Eventually we should
;;; probably fix that.

(defconstant +space+ 32)
(defconstant +tab+ 9)
(defconstant +line-feed+ 10)
(defconstant +vertical-tab+ 11)
(defconstant +form-feed+ 12)
(defconstant +carriage-return+ 13)
(defconstant +comment-char+ 35)


(defun white-space-p (byte)
  (if (member byte (list +space+ +form-feed+
                         +tab+ +vertical-tab+
                         +line-feed+ +carriage-return+))
    t
    nil))

(defun line-terminator-p (byte)
  (if (member byte (list +line-feed+ +carriage-return+))
    t
    nil))


(defun skip-comment-body (stream)
  (loop :until (line-terminator-p (read-byte stream))))

(defun skip-whitespace (stream)
  (loop :for byte = (read-byte stream nil)
        :while (white-space-p byte)
        :finally (unread-byte stream byte)))


(defun error-junk (section byte)
  (error "Junk byte in ~A data: ~D (~S)" section byte (code-char byte)))

(defun byte-to-digit (byte)
  (when (and byte (<= (char-code #\0) byte (char-code #\9)))
    (- byte (char-code #\0))))


(defun read-raster-number (stream)
  "Read the next ASCII-encoded number from `stream` (does not allow comments)."
  (skip-whitespace stream)
  (loop :with i = nil
        :for byte = (read-byte stream nil)
        :for digit = (byte-to-digit byte)
        :unless (or (null byte) digit (white-space-p byte))
        :do (error-junk "raster" byte)
        :while digit
        :do (setf i (+ (* (or i 0) 10) digit))
        :finally (return i)))

(defun read-header-number (stream)
  "Read the next ASCII-encoded number from `stream` (allows comments)."
  (skip-whitespace stream)
  (loop :with i = nil
        :for byte = (read-byte stream nil)
        :for digit = (byte-to-digit byte)
        :while byte
        :while (cond ((= byte +comment-char+) (skip-comment-body stream) t)
                     (digit (setf i (+ (* (or i 0) 10) digit)) t)
                     ((white-space-p byte) nil)
                     (t (error-junk "header" byte)))
        :finally (return i)))

(defun read-magic-byte (stream)
  "Read the initial `P#` from `stream`, returning the magic `#` character."
  (assert (eql (cl:read-byte stream) (char-code #\P)) (stream)
          "Stream ~S does not appear to be in P*M file."
          stream)
  (code-char (cl:read-byte stream)))


(defun write-string-as-bytes (string stream)
  (loop :for ch :across string
        :do (write-byte (char-code ch) stream)))

(defun format-to-stream (stream &rest format-args)
  (write-string-as-bytes (apply #'format nil format-args) stream))

(defmacro check-number (place maximum-value)
  `(assert (typep ,place `(integer 0 ,maximum-value)) (,place)
     "Cannot write sample value ~D to P*M with maximum value of ~D"
     ,place
     ,maximum-value))

(defun write-number-ascii (maximum-value value stream)
  "Write `value` to stream as an ASCII-encoded number, with sanity check."
  (check-number value maximum-value)
  (format-to-stream stream "~D " value))

(defun write-number-binary (maximum-value value stream)
  "Write `value` to stream as a binary value, with sanity check."
  (check-number value maximum-value)
  (write-byte value stream))


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
(defun bits (byte)
  (loop :for i :from 7 :downto 0
        :collect (ldb (byte 1 i) byte)))

(defun read% (stream format binary?)
  (let ((buffer nil))
    (flet ((read-bit-binary (stream)
             (when (null buffer)
               (setf buffer (bits (read-byte stream))))
             (pop buffer))
           (flush-buffer ()
             (setf buffer nil)))
      (let* ((width (read-header-number stream))
             (height (read-header-number stream))
             (bit-depth (if (eql :pbm format) 1 (read-header-number stream)))
             (data (make-array (list width height)
                     :element-type (pixel-type format bit-depth)))
             (reader (if binary?
                       (if (eql format :pbm)
                         #'read-bit-binary
                         #'read-byte)
                       #'read-raster-number)))
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
                            :element-type 'fixnum)))))
          (flush-buffer))
        (values data format bit-depth)))))

(defun write% (data stream format binary? maximum-value)
  (let ((buffer 0)
        (buffer-length 0))
    (labels ((write-bit-binary (bit stream)
               (declare (ignore stream))
               (setf buffer (+ (ash buffer 1) bit))
               (incf buffer-length)
               (when (= buffer-length 8)
                 (flush-buffer)))
             (flush-buffer ()
               (when (plusp buffer-length)
                 (write-byte (ash buffer (- 8 buffer-length)) stream)
                 (setf buffer 0 buffer-length 0))))
      (let ((writer (if binary?
                      (if (eql format :pbm)
                        #'write-bit-binary
                        (curry #'write-number-binary maximum-value))
                      (curry #'write-number-ascii maximum-value))))
        (destructuring-bind (width height) (array-dimensions data)
          (format-to-stream stream "P~D~%~D ~D~%"
                            (magic-byte format binary?) width height)
          (unless (eql format :pbm)
            (format-to-stream stream "~D~%" maximum-value))
          (dotimes (y height)
            (dotimes (x width)
              (let ((pixel (aref data x y)))
                (ecase format
                  (:pbm (funcall writer (- 1 pixel) stream))
                  (:pgm (funcall writer pixel stream))
                  (:ppm (progn (funcall writer (aref pixel 0) stream)
                               (funcall writer (aref pixel 1) stream)
                               (funcall writer (aref pixel 2) stream))))))
            (flush-buffer)
            (unless binary? (write-byte +line-feed+ stream))))))))


;;;; API ----------------------------------------------------------------------
;;; TODO: The stream type checking here is kind of a mess.  Basically what we
;;; care about is the following:
;;;
;;;   * For input streams we need to be able to call (read-byte …) and get
;;;     back numbers in the range 0-255.
;;;   * For output streams we need to be able to call (write-byte …) with
;;;     numbers in the range 0-255.
;;;
;;; As far as I can tell, there's no way to verify this in advance.  Or, indeed,
;;; *at all*, because the spec for `write-byte` says:
;;;
;;; > Might signal an error of type type-error if byte is not an integer of the
;;; > stream element type of stream.
;;;
;;; "Might"?!

(defun read-from-stream (stream)
  "Read a PPM image file from `stream`, returning an array of pixels and more.

  `stream` must be a binary input stream, specifically of `(unsigned-byte 8)`s
  unless you *really* know what you're doing.

  The primary return value will be a 2D array with dimensions `(width height)`.
  Each element of the array will be a single pixel whose type depends on the
  image file format:

  * PBM: `bit`
  * PGM: `(integer 0 maximum-value)`
  * PPM: `(simple-array (integer 0 maximum-value) (3))`

  Two other values are returned:

  * The format of the image that was read (one of `:pbm`, `:pgm`, `:ppm`).
  * The bit depth of the image.

  "
  (check-type stream stream)
  (assert (input-stream-p stream) (stream)
    "Stream ~S is not an input stream." stream)
  (multiple-value-bind (format binary?)
      (file-format (read-magic-byte stream))
    (read% (make-peekable-stream stream) format binary?)))

(defun write-to-stream (stream data &key
                        (format :ppm)
                        (encoding :binary)
                        (maximum-value (ecase format (:pbm 1) ((:pgm :ppm) 255))))
  "Write a PPM image array `data` to `stream`.

  Nothing is returned.

  `stream` must be a binary output stream, specifically of `(unsigned-byte 8)`s
  unless you *really* know what you're doing.

  `format` must be one of `:pbm`, `:pgm`, `:ppm`.

  `encoding` must be one of `:binary`, `:ascii`.

  `maximum-value` must be the desired bit depth of the image (the maximum value
  any particular pixel can have).  For PBM images it must be `1`.

  For PBM and PGM images, `data` must be a two dimensional array of integers
  between `0` and `maximum-value` inclusive.

  For PPM images, `data` must be a two dimensional array of pixels, each of
  which must be a 3 element vector of integers between `0` and `maximum-value`
  inclusive.

  "
  (check-type stream stream)
  (assert (output-stream-p stream) (stream)
    "Stream ~S is not an output stream." stream)
  (check-type format (member :ppm :pgm :pbm))
  (check-type encoding (member :binary :ascii))
  (if (eql format :pbm)
    (check-type maximum-value (eql 1))
    (check-type maximum-value (integer 1 *)))
  (write% data stream format (eql :binary encoding) maximum-value)
  (values))


(defun read-from-file (path)
  "Read a PPM image file from `path`, returning an array of pixels and more.

  The primary return value will be a 2D array with dimensions `(width height)`.
  Each element of the array will be a single pixel whose type depends on the
  image file format:

  * PBM: `bit`
  * PGM: `(integer 0 maximum-value)`
  * PPM: `(simple-array (integer 0 maximum-value) (3))`

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
                      (maximum-value (ecase format (:pbm 1) ((:pgm :ppm) 255))))
  "Write a PPM image array `data` to a file at `path`.

  Nothing is returned.

  `format` must be one of `:pbm`, `:pgm`, `:ppm`.

  `encoding` must be one of `:binary`, `:ascii`.

  `maximum-value` must be the desired bit depth of the image (the maximum value
  any particular pixel can have).  For PBM images it must be `1`.

  For PBM and PGM images, `data` must be a two dimensional array of integers
  between `0` and `maximum-value` inclusive.

  For PPM images, `data` must be a two dimensional array of pixels, each of
  which must be a 3 element vector of integers between `0` and `maximum-value`
  inclusive.

  "
  (check-type format (member :ppm :pgm :pbm))
  (check-type encoding (member :binary :ascii))
  (if (eql format :pbm)
    (check-type maximum-value (eql 1))
    (check-type maximum-value (integer 1 *)))
  (flet ((write-it (stream)
           (write-to-stream stream data
                            :format format
                            :encoding encoding
                            :maximum-value maximum-value)))
    (if if-exists-given
      (with-open-file (s path :direction :output :if-exists if-exists :element-type '(unsigned-byte 8))
        (write-it s))
      (with-open-file (s path :direction :output :element-type '(unsigned-byte 8))
        (write-it s))))
  (values))
