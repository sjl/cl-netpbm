# API Reference

The following is a list of all user-facing parts of cl-netpbm.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `NETPBM`

### `READ-FROM-FILE` (function)

    (READ-FROM-FILE PATH)

Read a PPM image file from `path`, returning an array of pixels and more.

  The primary return value will be a 2D array with dimensions `(width height)`.
  Each element of the array will be a single pixel whose type depends on the
  image file format:

  * PBM: `bit`
  * PGM: `(integer 0 maximum-value)`
  * PPM: `(simple-array (integer 0 maximum-value) (3))`

  Two other values are returned:

  * The format of the image that was read (one of `:pbm`, `:pgm`, `:ppm`).
  * The bit depth of the image.

  

### `READ-FROM-STREAM` (function)

    (READ-FROM-STREAM STREAM)

Read a PPM image file from `stream`, returning an array of pixels and more.

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

  

### `WRITE-TO-FILE` (function)

    (WRITE-TO-FILE PATH DATA &KEY (IF-EXISTS NIL IF-EXISTS-GIVEN) (FORMAT :PPM) (ENCODING :BINARY)
                   (MAXIMUM-VALUE (ECASE FORMAT (:PBM 1) ((:PGM :PPM) 255))))

Write a PPM image array `data` to a file at `path`.

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

  

### `WRITE-TO-STREAM` (function)

    (WRITE-TO-STREAM STREAM DATA &KEY (FORMAT :PPM) (ENCODING :BINARY)
                     (MAXIMUM-VALUE (ECASE FORMAT (:PBM 1) ((:PGM :PPM) 255))))

Write a PPM image array `data` to `stream`.

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

  

