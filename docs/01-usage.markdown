Usage
=====

The [netpbm image formats (PPM, PGM, and PBM)][netpbm] are a family of very
simple image formats.  You can convert to/from these formats with third-party
tools like [ImageMagick][im].

Instead of trying to link `libjpeg` into your Lisp program with CFFI, you can
use this library to read/write images in the simple netpbm format and then use
a third-party tool to convert to whatever other format(s) you need.

cl-netpbm provides functions for both reading and writing images, as well as
a little bit of sugar for working with OpenGL textures.

[netpbm]: https://en.wikipedia.org/wiki/Netpbm_format
[im]: https://www.imagemagick.org/

[TOC]

Reading Images
--------------

The `netpbm:read-from-stream` function can be used to read a netpbm file from
stream.  The stream *must* be a binary input stream with `element-type` of
`(unsigned-byte 8)`.

Three values are returned: a 2D array of pixels, the format of the image
(`:pbm`, `:pgm`, or `:ppm`), and the bit depth of the image:

    (with-open-file (f "foo.ppm" :element-type '(unsigned-byte 8))
      (netpbm:read-from-stream f))
    ; =>
    #2A((#(255 0 0) #(0 255 0) #(0 0 255))
        (#(255 0 0) #(0 255 0) #(0 0 255))
        (#(255 0 0) #(0 255 0) #(0 0 255))
        (#(255 0 0) #(0 255 0) #(0 0 255)))
    :PPM
    255

A `netpbm:read-from-file` function is also provided to save you some
boilerplate:

    (netpbm:read-from-file "foo.ppm")

See the [API reference](../reference) for these functions for more information.

Image Arrays
------------

When an image is read a 2D array of pixels is returned.

The first array dimension is the columns of the image and the second array
dimension is the rows.  This means to access pixel `(x, y)` of the image you use
`(aref image x y)`.  The `y` dimension starts at the top of the image and grows
downwards, so:

* `(aref image 0 0)` returns the top-left pixel.
* `(aref image width height)` returns the bottom-right pixel.

The `element-type` of the array (i.e. the type of the pixels) depends on the
format of the image that was read:

* Pixels of PBM images are of type `bit`.
* Pixels of PGM images are of type `(integer 0 ,bit-depth)`.
* Pixels of PPM images are of type `(simple-array (integer 0 ,bit-depth) (3))`.

Lower values represent darker colors, e.g. for PBM images `0` is black and `1`
is white, for PGM `0` is black, `1` is very dark gray, etc.

(Note that the actual PBM image format on disk is backwards from all the other
netpbm formats — in PBM a `1` bit represents black.  cl-netpbm flips the bits
when reading/writing PBM files for consistency on the Lisp side of things.)

Writing Images
--------------

An image array can be written to a stream with `netpbm:write-to-stream`.  The
stream *must* be a binary output stream with `element-type` of `(unsigned-byte
8)`.  The input image array must have the appropriate contents for the desired
output format (e.g. integers for `:pbm` and `:pgm`, 3-element vectors for
`:pgm`):

    (with-open-file (f "foo.pbm"
                      :direction :output
                      :element-type '(unsigned-byte 8))
      (netpbm:write-to-stream
        f #2A((0 0 0 0 0 0 0 0 0 0)
              (0 1 1 1 1 1 1 1 1 0)
              (0 1 0 0 1 1 0 0 0 0)
              (0 1 0 0 1 0 1 0 0 0)
              (0 0 1 1 0 0 0 1 1 0)
              (0 0 0 0 0 0 0 0 0 0))
        :format :pbm
        :encoding :binary))
    ; => Write an "R" character into "foo.pbm"

`netpbm:write-to-file` is provided for convenience:

    (netpbm:write-to-file "foo.pbm" image :format :pbm)

See the [API reference](../reference) for these functions for more information.

Example: Inverting an Image
---------------------------

For a concrete example, let's invert an image.

First we'll get a kitten photo to work with and convert it to PPM with
ImageMagick:

    wget 'https://upload.wikimedia.org/wikipedia/commons/7/75/Cute_grey_kitten.jpg' -O kitten.jpg
    convert -resize x600 kitten.jpg kitten.ppm

The initial kitten ([source](https://en.m.wikipedia.org/wiki/File:Cute_grey_kitten.jpg)):

![kitten photo](../assets/kitten.jpg)

Now we can write our Lisp code:

    (defun invert-value (value)
      (- 255 value))

    (defun invert-pixel (pixel)
      (map-into pixel #'invert-value pixel))

    (defun invert-image (image)
      (destructuring-bind (width height) (array-dimensions image)
        (dotimes (y height)
          (dotimes (x width)
            (invert-pixel (aref image x y))))))

    (let ((image (netpbm:read-from-file "kitten.ppm")))
      (invert-image image)
      (netpbm:write-to-file "kitten-inverted.ppm" image))

And convert it back into JPG:

    convert kitten-inverted.ppm kitten-inverted.jpg

And now we have an inverted kitten:

![kitten photo](../assets/kitten-inverted.jpg)

OpenGL Textures
---------------

cl-netpbm's image array layout (column major, 3-element vectors for RGB pixels,
y-axis from top to bottom) is meant to be simple for humans to code against.
If you're working with OpenGL and were hoping to use cl-netpbm to easily load
textures (instead of fiddling around with FFI'ing out to something like
[STB](https://github.com/nothings/stb)) this won't work, because OpenGL expects
a different format (a flat array of `single-float`s, y-axis from bottom to top).

For your convenience, cl-netpbm provides two additional functions that will
return an array in the format OpenGL expects: `netpbm:read-texture-from-stream`
and `netpbm:read-texture-from-file`.

Remember, though, that the netpbm formats are designed for simplicity, not
efficiency.  If you're just going through [an OpenGL
tutorial](https://learnopengl.com/) and want to load a texture without screwing
around with CFFI, cl-netpbm can help you out.  But if you're creating an actual
game where performance matters, you'll likely want to replace it with something
much more efficient.

