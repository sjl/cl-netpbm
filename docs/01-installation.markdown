Installation
============

cl-netpbm can be installed with Quicklisp:

    (ql:quickload :cl-netpbm)

The `cl-netpbm` system provides all the functionality of the library.  It has no
dependencies.

The `cl-netpbm/test` system provides the unit tests.  It depends on `1am` and
`external-program`, and also requires [ImageMagick][im] for its fuzz testing.

[im]: https://www.imagemagick.org/
