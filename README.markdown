cl-netpbm is a pure Common Lisp library for reading and writing the [netpbm
image formats (PPM, PGM, and PBM)](https://en.wikipedia.org/wiki/Netpbm_format).

These image formats are very simple, but not efficient.  If you need extreme
performance you should not use these formats (or this library).

* **License:** MIT/X11
* **Documentation:** <https://docs.stevelosh.com/cl-netpbm/>
* **Mercurial:** <https://hg.sr.ht/~sjl/cl-netpbm/>
* **Git:** <http://github.com/sjl/cl-netpbm/>

cl-netpbm can be installed with Quicklisp: `(ql:quickload :cl-netpbm)`.

The `cl-netpbm` system provides all the functionality of the library.  It has no
dependencies.

The `cl-netpbm/test` system provides the unit tests.  It depends on `1am` and
`external-program`, and also requires [ImageMagick][im] for its fuzz testing.

[im]: https://www.imagemagick.org/
