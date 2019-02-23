Installation
============

cl-netpbm is compatible with Quicklisp, but not *in* Quicklisp (yet?).  You
can clone the repository into your [Quicklisp local-projects][local] directory
for now.

The `cl-netpbm` system provides all the functionality of the library.  It has no
dependencies.

The `cl-netpbm/test` system provides the unit tests.  It depends on `1am` and
`external-program`, and also requires [ImageMagick][im] for its fuzz testing.

[local]: https://www.quicklisp.org/beta/faq.html#local-project
[im]: https://www.imagemagick.org/
