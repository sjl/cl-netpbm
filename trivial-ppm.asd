(asdf:defsystem :trivial-ppm
  :description
  "Common Lisp support for reading/writing the PPM/PGM/PBM image formats."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on ()

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "main")))))

