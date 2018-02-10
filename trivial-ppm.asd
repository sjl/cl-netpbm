(asdf:defsystem :trivial-ppm
  :description
  "Common Lisp support for reading/writing the PPM/PGM/PBM image formats."

  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/trivial-ppm/"
  :license "MIT/X11"
  :version "0.0.1"

  :depends-on ()

  :in-order-to ((asdf:test-op (asdf:test-op :trivial-ppm/test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "main")))))

(asdf:defsystem :trivial-ppm/test
  :description
  "Test suite for trivial-ppm."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:trivial-ppm :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "trivial-ppm/test:run-tests"))))

