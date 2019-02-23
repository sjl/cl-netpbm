(asdf:defsystem :cl-netpbm
  :description
  "Common Lisp support for reading/writing the netpbm image formats (PPM, PGM, and PBM)."

  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/cl-netpbm/"
  :license "MIT/X11"
  :version "0.0.1"

  :depends-on ()

  :in-order-to ((asdf:test-op (asdf:test-op :cl-netpbm/test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "main")))))

(asdf:defsystem :cl-netpbm/test
  :description "Test suite for cl-netpbm."
  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT/X11"

  :depends-on (:cl-netpbm :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "netpbm/test:run-tests"))))

