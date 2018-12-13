#|
  This file is a part of cl-double-array project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "cl-double-array"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-double-array"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-double-array-test"))))
