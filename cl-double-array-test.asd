#|
  This file is a part of cl-double-array project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "cl-double-array-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("cl-double-array"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-double-array"))))
  :description "Test system for cl-double-array"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
