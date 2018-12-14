#|
  This file is a part of cl-double-array project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "cl-double-array-benchmark"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("cl-double-array")
  :components ((:module "bench"
                :components
                ((:file "cl-double-array"))))
  :description "benchmark for cl-double-array")
