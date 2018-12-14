(defpackage cl-double-array.benchmark
  (:use :cl
        :cl-double-array))
(in-package :cl-double-array.benchmark)

(defvar vocabulary
  "abcdefghijklmnopqrstuvwxyz")

(defun make-words (n)
  (loop
    with vocab-size = (length vocabulary)
    for x from 1 to n
    collect (coerce (loop
                      with i = (* x (ceiling (expt x 0.5)))
                      while (< 0 i)
                      collect (char vocabulary (mod i vocab-size))
                      do (setf i (floor i vocab-size)))
                    'string)))

(defun bench (n)
  (terpri)
  (format t "*** ~a words ***~%" n)
  (format t "build-double-array:~%")
  (let* ((words (make-words n))
         (double-array (time (build-double-array words))))
    (format t "common-prefix-search * 100,000:~%")
    (time (dotimes (i 100000)
            (common-prefix-search double-array (car (last words)))))))

(bench 100)
(bench 1000)
(bench 10000)
