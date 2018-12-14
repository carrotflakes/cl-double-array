(defpackage cl-double-array-test
  (:use :cl
        :cl-double-array
        :prove))
(in-package :cl-double-array-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-double-array)' in your Lisp.

(plan nil)

(defvar string-list
  '("a" "to" "tea" "ted" "ten" "i" "in" "inn" "int" "inter" "internet"))

(defun f (string-list)
  (let ((double-array (build-double-array string-list)))

    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "a"))
        "a")

    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "abc"))
        "a")

    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "teddy"))
        "ted")

    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "inn"))
        "i,in,inn")

    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "india"))
        "i,in")
  
    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "internet!"))
        "i,in,int,inter,internet")

    (is (format nil "~{~a~^,~}" (common-prefix-search double-array "x"))
        "")

    (is (format nil "~{~a~^,~}" (sort (complete double-array "t") #'string<))
        "tea,ted,ten,to")

    (is (format nil "~{~a~^,~}" (sort (complete double-array "in") #'string<))
        "in,inn,int,inter,internet")

    (is (format nil "~{~a~^,~}" (sort (complete double-array "an") #'string<))
        "")

    (is (format nil "~{~a~^,~}" (sort (complete double-array "x") #'string<))
        "")

    (is (format nil "~{~a~^,~}" (sort (complete double-array "") #'string<))
        (format nil "~{~a~^,~}" (sort (copy-list string-list) #'string<)))))

(f string-list)
(f (cons "" string-list))
(f (cons "foo" string-list))
(f (cons "eat" string-list))

(finalize)
