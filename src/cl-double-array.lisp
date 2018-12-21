(defpackage cl-double-array
  (:nicknames :clda)
  (:use :cl)
  (:export :double-array
           :build-double-array
           :do-common-prefix-search
           :common-prefix-search
           :complete))
(in-package :cl-double-array)

(defstruct dictionary
  (object-id (make-hash-table :test 'eq))
  (id-object (make-array 0 :adjustable t :fill-pointer 0)))

(defun initialize-dictionary (dictionary)
  (setf (gethash :end (dictionary-object-id dictionary)) 0)
  (vector-push-extend :end (dictionary-id-object dictionary)))

(defun dictionary-size (dictionary)
  (fill-pointer (dictionary-id-object dictionary)))

(defun register (dictionary objects)
  (loop
    with object-id = (dictionary-object-id dictionary)
    with id-object = (dictionary-id-object dictionary)
    for object in objects
    unless (gethash object object-id)
    do (setf (gethash object object-id) (fill-pointer id-object))
       (vector-push-extend object id-object)))

(defun encode (dictionary string)
  (loop
    with object-id = (dictionary-object-id dictionary)
    for object in (coerce string 'list)
    collect (gethash object object-id)))

(declaim (inline encode-char))
(defun encode-char (dictionary char)
  (gethash char (dictionary-object-id dictionary)))

(defun decode (dictionary ids)
  (coerce (loop
            with id-object = (dictionary-id-object dictionary)
            for id in ids
            collect (aref id-object id))
          'string))

(declaim (inline decode-char))
(defun decode-char (dictionary id)
  (aref (dictionary-id-object dictionary) id))


(defstruct double-array
  (dictionary (make-dictionary))
  (base (make-array 2
                    :element-type '(unsigned-byte 16)
                    :initial-element 0
                    :adjustable t
                    :fill-pointer 0))
  (check (make-array 2
                     :element-type '(unsigned-byte 16)
                     :initial-element 0
                     :adjustable t
                     :fill-pointer 0)))

(defvar *buffer-increment-step* 128)

(declaim (inline set-value))
(defun set-value (array i element)
  (unless (< i (array-total-size array))
    (adjust-array array (+ (array-total-size array) *buffer-increment-step*)))
  (setf (aref array i) element))

(declaim (inline get-value))
(defun get-value (array i)
  (if (< i (array-total-size array))
      (aref array i)
      0))

(defun lists-to-tree (lists)
  (let ((tree ()))
    (dolist (list lists)
      (cond
        ((null list)
         (push '(0) tree))
        ((eq (caar tree) (car list))
         (push (cdr list) (cdar tree)))
        (t
         (push `(,(car list) ,(cdr list)) tree))))
    (dolist (node tree)
      (setf (cdr node) (lists-to-tree (cdr node))))
    (nreverse tree)))

(defun build-double-array (string-list)
  (let* ((double-array (make-double-array))
         (dictionary (double-array-dictionary double-array))
         (base (double-array-base double-array))
         (check (double-array-check double-array))
         (used (make-array 2
                           :element-type 'bit
                           :initial-element 0
                           :adjustable t
                           :fill-pointer 0))
         (string-list (sort (copy-list string-list) #'string<)))
    (initialize-dictionary dictionary)
    (dolist (string string-list)
      (register dictionary (coerce string 'list)))
    (let* ((encoded-list (loop
                           for string in string-list
                           collect (encode dictionary string)))
           (tree (lists-to-tree encoded-list)))
      (labels ((f (n tree)
                 (unless tree
                   (return-from f))
                 (let ((m (loop
                            for m from 1
                            when (and (= (get-value used m) 0) (check m tree))
                            return m)))
                   (set-value base n m)
                   (set-value used m 1)
                   (dolist (node tree)
                     (set-value check (+ m (car node)) m))
                   (dolist (node tree)
                     (f (+ m (car node)) (cdr node)))))
               (check (m tree)
                 (dolist (node tree)
                   (unless (= (get-value check (+ m (car node))) 0)
                     (return-from check nil)))
                 t))
        ;(print encoded-list)
        (f 1 tree)))
    ; padding for range-check-less indexing
    (adjust-array check
                  (+ (array-total-size check)
                     (dictionary-size dictionary))) ; FIXME: can be shrink?
    (setf (fill-pointer base) (array-total-size base)
          (fill-pointer check) (array-total-size check))
    ;(print base)
    ;(print check)(terpri)
    double-array))

(defmacro do-common-prefix-search
    ((double-array string &key (start 0) end node subseq i) &body body)
  (let ((double-array-sym (gensym "DOUBLE-ARRAY"))
        (string-sym (gensym "STRING"))
        (start-sym (gensym "START"))
        (bindings '()))
    (when node
      (push `(,node n) bindings))
    (when subseq
      (push `(,subseq (subseq ,string-sym ,start-sym (1+ i))) bindings))
    (when i
      (push `(,i (1+ i)) bindings))
    `(let* ((,double-array-sym ,double-array)
            (,string-sym ,string)
            (,start-sym ,start)
            (dictionary (double-array-dictionary ,double-array-sym))
            (base (double-array-base ,double-array-sym))
            (check (double-array-check ,double-array-sym)))
       (loop
         with n fixnum = 1
         until (zerop n)
         for i fixnum from ,start-sym below (or ,end (length ,string-sym))
         for id fixnum = (or (encode-char dictionary (schar ,string-sym i))
                             (return))
         for m = (+ n id)
         while (= n (aref check m))
         do (setf n (aref base m))
         when (= n (aref check n))
         do (let ,bindings ,@body)))))

(defun common-prefix-search (double-array string &optional (start 0) end)
  (let ((result '()))
    (do-common-prefix-search (double-array string :start start :end end :subseq subseq)
      (push subseq result))
    (nreverse result)))

; common-prefix-search-all

(defun complete (double-array string &optional (start 0) end)
  (let* ((dictionary (double-array-dictionary double-array))
         (base (double-array-base double-array))
         (check (double-array-check double-array))
         (n 1)
         (dictionary-size (dictionary-size dictionary)))
    (loop
      until (zerop n)
      for i from start below (or end (length string))
      for id = (or (encode-char dictionary (char string i))
                   (return-from complete '()))
      for m = (+ n id)
      unless (= n (aref check m))
      do (return-from complete '())
      do (setf n (aref base m)))
    (labels ((f (n)
               (nconc (when (= n (aref check n))
                        (list (list)))
                      (loop
                        for i from 1 below dictionary-size
                        for m = (+ n i)
                        when (= n (aref check m))
                        append (loop
                                 with char = (decode-char dictionary i)
                                 for x in (f (aref base m))
                                 collect (cons char x))))))
      (loop
        with prefix = (subseq string start end)
        for chars in (f n)
        collect (format nil "~a~{~a~}" prefix chars)))))
