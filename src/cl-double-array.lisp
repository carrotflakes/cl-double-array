(defpackage cl-double-array
  (:nicknames :clda)
  (:use :cl)
  (:export :*array-element-type-unsigned-byte*
           :double-array
           :build-double-array
           :do-common-prefix-search
           :common-prefix-search
           :do-complete
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


(defparameter *array-element-type-unsigned-byte* 32)

(defun element-type ()
  `(unsigned-byte ,*array-element-type-unsigned-byte*))

(defstruct double-array
  dictionary
  base
  check)

(declaim (inline set-value))
(defun set-value (array i element)
  (unless (< i (array-total-size array))
    (adjust-array array (expt 2 (1+ (floor (log i 2))))))
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

(defun make-vector (element-type)
  (make-array 2
              :element-type element-type
              :initial-element 0
              :adjustable t
              :fill-pointer 0))

(defun vector-last-index (vector)
  (loop
    with n = (array-total-size vector)
    for element = (aref vector (decf n))
    unless (zerop element)
    return n))

(defun build-double-array (string-list)
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (let* ((dictionary (make-dictionary))
         (base (make-vector (element-type)))
         (check (make-vector (element-type)))
         (used (make-vector 'bit))
         (skip (make-vector (element-type)))
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
                            with first-id = (caar tree)
                            with m = (skip 1 first-id)
                            when (and (= (get-value used m) 0) (check m tree))
                            return m
                            do (setf m (skip (1+ m) first-id)))))
                   (set-value base n m)
                   (set-value used m 1)
                   (dolist (node tree)
                     (let ((o (+ m (car node))))
                       (set-value check o m)
                       (set-value skip o (1+ (get-value skip (1+ o))))))
                   (dolist (node tree)
                     (f (+ m (car node)) (cdr node)))))
               (check (m tree)
                 (dolist (node tree)
                   (unless (= (get-value check (+ m (car node))) 0)
                     (return-from check nil)))
                 t)
               (skip (n id &aux (m n))
                 (loop
                   for offset = (get-value skip (+ m id))
                   until (zerop offset)
                   do (incf m offset))
                 (set-value skip (+ n id) (- m n))
                 m))
        ;(print encoded-list)
        (f 1 tree)))
    (setf (fill-pointer base) (1+ (vector-last-index base)))
    ; padding for range-check-less indexing
    (let ((check-size (+ (vector-last-index check)
                         (dictionary-size dictionary)
                         1)))
      (adjust-array check check-size)
      (setf (fill-pointer check) check-size))
    ;(print base)
    ;(print check)(terpri)
    (make-double-array :dictionary dictionary
                       :base (make-array (fill-pointer base)
                                         :element-type (element-type)
                                         :initial-contents base)
                       :check (make-array (fill-pointer check)
                                         :element-type (element-type)
                                         :initial-contents check))))

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

(defun progress (double-array string &key (start 0) end)
  (let* ((dictionary (double-array-dictionary double-array))
         (base (double-array-base double-array))
         (check (double-array-check double-array))
         (dictionary-size (dictionary-size dictionary)))
    (loop
      with n = 1
      until (zerop n)
      for i from start below (or end (length string))
      for id = (or (encode-char dictionary (char string i))
                   (return))
      for m = (+ n id)
      unless (= n (aref check m))
      do (return)
      do (setf n (aref base m))
      finally (return n))))

(defmacro do-complete
    ((double-array string &key (start 0) end node completed) &body body)
  (let* ((double-array-sym (gensym "DOUBLE-ARRAY"))
         (string-sym (gensym "STRING"))
         (start-sym (gensym "START"))
         (end-sym (gensym "END"))
         (body
           (if completed
               `(labels
                    ((f (n)
                       (nconc (when (= n (aref check n))
                                (list (list n)))
                              (loop
                                for i from 1 below dictionary-size
                                for m = (+ n i)
                                when (= n (aref check m))
                                append (loop
                                       with char = (decode-char dictionary i)
                                         for x in (f (aref base m))
                                         collect (cons char x))))))
                  (loop
                    with prefix = (subseq ,string-sym ,start-sym ,end-sym)
                    for result in (f n)
                       ,@(if node
                             `(for ,node = (car (last result)))
                             '())
                    do (setf result (nbutlast result))
                       (let ((,completed (format nil "~a~{~a~}" prefix result)))
                         ,@body)))
               `(labels
                    ((f (n)
                       (when (= n (aref check n))
                         ,@(if node
                               `((let ((,node n))
                                   ,@body))
                               body))
                       (loop
                         for i from 1 below dictionary-size
                         for m = (+ n i)
                         when (= n (aref check m))
                         do (f (aref base m)))))
                  (f n)))))
    `(let* ((,double-array-sym ,double-array)
            (,string-sym ,string)
            (,start-sym ,start)
            (,end-sym ,end)
            (dictionary (double-array-dictionary ,double-array-sym))
            (base (double-array-base ,double-array-sym))
            (check (double-array-check ,double-array-sym))
            (dictionary-size (dictionary-size dictionary))
            (n (progress ,double-array-sym
                         ,string-sym
                         :start ,start-sym
                         :end ,end-sym)))
       (when n
         ,body))))

(defun complete (double-array string &optional (start 0) end)
  (let ((completed-list '()))
    (do-complete (double-array string :start start :end end :completed completed)
      (push completed completed-list))
    completed-list))
