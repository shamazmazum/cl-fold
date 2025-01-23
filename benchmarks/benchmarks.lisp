(in-package :cl-fold/benchmarks)

(declaim (type list *test-data-list*))
(defparameter *test-data-list*
  (loop repeat 200 collect (random 255)))

(declaim (type simple-vector *test-data-vector*))
(defparameter *test-data-vector*
  (make-array 200 :initial-contents
              (loop repeat 200 collect (random 255))))

(defmacro with-timing (n form)
  (let ((acc (gensym)))
    `(let ((,acc 0))
       (tb:with-timing (,n)
         (incf ,acc
               (loop repeat 400000 sum ,form)))
       (format t "~d~%" ,acc))))

(defun foldl ()
  (format t "Folding lists with FOLDL~%")
  (with-timing 100
    (cl-fold:foldl #'+ 0 *test-data-list*))

  (format t "Folding lists with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-list* :initial-value 0))

  (format t "Folding vectors with FOLDL~%")
  (with-timing 100
    (cl-fold:foldl #'+ 0 *test-data-vector*))

  (format t "Folding vectors with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-vector* :initial-value 0)))

(defun foldr ()
  (format t "Folding lists with FOLDR~%")
  (with-timing 100
    (cl-fold:foldr #'+ 0 *test-data-list*))

  (format t "Folding lists with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-list* :initial-value 0 :from-end t))

  (format t "Folding vectors with FOLDR~%")
  (with-timing 100
    (cl-fold:foldr #'+ 0 *test-data-vector*))

  (format t "Folding vectors with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-vector* :initial-value 0 :from-end t)))

(defun foldr1 ()
  (format t "Folding lists with FOLDR1~%")
  (with-timing 100
    (cl-fold:foldr1 #'+ *test-data-list*))

  (format t "Folding lists with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-list* :from-end t))

  (format t "Folding vectors with FOLDR1~%")
  (with-timing 100
    (cl-fold:foldr1 #'+ *test-data-vector*))

  (format t "Folding vectors with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-vector* :from-end t)))


(serapeum:defconstructor foo
  (x (unsigned-byte 8)))

(declaim (type list *test-data-list-2*))
(defparameter *test-data-list-2*
  (mapcar #'foo *test-data-list*))

(declaim (type simple-vector *test-data-vector-2*))
(defparameter *test-data-vector-2*
  (map 'vector #'foo *test-data-vector*))

(defun mapfoldl ()
  (format t "Folding lists with MAPFOLDL~%")
  (with-timing 100
    (cl-fold:mapfoldl #'+ #'foo-x 0 *test-data-list-2*))

  (format t "Folding lists with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-list-2* :initial-value 0 :key #'foo-x))

  (format t "Folding vectors with MAPFOLDL~%")
  (with-timing 100
    (cl-fold:mapfoldl #'+ #'foo-x 0 *test-data-vector-2*))

  (format t "Folding vectors with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-vector-2* :initial-value 0 :key #'foo-x)))

(defun mapfoldr ()
  (format t "Folding lists with MAPFOLDR~%")
  (with-timing 100
    (cl-fold:mapfoldr #'+ #'foo-x 0 *test-data-list-2*))

  (format t "Folding lists with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-list-2* :initial-value 0 :from-end t :key #'foo-x))

  (format t "Folding vectors with MAPFOLDR~%")
  (with-timing 100
    (cl-fold:mapfoldr #'+ #'foo-x 0 *test-data-vector-2*))

  (format t "Folding vectors with REDUCE~%")
  (with-timing 100
    (reduce #'+ *test-data-vector-2* :initial-value 0 :from-end t :key #'foo-x)))
