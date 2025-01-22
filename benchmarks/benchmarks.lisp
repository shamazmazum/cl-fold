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
